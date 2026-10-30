suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

ano <- as.integer(Sys.getenv("ANO", "2026"))
raw_dir <- function(...) here::here("data", "raw", ano, ...)
proc_dir <- function(...) here::here("data", "processed", ano, ...)
out_table <- function(name) {
  d <- here::here("output", "tables", ano); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.path(d, name)
}

tse_base <- "https://cdn.tse.jus.br/estatistica/sead/odsele"
ckan_api <- "https://dadosabertos.tse.jus.br/api/3/action"

# NR_VOTAVEL codes for non-nominal votes (LEIAME votacao_secao 2026).
nr_branco <- 95L
nr_nulo <- 96L
nr_anulado <- 97L

# secao_id (int64) = CD_MUNICIPIO * 1e7 + NR_ZONA * 1e4 + NR_SECAO; CD_MUNICIPIO is nationally unique.
secao_pack <- function(cd_municipio, nr_zona, nr_secao) {
  stopifnot(all(nr_zona >= 0 & nr_zona < 1000), all(nr_secao >= 0 & nr_secao < 10000),
            all(cd_municipio >= 0 & cd_municipio < 1e5))
  bit64::as.integer64(cd_municipio) * 10000000L + bit64::as.integer64(nr_zona) * 10000L +
    bit64::as.integer64(nr_secao)
}

zip_members <- function(zip) system2("unzip", c("-Z1", shQuote(zip)), stdout = TRUE)

zip_header <- function(zip, member) {
  hdr <- system(sprintf("unzip -p %s %s | head -1", shQuote(zip), shQuote(member)), intern = TRUE)
  gsub('["\r]', "", strsplit(iconv(hdr, "latin1", "UTF-8"), ";", fixed = TRUE)[[1]])
}

# Streams one csv member of a TSE zip; with `cargo`, keeps rows with CD_CARGO == cargo (awk on the header position).
read_tse_zip <- function(zip, member, cols, cargo = 1L) {
  nm <- zip_header(zip, member)
  miss <- setdiff(c(cols, if (!is.null(cargo)) "CD_CARGO"), nm)
  if (length(miss)) stop("columns not in ", member, ": ", paste(miss, collapse = ", "))
  cmd <- sprintf("unzip -p %s %s", shQuote(zip), shQuote(member))
  if (!is.null(cargo)) {
    cmd <- sprintf("%s | LC_ALL=C awk -F';' 'NR==1{print;next}{c=$%d;gsub(/\"/,\"\",c)}c==%d{print}'",
                   cmd, match("CD_CARGO", nm), cargo)
  }
  fread(cmd = cmd, encoding = "Latin-1", sep = ";", select = cols, integer64 = "character")
}

assert <- function(cond, msg) if (!isTRUE(all(cond))) stop("assertion failed: ", msg, call. = FALSE)

write_proc <- function(x, name) {
  dir.create(proc_dir(), recursive = TRUE, showWarnings = FALSE)
  write_parquet(x, proc_dir(paste0(name, ".parquet")))
}
read_proc <- function(name) as.data.table(read_parquet(proc_dir(paste0(name, ".parquet"))))

# Presidential runoff finalists by year (label = candidate column c<NR_VOTAVEL>), checked against the round-2 data.
# "bolsonaro" is Jair in 2022 and Flávio in 2026.
finalistas <- list(`2014` = c(dilma = "c13", aecio = "c45"),
                   `2022` = c(lula = "c13", bolsonaro = "c22"),
                   `2026` = c(lula = "c13", bolsonaro = "c22"))
fin_ano <- finalistas[[as.character(ano)]]
# Finalist from the government side (incumbent president or endorsed successor).
governo <- c(`2014` = "dilma", `2022` = "bolsonaro", `2026` = "lula")

# Round-1 row groupings; candidates not listed go to "outros". Only spec A (all non-finalists merged): the
# also-rans are too small to be separately identified.
specs <- list(A = as.list(fin_ano))

# X: seções x rows (round 1), Y: seções x columns (round 2); both sum to aptos in every seção.
# `finalists` names the round-2 candidate columns (label = column in t2).
make_spec <- function(t1, t2, spec, finalists = fin_ano) {
  stopifnot(identical(t1$secao_id, t2$secao_id))
  cands <- grep("^c[0-9]+$", names(t1), value = TRUE)
  g <- c(spec, list(outros = setdiff(cands, unlist(spec))))
  X <- sapply(g, function(cs) rowSums(t1[, ..cs, drop = FALSE]))
  X <- cbind(X, invalido = t1$branco + t1$nulo, abstencao = t1$abstencao)
  Y <- sapply(finalists, function(cc) t2[[cc]])
  Y <- cbind(Y, invalido = t2$branco + t2$nulo, abstencao = t2$abstencao)
  assert(rowSums(X) == t1$aptos & rowSums(Y) == t1$aptos, "spec rows and columns sum to aptos")
  list(X = X, Y = Y, uf = t1$uf, cd_municipio = t1$cd_municipio, secao_id = t1$secao_id)
}

lp_methods <- c("lphom", "tslphom", "nslphom")

# Seção-level bound counts for every cell, summed by group. Returns row, col, lo/hi counts and row totals.
bounds_by <- function(s, by) {
  N <- rowSums(s$X)
  cells <- CJ(row = colnames(s$X), col = colnames(s$Y), sorted = FALSE)
  rbindlist(lapply(seq_len(nrow(cells)), function(k) {
    x <- s$X[, cells$row[k]]; y <- s$Y[, cells$col[k]]
    data.table(g = by, x = x, lo = pmax(0, y - (N - x)), hi = pmin(x, y))[
      , .(row = cells$row[k], col = cells$col[k], row_total = sum(x), lo = sum(lo), hi = sum(hi)), by = g]
  }))[, `:=`(p_lo = lo / row_total, p_hi = hi / row_total)]
}

# One zona: J x K vote counts per method; tslphom / nslphom unit matrices summed by município.
fit_zona <- function(X, Y, mun) {
  out <- list(); muni <- list()
  for (m in lp_methods) {
    r <- tryCatch(get(m, asNamespace("lphom"))(X, Y, new_and_exit_voters = "simultaneous", verbose = FALSE),
                  error = function(e) e)
    if (inherits(r, "error")) { out[[m]] <- conditionMessage(r); next }
    V <- r$VTM.votes
    assert(identical(dim(V), c(ncol(X), ncol(Y))), "VTM.votes is J x K")
    dimnames(V) <- list(colnames(X), colnames(Y))
    out[[m]] <- V
    if (!is.null(r$VTM.votes.units)) {
      U <- r$VTM.votes.units
      muni[[m]] <- lapply(split(seq_along(mun), mun), function(i) {
        M <- apply(U[, , i, drop = FALSE], 1:2, sum); dimnames(M) <- dimnames(V); M })
    }
  }
  list(zona = out, muni = muni)
}

# Net change of each finalist: votes received from category k minus votes lost to k; sums to the observed change.
# `e` has row, col, votos (one geography/method).
swing_tab <- function(e, fin) {
  rbindlist(lapply(fin, function(f) {
    d <- data.table(finalista = f, contraparte = setdiff(union(e$row, e$col), f))
    d[, ganho := vapply(contraparte, function(k) e[row == k & col == f, sum(votos)], numeric(1))]
    d[, perda := vapply(contraparte, function(k) e[row == f & col == k, sum(votos)], numeric(1))]
    d[, liquido := ganho - perda]
    obs <- e[col == f, sum(votos)] - e[row == f, sum(votos)]
    assert(abs(sum(d$liquido) - obs) < 1, sprintf("swing components sum to observed change for %s", f))
    d[, delta_obs := obs][]
  }))
}

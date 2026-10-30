# Governor runoffs 2026: per-zona lphom / tslphom / nslphom (seção units) per state, bounds, state aggregates and
# net change decomposition for both finalists.
source(here::here("R", "functions.R"))

g1 <- read_proc("gov_t1")
g2 <- read_proc("gov_t2")
fin <- fread(here::here("data", "manual", "gov_2026_finalistas.csv"))
n_cores <- 8L

run_state <- function(u) {
  t1 <- g1[uf == u]; t2 <- g2[uf == u]
  ran <- function(t) names(which(colSums(t[, grep("^c[0-9]+$", names(t), value = TRUE), with = FALSE]) > 0))
  keep <- c(setdiff(names(t1), grep("^c[0-9]+$", names(t1), value = TRUE)), ran(t1))
  t1 <- t1[, ..keep]
  fc <- paste0("c", fin[uf == u, nr_candidato])
  s <- make_spec(t1, t2, setNames(as.list(fc), fc), finalists = setNames(fc, fc))
  zona <- paste(u, t1$nr_zona, sep = "_")
  b <- rbind(bounds_by(s, u)[, geo := "UF"], bounds_by(s, zona)[, geo := "zona"])[, uf := u]
  fits <- parallel::mclapply(split(seq_along(zona), zona), function(i)
    fit_zona(s$X[i, , drop = FALSE], s$Y[i, , drop = FALSE], s$cd_municipio[i]), mc.cores = n_cores)
  bad <- names(fits)[vapply(fits, function(f) inherits(f, "try-error") || any(vapply(f$zona, is.character, logical(1))), logical(1))]
  if (length(bad)) cat(u, "failed zonas:", paste(bad, collapse = ", "), "\n")
  zl <- rbindlist(lapply(setdiff(names(fits), bad), function(z) rbindlist(lapply(names(fits[[z]]$zona), function(m) {
    d <- as.data.table(as.table(fits[[z]]$zona[[m]])); setnames(d, c("row", "col", "votos")); d[, `:=`(zona = z, method = m)]
  }))))
  for (m in unique(zl$method)) {
    obs_r <- data.table(zona = zona, s$X)[, lapply(.SD, sum), by = zona] |> melt(id.vars = "zona", variable.name = "row", value.name = "obs")
    obs_c <- data.table(zona = zona, s$Y)[, lapply(.SD, sum), by = zona] |> melt(id.vars = "zona", variable.name = "col", value.name = "obs")
    er <- merge(zl[method == m, .(fit = sum(votos)), by = .(zona, row)], obs_r, by = c("zona", "row"))[, max(abs(fit - obs))]
    ec <- merge(zl[method == m, .(fit = sum(votos)), by = .(zona, col)], obs_c, by = c("zona", "col"))[, max(abs(fit - obs))]
    assert(er < 1e-3 && ec < 1e-3, sprintf("%s %s reproduces zona margins", u, m))
  }
  cat(sprintf("%s: %d seções, %d zonas, finalists %s\n", u, nrow(t1), length(fits), paste(fc, collapse = " vs ")))
  list(bounds = b, zona = zl[, uf := u])
}

res <- lapply(unique(fin$uf), run_state)
bounds <- rbindlist(lapply(res, `[[`, "bounds"))
zona_est <- rbindlist(lapply(res, `[[`, "zona"))
est <- zona_est[, .(votos = sum(votos)), by = .(uf, method, row, col)][, p := votos / sum(votos), by = .(uf, method, row)]
sw <- est[, swing_tab(.SD, intersect(unique(col), paste0("c", fin[uf == .BY$uf, nr_candidato]))), by = .(uf, method)]

dir.create(here::here("output", "tables", ano, "gov"), showWarnings = FALSE)
write_proc(bounds, "gov_bounds")
write_proc(zona_est, "gov_lp_zona")
write_proc(est, "gov_lp_uf")
fwrite(est, here::here("output", "tables", ano, "gov", "matrizes_uf.csv"))
fwrite(sw, here::here("output", "tables", ano, "gov", "swing_uf.csv"))
fwrite(bounds[geo == "UF"], here::here("output", "tables", ano, "gov", "bounds_uf.csv"))

for (u in unique(fin$uf)) {
  cat(sprintf("\n==== %s, tslphom: row-standardised (%%) ====\n", u))
  print(round(100 * xtabs(p ~ row + col, est[uf == u & method == "tslphom"]), 1))
}

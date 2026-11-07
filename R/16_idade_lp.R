# Step 16: age group (16-17, 18-69, 70+) x round outcome, per zona, by LP with the lphom objective (min sum of absolute
# seção residuals), seções as units, and each group's abstention rate fixed to the official zona rate (step 14).
# TURNO=1|2. For 2026 round 2 (not yet published) the rate is imputed: 2026 round-1 zona rate + the zona's 2022
# round-1 -> round-2 change for the group (else UF, else national mean), plus a common zona shift so that the implied
# abstention matches the observed round-2 abstention of the zona's seções.
source(here::here("R", "functions.R"))
turno <- as.integer(Sys.getenv("TURNO", "1"))

G <- c("16-17", "18-69", "70+")
K <- c("abstencao", "invalido", names(fin_ano), if (turno == 1) "outros")
t <- read_proc(paste0("secao_t", turno))
cands <- grep("^c[0-9]+$", names(t), value = TRUE)
d <- t[, .(secao_id, uf, zona = paste(uf, nr_zona), aptos, abstencao, invalido = branco + nulo)]
for (f in names(fin_ano)) set(d, j = f, value = t[[fin_ano[[f]]]])
if (turno == 1) d[, outros := rowSums(t[, cands, with = FALSE]) - rowSums(t[, unname(fin_ano), with = FALSE])]
assert(d[, all(rowSums(.SD) == aptos), .SDcols = K], "outcomes sum to aptos")
d <- merge(d, read_proc("perfil_idade_secao"), by = "secao_id")
d <- d[aptos > 0 & invalida / perfil_n < 0.01]
d[, `:=`(a16 = round(aptos * n16 / perfil_n), a70 = round(aptos * n70 / perfil_n))][, a18 := aptos - a16 - a70]
cat("seções:", nrow(d), " zonas:", uniqueN(d$zona), "\n")

zona_rates <- function(comp, tn) {
  comp[turno == tn & !is.na(idade_ini), .(r = sum(abstencao) / sum(aptos), n = sum(aptos)),
       by = .(uf, zona = paste(uf, nr_zona), g = idade3(idade_ini))]
}
comp <- read_proc("comparecimento_idade")
if (ano == 2026 && turno == 2 && !2L %in% comp$turno) {
  comp22 <- as.data.table(read_parquet(here::here("data", "processed", "2022", "comparecimento_idade.parquet")))
  d22 <- merge(zona_rates(comp22, 1), zona_rates(comp22, 2)[, .(zona, g, r2 = r)], by = c("zona", "g"))[, dlt := r2 - r]
  r <- merge(zona_rates(comp, 1), d22[, .(zona, g, dz = dlt)], by = c("zona", "g"), all.x = TRUE)
  r <- merge(r, d22[, .(du = weighted.mean(dlt, n)), by = .(uf, g)], by = c("uf", "g"), all.x = TRUE)
  r <- merge(r, d22[, .(dn = weighted.mean(dlt, n)), by = g], by = "g")
  print(r[, .(eleitores = sum(n)), by = .(fonte_delta = fifelse(!is.na(dz), "zona", fifelse(!is.na(du), "uf", "br")))])
  rates <- dcast(r[, .(zona, g, r = r + fcoalesce(dz, du, dn))], zona ~ g, value.var = "r")
  obs <- d[, .(abst_obs = sum(abstencao), a16 = sum(a16), a18 = sum(a18), a70 = sum(a70)), by = zona]
  rates <- merge(rates, obs, by = "zona")
  rates[, ajuste := (abst_obs - (a16 * `16-17` + a18 * `18-69` + a70 * `70+`)) / (a16 + a18 + a70)]
  cat("zona shift (pp):\n"); print(round(100 * quantile(rates$ajuste, c(.05, .25, .5, .75, .95)), 2))
  for (g in G) set(rates, j = g, value = pmin(pmax(rates[[g]] + rates$ajuste, 0), 1))
  fwrite(rates, out_table("abstencao_idade_t2_imputada.csv"))
} else {
  rates <- dcast(zona_rates(comp, turno), zona ~ g, value.var = "r")
}
cat("zonas without rates:", length(setdiff(unique(d$zona), rates$zona)), "\n")

lp_zona <- function(X, Y, r) {
  n <- nrow(X); nG <- length(G); nK <- length(K); np <- nG * nK; ne <- n * nK
  pidx <- function(g, k) (k - 1) * nG + g
  # seção residual rows: sum_g X[i, g] p[g, k] + e+ - e- = Y[i, k]; then each group's row of p sums to 1
  ii <- unlist(lapply(seq_len(nK), function(k) rep((k - 1) * n + seq_len(n), nG)))
  jj <- unlist(lapply(seq_len(nK), function(k) rep(pidx(seq_len(nG), k), each = n)))
  xx <- rep(as.vector(X), nK)
  ii <- c(ii, seq_len(ne), seq_len(ne), rep(ne + seq_len(nG), nK))
  jj <- c(jj, np + seq_len(ne), np + ne + seq_len(ne), seq_len(np))
  xx <- c(xx, rep(1, ne), rep(-1, ne), rep(1, np))
  A <- Matrix::sparseMatrix(ii, jj, x = xx, dims = c(ne + nG, np + 2 * ne))
  rhs <- c(as.vector(Y), rep(1, nG))
  lo <- rep(0, np + 2 * ne); up <- c(rep(1, np), rep(Inf, 2 * ne))
  lo[pidx(seq_len(nG), 1)] <- up[pidx(seq_len(nG), 1)] <- r  # abstention is outcome 1
  s <- highs::highs_solve(L = c(rep(0, np), rep(1, 2 * ne)), lower = lo, upper = up, A = A, lhs = rhs, rhs = rhs)
  if (s$status != 7) return(NULL)
  matrix(s$primal_solution[1:np], nG, nK, dimnames = list(G, K)) * colSums(X)
}

idx <- split(seq_len(nrow(d)), d$zona)
fits <- parallel::mclapply(names(idx), function(z) {
  r <- unlist(rates[zona == z, ..G])
  if (length(r) != length(G) || anyNA(r)) return(NULL)
  i <- idx[[z]]
  V <- tryCatch(lp_zona(as.matrix(d[i, .(a16, a18, a70)]), as.matrix(d[i, ..K]), r), error = function(e) NULL)
  if (is.null(V)) NULL else as.data.table(as.table(V))[, zona := z]
}, mc.cores = max(1L, parallel::detectCores() - 1L))
cat("zonas without a fit:", sum(vapply(fits, is.null, TRUE)), "\n")
res <- rbindlist(fits)
setnames(res, c("V1", "V2", "N"), c("idade", "destino", "votos"))
res[, uf := sub(" .*", "", zona)]
write_proc(res, sprintf("lp_idade_t%d", turno))

nat <- res[, .(votos = sum(votos)), by = .(idade, destino)][, pct := round(100 * votos / sum(votos), 1), by = idade]
print(dcast(nat, idade ~ destino, value.var = "pct"))
fwrite(nat, out_table(sprintf("voto_idade_t%d.csv", turno)))

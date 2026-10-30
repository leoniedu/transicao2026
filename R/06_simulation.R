# Step 6a: simulation recovery. Real round-1 margins (spec A) in small UFs, synthetic runoff from a known matrix with
# Dirichlet noise, then each method / level of pooling is scored on per-cell error against the realised transitions.
source(here::here("R", "functions.R"))

sim_ufs <- strsplit(Sys.getenv("SIM_UFS", "RR,AP,AC"), ",")[[1]]
n_reps <- as.integer(Sys.getenv("SIM_REPS", "3"))
n_cores <- 4L  # pooled-UF lphom builds dense matrices of a few GB each
set.seed(20261025)

t1 <- read_proc("secao_t1")
t2 <- read_proc("secao_t2")
s_all <- make_spec(t1, t2, specs$A)
cols <- colnames(s_all$Y)

P0 <- rbind(lula        = c(0.90, 0.01, 0.01, 0.08),
            bolsonaro      = c(0.02, 0.90, 0.01, 0.07),
            outros      = c(0.25, 0.50, 0.08, 0.17),
            invalido    = c(0.30, 0.15, 0.35, 0.20),
            abstencao   = c(0.03, 0.04, 0.01, 0.92))
colnames(P0) <- cols
stopifnot(identical(rownames(P0), colnames(s_all$X)), all(abs(rowSums(P0) - 1) < 1e-12))

rdirichlet <- function(n, a) { g <- matrix(rgamma(n * length(a), a), n, byrow = TRUE); g / rowSums(g) }

# Scenarios: iid seção-level noise around P0; zona-level drift plus seção noise (systematic geographic variation).
scenarios <- list(secao_iid = list(conc_zona = Inf, conc_secao = 50),
                  zona_drift = list(conc_zona = 30, conc_secao = 100))

simulate <- function(X, zona, sc) {
  R <- nrow(P0); K <- ncol(P0); I <- nrow(X)
  zs <- unique(zona)
  Pz <- lapply(zs, function(z) if (is.finite(sc$conc_zona)) t(sapply(seq_len(R), function(r) rdirichlet(1, sc$conc_zona * P0[r, ]))) else P0)
  names(Pz) <- zs
  Y <- matrix(0, I, K, dimnames = list(NULL, cols))
  truth <- matrix(0, R, K, dimnames = dimnames(P0))
  for (r in seq_len(R)) {
    base <- t(vapply(zona, function(z) Pz[[z]][r, ], numeric(K)))
    g <- matrix(rgamma(I * K, sc$conc_secao * base), I, K); p <- g / rowSums(g)
    draws <- t(vapply(seq_len(I), function(i) rmultinom(1, X[i, r], p[i, ])[, 1], numeric(K)))
    Y <- Y + draws
    truth[r, ] <- colSums(draws)
  }
  list(Y = Y, truth = truth)
}

lp <- function(m, X, Y) get(m, asNamespace("lphom"))(X, Y, new_and_exit_voters = "simultaneous", verbose = FALSE)$VTM.votes

estimators <- list(
  lphom_zona   = function(X, Y, zona, mun) Reduce(`+`, lapply(split(seq_len(nrow(X)), zona), function(i) lp("lphom", X[i, , drop = FALSE], Y[i, , drop = FALSE]))),
  tslphom_zona = function(X, Y, zona, mun) Reduce(`+`, lapply(split(seq_len(nrow(X)), zona), function(i) lp("tslphom", X[i, , drop = FALSE], Y[i, , drop = FALSE]))),
  nslphom_zona = function(X, Y, zona, mun) Reduce(`+`, lapply(split(seq_len(nrow(X)), zona), function(i) lp("nslphom", X[i, , drop = FALSE], Y[i, , drop = FALSE]))),
  lphom_uf     = function(X, Y, zona, mun) lp("lphom", X, Y),
  tslphom_uf   = function(X, Y, zona, mun) lp("tslphom", X, Y),
  nslphom_uf   = function(X, Y, zona, mun) lp("nslphom", X, Y),
  lphom_uf_mununits   = function(X, Y, zona, mun) lp("lphom", rowsum(X, mun), rowsum(Y, mun)),
  tslphom_uf_zonaunits = function(X, Y, zona, mun) lp("tslphom", rowsum(X, zona), rowsum(Y, zona)),
  ecolRxC_ipf_uf   = function(X, Y, zona, mun) ecolRxC::ecolRxC(X, Y, census.changes = "simultaneous", method = "IPF")$VTM.votes.global,
  ecolRxC_ipf_zona = function(X, Y, zona, mun) ecolRxC::ecolRxC(X, Y, census.changes = "simultaneous", method = "IPF", regions = zona)$VTM.votes.global
)

jobs <- CJ(uf = sim_ufs, scenario = names(scenarios), rep = seq_len(n_reps))
res <- parallel::mclapply(seq_len(nrow(jobs)), function(j) {
  jb <- jobs[j]
  i <- s_all$uf == jb$uf
  X <- s_all$X[i, ]; zona <- as.character(t1$nr_zona[i]); mun <- t1$cd_municipio[i]
  set.seed(1000 * j)
  sim <- simulate(X, zona, scenarios[[jb$scenario]])
  rbindlist(lapply(names(estimators), function(e) {
    t0 <- Sys.time()
    V <- tryCatch(estimators[[e]](X, sim$Y, zona, mun), error = function(err) NULL)
    if (is.null(V)) return(data.table(uf = jb$uf, scenario = jb$scenario, rep = jb$rep, method = e, ok = FALSE))
    V <- as.matrix(V)[rownames(P0), cols]
    P <- V / rowSums(V); Ptrue <- sim$truth / rowSums(sim$truth)
    d <- as.data.table(as.table(P - Ptrue)); setnames(d, c("row", "col", "erro"))
    d[, `:=`(uf = jb$uf, scenario = jb$scenario, rep = jb$rep, method = e, ok = TRUE,
             secs = as.numeric(Sys.time() - t0, units = "secs"))]
  }), fill = TRUE)
}, mc.cores = n_cores)
sim <- rbindlist(res, fill = TRUE)
write_proc(sim, "simulacao")

cat("failed runs:\n"); print(sim[ok == FALSE, .N, by = .(method, uf)])
s <- sim[ok == TRUE]
cat("\nmean absolute error (pp) by method x scenario, all cells / small rows (outros, invalido) / big rows:\n")
small <- c("outros", "invalido")
tab <- s[, .(mae_all = 100 * mean(abs(erro)), mae_small = 100 * mean(abs(erro[row %in% small])),
             mae_big = 100 * mean(abs(erro[!row %in% small])), max_abs = 100 * max(abs(erro)),
             secs = mean(secs)), by = .(scenario, method)][order(scenario, mae_all)]
print(tab[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
fwrite(tab, out_table("simulacao_resumo.csv"))

cat("\nmean absolute error by row (pp), zona_drift scenario:\n")
print(dcast(s[scenario == "zona_drift", .(mae = round(100 * mean(abs(erro)), 1)), by = .(method, row)], method ~ row, value.var = "mae"))

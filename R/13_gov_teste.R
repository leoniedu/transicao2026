# Governor runoffs 2026: incumbency vs presidential (Lula) mobilisation.
# vant_mob = net gain from round-1 non-choosers (invalid + abstention) of one finalist minus the other's.
source(here::here("R", "functions.R"))
set.seed(20261025)

fin <- fread(here::here("data", "manual", "gov_2026_finalistas.csv"))
fin[, cand := paste0("c", nr_candidato)]
m <- "tslphom"
nao <- c("invalido", "abstencao")

# Net gain from non-choosers per finalist, from a long count table (row, col, votos) of one unit.
mob <- function(e, cands) {
  s <- swing_tab(e, cands)
  s[contraparte %in% nao, .(mob = sum(liquido)), by = finalista]
}

# State level: governor finalists and Lula vs Flávio in the same seções' state.
gz <- read_proc("gov_lp_zona")[method == m]
pz <- read_proc("lp_zona")[method == m & spec == "A" & uf %in% fin$uf]
gs <- gz[, .(votos = sum(votos)), by = .(uf, row, col)]
ps <- pz[, .(votos = sum(votos)), by = .(uf, row, col)]
st <- rbindlist(lapply(unique(fin$uf), function(u) {
  g <- mob(gs[uf == u], fin[uf == u, cand])
  p <- mob(ps[uf == u], c("lula", "bolsonaro"))
  merge(fin[uf == u, .(uf, cand, nome_urna, partido, incumbente_exercicio, alinhamento_presidencial)], g,
        by.x = "cand", by.y = "finalista")[, `:=`(mob_lula_menos_flavio = p[finalista == "lula", mob] - p[finalista == "bolsonaro", mob])]
}))
aptos <- read_proc("gov_t1")[, .(aptos = sum(aptos)), by = uf]
st <- merge(st, aptos, by = "uf")
st[, vant_mob := mob - sum(mob) + mob, by = uf]   # own minus rival = 2*own - total
st[, `:=`(vant_mob_pct = 100 * vant_mob / aptos, lula_pct = 100 * mob_lula_menos_flavio / aptos)]
cat("state level (tslphom): net gain from round-1 non-choosers, own minus rival, % of aptos\n")
print(st[order(uf, -incumbente_exercicio), .(uf, nome_urna, partido, incumbente = incumbente_exercicio,
                                              alinhamento = alinhamento_presidencial, mob_mil = round(mob / 1e3),
                                              vant_mob_pct = round(vant_mob_pct, 2), lula_vs_flavio_pct = round(lula_pct, 2))])

# Zona level: governor finalist's non-chooser advantage vs Lula's, per zona (shares of zona aptos).
zap <- read_proc("gov_t1")[, .(aptos = sum(aptos)), by = .(zona = paste(uf, nr_zona, sep = "_"))]
zmob <- function(dt, cands) dt[, mob(.SD, cands), by = zona]
zg <- rbindlist(lapply(unique(fin$uf), function(u) {
  x <- dcast(zmob(gz[uf == u], fin[uf == u, cand]), zona ~ finalista, value.var = "mob")
  melt(x, id.vars = "zona", variable.name = "cand", value.name = "mob")[
    , vant := mob - (sum(mob) - mob), by = zona][, uf := u]
}))
zp <- dcast(zmob(pz, c("lula", "bolsonaro")), zona ~ finalista, value.var = "mob")[, lula_vant := lula - bolsonaro]
z <- merge(merge(zg, zp[, .(zona, lula_vant)], by = "zona"), zap, by = "zona")
z[, `:=`(vant_pct = vant / aptos, lula_pct = lula_vant / aptos)]
wcor <- function(x, y, w) { cv <- cov.wt(cbind(x, y), wt = w / sum(w), cor = TRUE); cv$cor[1, 2] }
zc <- z[, .(zonas = .N, cor_com_lula = wcor(vant_pct, lula_pct, aptos)), by = .(uf, cand)]
zc <- merge(zc, fin[, .(cand, uf, nome_urna, incumbente_exercicio, alinhamento_presidencial)], by = c("uf", "cand"))

# Reference under independence: governor round 2 drawn from the state's estimated governor matrix (constant across
# zonas, Dirichlet noise per seção), refitted per zona; correlation with Lula's real per-zona advantage.
g1 <- read_proc("gov_t1"); g2 <- read_proc("gov_t2")
rdir <- function(n, a) { g <- matrix(rgamma(n * length(a), a), n, byrow = TRUE); g / rowSums(g) }
null_cor <- function(u, reps = 5L, conc = 100) {
  t1 <- g1[uf == u]; fc <- fin[uf == u, cand]
  s <- make_spec(t1, g2[uf == u], setNames(as.list(fc), fc), finalists = setNames(fc, fc))
  P <- xtabs(p ~ row + col, read_proc("gov_lp_uf")[uf == u & method == m])[colnames(s$X), colnames(s$Y)]
  zona <- paste(u, t1$nr_zona, sep = "_")
  rbindlist(lapply(seq_len(reps), function(r) {
    Y <- matrix(0, nrow(s$X), ncol(P), dimnames = list(NULL, colnames(P)))
    for (j in rownames(P)) {
      p <- rdir(nrow(s$X), conc * pmax(P[j, ], 1e-4))
      Y <- Y + t(vapply(seq_len(nrow(s$X)), function(i) rmultinom(1, s$X[i, j], p[i, ])[, 1], numeric(ncol(P))))
    }
    fits <- parallel::mclapply(split(seq_along(zona), zona), function(i)
      lphom::tslphom(s$X[i, , drop = FALSE], Y[i, , drop = FALSE], new_and_exit_voters = "simultaneous", verbose = FALSE)$VTM.votes,
      mc.cores = 8L)
    zz <- rbindlist(lapply(names(fits), function(zn) {
      d <- as.data.table(as.table(fits[[zn]])); setnames(d, c("row", "col", "votos"))
      mob(d, fc)[, zona := zn]
    }))
    zz <- dcast(zz, zona ~ finalista, value.var = "mob")
    zz[, vant := get(fc[1]) - get(fc[2])]
    zz <- merge(merge(zz, zp[, .(zona, lula_vant)], by = "zona"), zap, by = "zona")
    data.table(uf = u, rep = r, cand = fc[1], cor_nula = wcor(zz$vant / zz$aptos, zz$lula_vant / zz$aptos, zz$aptos))
  }))
}
nul <- rbindlist(lapply(unique(fin$uf), null_cor))
outro <- merge(nul, fin[, .(uf, cand_outro = cand)], by = "uf", allow.cartesian = TRUE)[cand_outro != cand, .(uf, rep, cand = cand_outro, cor_nula = -cor_nula)]
nul <- rbind(nul, outro)
cat("\nnull correlations by replicate:\n"); print(dcast(nul, uf + cand ~ rep, value.var = "cor_nula")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
zc <- merge(zc, nul[, .(cor_nula_media = mean(cor_nula), cor_nula_max_abs = max(abs(cor_nula))), by = .(uf, cand)],
            by = c("uf", "cand"))
cat("\nzona level: weighted correlation between the finalist's non-chooser advantage and Lula's (same zonas)\n")
print(zc[order(uf, -cor_com_lula), .(uf, nome_urna, incumbente = incumbente_exercicio, alinhamento = alinhamento_presidencial,
                                     zonas, cor_com_lula = round(cor_com_lula, 2), cor_nula_media = round(cor_nula_media, 2),
                                     cor_nula_max_abs = round(cor_nula_max_abs, 2))])

out <- here::here("output", "tables", ano, "gov")
fwrite(st, file.path(out, "teste_estado.csv"))
fwrite(z, file.path(out, "teste_zona.csv"))
fwrite(zc, file.path(out, "teste_correlacao.csv"))

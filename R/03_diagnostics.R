# Step 3: identification diagnostics for round-1 categories (shares of padded aptos).
source(here::here("R", "functions.R"))

t1 <- read_proc("secao_t1")
cands <- grep("^c[0-9]+$", names(t1), value = TRUE)
t1[, invalido := branco + nulo]
cats <- c(cands, "branco", "nulo", "invalido", "abstencao")
nm <- unique(read_proc("votacao_secao_pres")[NR_TURNO == 1L, .(cat = paste0("c", NR_VOTAVEL), NM_VOTAVEL)])

P <- t1[, lapply(.SD, function(x) x / aptos), .SDcols = cats]
valid <- rowSums(t1[, ..cands])
summ <- data.table(cat = cats, votos = sapply(cats, function(j) sum(t1[[j]])))
summ[, `:=`(share_aptos = votos / sum(t1$aptos), share_validos = fifelse(cat %in% cands, votos / sum(valid), NA_real_),
            mean_secao = sapply(cats, function(j) mean(P[[j]])), sd_secao = sapply(cats, function(j) sd(P[[j]])))]
summ <- merge(summ, nm, by = "cat", all.x = TRUE)[order(-votos)]
cat("national, round 1 (shares of aptos; SD across seções):\n")
print(summ[, .(cat, NM_VOTAVEL, votos, share_aptos = round(share_aptos, 4), share_validos = round(share_validos, 4),
               mean_secao = round(mean_secao, 4), sd_secao = round(sd_secao, 4), cv = round(sd_secao / mean_secao, 2))])
fwrite(summ, out_table("diag_nacional.csv"))

big <- summ[cat %in% cands & share_validos > 0.01, cat]
cat("\ncandidates above 1% of valid votes:", paste(big, collapse = ", "), "\n")

# Groupings: rows of the transition matrix. Remaining candidates go to "outros".
right <- intersect(c("c55", "c14", "c30"), cands)   # Caiado, Renan, Zema
groupings <- list(
  G1_separados = c(setNames(as.list(big), big), list(outros = setdiff(cands, big))),
  G2_blocos = list(c13 = "c13", c22 = "c22", direita = right, outros = setdiff(cands, c("c13", "c22", right))),
  A_outros = list(c13 = "c13", c22 = "c22", outros = setdiff(cands, c("c13", "c22")))
)
group_shares <- function(dt, g) {
  m <- sapply(g, function(cs) rowSums(dt[, ..cs]))
  cbind(m, invalido = dt$branco + dt$nulo, abstencao = dt$abstencao) / dt$aptos
}
diag_group <- function(dt, g) {
  S <- group_shares(dt, g)
  list(cor = cor(S), kappa_shares = kappa(S, exact = TRUE), kappa_cor = kappa(cor(S), exact = TRUE))
}
for (gn in names(groupings)) {
  r <- diag_group(t1, groupings[[gn]])
  cat(sprintf("\n== %s: rows %s\n   kappa(seção shares) = %.1f | kappa(cor) = %.1f\n", gn,
              paste(names(groupings[[gn]]), collapse = "/"), r$kappa_shares, r$kappa_cor))
  print(round(r$cor, 2))
}

# By UF: candidates above 1% nationally, plus invalid and abstention.
uf_cats <- c(big, "invalido", "abstencao")
by_uf <- t1[, c(list(secoes = .N), lapply(setNames(uf_cats, paste0("share_", uf_cats)), function(j) sum(get(j)) / sum(aptos)),
                lapply(setNames(uf_cats, paste0("sd_", uf_cats)), function(j) sd(get(j) / aptos))), by = uf]
kap <- rbindlist(lapply(split(t1, by = "uf"), function(dt)
  as.data.table(c(list(uf = dt$uf[1]), lapply(groupings, function(g) kappa(group_shares(dt, g), exact = TRUE))))))
by_uf <- merge(by_uf, setnames(kap, names(groupings), paste0("kappa_", names(groupings))), by = "uf")
cat("\nby UF (shares of aptos, SD of seção shares, kappa per grouping):\n")
print(by_uf[, lapply(.SD, function(x) if (is.numeric(x) && !is.integer(x)) round(x, 3) else x)], width = 250)
fwrite(by_uf, out_table("diag_uf.csv"))

# Within-UF correlation of round-1 shares by grouping (median across UFs) for the minor rows.
cat("\nmedian within-UF correlation, G1 rows:\n")
cors <- lapply(split(t1, by = "uf"), function(dt) cor(group_shares(dt, groupings$G1_separados)))
print(round(apply(simplify2array(cors), 1:2, median), 2))

# Covariate variation (for heterogeneity / sex x vote questions).
pf <- read_proc("perfil_secao")
cat("\nSD across seções of covariate shares (perfil):\n")
pc <- grep("^p_(sexo_fem|idade_(18_24|60_69|70_mais)|escol_(superior|sem_fund))$", names(pf), value = TRUE)
print(round(pf[, lapply(.SD, function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE),
                                        p05 = quantile(x, .05, na.rm = TRUE), p95 = quantile(x, .95, na.rm = TRUE))),
               .SDcols = pc], 3))

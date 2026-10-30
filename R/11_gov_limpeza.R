# Governor runoffs 2026: one wide table per round over the seções kept for president. Governor aptos can be lower
# than presidential aptos (voters who vote only for president), so the governor tables use the governor's own aptos
# and abstention, with the same rule as 02_clean.R: aptos gaps between rounds up to 1% are padded as abstention.
source(here::here("R", "functions.R"))

v <- read_proc("gov_votacao_secao")
d <- read_proc("gov_detalhe_secao")
v[, secao_id := secao_pack(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]
d[, secao_id := secao_pack(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]

pres <- read_proc("secao_t1")[uf %in% unique(d$SG_UF)]
d <- d[secao_id %in% pres$secao_id]
aptos_tol <- 0.01
ap <- dcast(d, secao_id ~ NR_TURNO, value.var = "QT_APTOS")
setnames(ap, c("1", "2"), c("aptos_t1", "aptos_t2"))
fora <- ap[abs(aptos_t2 - aptos_t1) > aptos_tol * pmax(aptos_t1, aptos_t2), secao_id]
cat("seções kept for president:", nrow(pres), "| dropped for governor (aptos gap > 1%):", length(fora), "\n")
ap <- ap[!secao_id %in% fora]

wide <- function(turno) {
  w <- merge(pres[, .(secao_id, uf, cd_municipio, nr_zona, nr_secao, nr_local_votacao, lat, lon)],
             d[NR_TURNO == turno, .(secao_id, aptos_tse = QT_APTOS, abstencao_tse = QT_ABSTENCOES,
                                    comparecimento = QT_COMPARECIMENTO, branco = QT_VOTOS_BRANCOS,
                                    nulo = QT_VOTOS_NULOS, anulado = QT_VOTOS_ANULADOS_APU_SEP)], by = "secao_id")
  w <- merge(w, ap[, .(secao_id, aptos = pmax(aptos_t1, aptos_t2), aptos_igual = aptos_t1 == aptos_t2)], by = "secao_id")
  assert(w$anulado == 0L, sprintf("no anulados, round %d", turno))
  assert(w$aptos_tse - w$comparecimento == w$abstencao_tse, sprintf("aptos - comparecimento = abstenção, round %d", turno))
  w[, abstencao := abstencao_tse + aptos - aptos_tse]
  vt <- v[NR_TURNO == turno & !NR_VOTAVEL %in% c(nr_branco, nr_nulo, nr_anulado) & secao_id %in% w$secao_id]
  c <- dcast(vt, secao_id ~ paste0("c", NR_VOTAVEL), value.var = "QT_VOTOS", fun.aggregate = sum, fill = 0L)
  w <- merge(w, c, by = "secao_id", all.x = TRUE)
  cc <- grep("^c[0-9]+$", names(w), value = TRUE)
  for (j in cc) set(w, which(is.na(w[[j]])), j, 0L)
  tot <- rowSums(w[, c(cc, "branco", "nulo", "abstencao"), with = FALSE])
  assert(tot == w$aptos, sprintf("governor categories sum to padded aptos, round %d", turno))
  w[, c("anulado", "comparecimento") := NULL]
  setorder(w, secao_id)
  w
}
g1 <- wide(1L)
g2 <- wide(2L)
assert(identical(g1$secao_id, g2$secao_id), "same seções in both rounds")
cat("governor seções by state:\n"); print(g1[, .(secoes = .N, aptos = sum(aptos)), by = uf])
write_proc(g1, "gov_t1")
write_proc(g2, "gov_t2")

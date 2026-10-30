# Governor runoffs 2026 (cargo 3): read seção votes for the runoff states and the detalhe rows, reconcile.
source(here::here("R", "functions.R"))
stopifnot(ano == 2026L)

fin <- fread(here::here("data", "manual", "gov_2026_finalistas.csv"))
gov_ufs <- unique(fin$uf)
key <- c("NR_TURNO", "SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")

v <- rbindlist(lapply(gov_ufs, function(u) {
  zip <- raw_dir("gov", sprintf("votacao_secao_%d_%s.zip", ano, u))
  read_tse_zip(zip, sprintf("votacao_secao_%d_%s.csv", ano, u), c(key, "NR_VOTAVEL", "NM_VOTAVEL", "QT_VOTOS"), cargo = 3L)
}))
d <- read_tse_zip(raw_dir(sprintf("detalhe_votacao_secao_%d.zip", ano)), sprintf("detalhe_votacao_secao_%d_BRASIL.csv", ano),
                  c(key, "QT_APTOS", "QT_COMPARECIMENTO", "QT_ABSTENCOES", "QT_VOTOS_NOMINAIS", "QT_VOTOS_BRANCOS",
                    "QT_VOTOS_NULOS", "QT_VOTOS_LEGENDA", "QT_VOTOS_ANULADOS_APU_SEP"), cargo = 3L)
assert(setequal(unique(d[NR_TURNO == 2L, SG_UF]), gov_ufs), "governor runoff states in detalhe match the manual file")
d <- d[SG_UF %in% gov_ufs]

cand <- v[, .(votos = sum(QT_VOTOS)), by = .(SG_UF, NR_TURNO, NR_VOTAVEL, NM_VOTAVEL)][order(SG_UF, NR_TURNO, -votos)]
r2 <- cand[NR_TURNO == 2L & !NR_VOTAVEL %in% c(nr_branco, nr_nulo, nr_anulado)]
assert(r2[, .N, by = SG_UF][, all(N == 2L)], "exactly two governor finalists per runoff state")
assert(nrow(merge(r2, fin, by.x = c("SG_UF", "NR_VOTAVEL"), by.y = c("uf", "nr_candidato"))) == nrow(fin),
       "finalists match the manual coding")
cat("governor votes by state, round and candidate:\n"); print(cand)

vs <- v[, .(nom = sum(QT_VOTOS[NR_VOTAVEL < nr_branco | NR_VOTAVEL > nr_anulado]),
            bra = sum(QT_VOTOS[NR_VOTAVEL == nr_branco]), nul = sum(QT_VOTOS[NR_VOTAVEL == nr_nulo]),
            anu = sum(QT_VOTOS[NR_VOTAVEL == nr_anulado])), by = key]
rc <- merge(vs, d, by = key)
assert(nrow(rc) == nrow(vs), "every votacao seção has a detalhe row")
assert(rc[, all(nom == QT_VOTOS_NOMINAIS + QT_VOTOS_LEGENDA & bra == QT_VOTOS_BRANCOS & nul == QT_VOTOS_NULOS &
                anu == QT_VOTOS_ANULADOS_APU_SEP)], "governor votacao and detalhe reconcile per seção")
cat("detalhe-only seções (no governor votes):", nrow(d) - nrow(rc), "\n")

write_proc(v, "gov_votacao_secao")
write_proc(d, "gov_detalhe_secao")
fwrite(cand, out_table("gov_votos_por_candidato.csv"))

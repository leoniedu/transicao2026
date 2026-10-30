# Step 2: seção key, exclusions, one wide table per round (candidates, branco, nulo, abstenção) summing to aptos.
source(here::here("R", "functions.R"))

key <- c("NR_TURNO", "SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")
v <- read_proc("votacao_secao_pres")
d <- read_proc("detalhe_secao_pres")

zip_l <- raw_dir(sprintf("eleitorado_local_votacao_%d.zip", ano))
loc <- read_tse_zip(zip_l, grep("(BRASIL|_[0-9]{4})\\.csv$", zip_members(zip_l), value = TRUE),
                    c(key, "CD_TIPO_SECAO_AGREGADA", "CD_TIPO_LOCAL", "DS_TIPO_LOCAL", "NR_LOCAL_VOTACAO",
                      "NR_LATITUDE", "NR_LONGITUDE", "NR_CEP"), cargo = NULL)
assert(!anyDuplicated(loc, by = key), "eleitorado_local_votacao unique by round x seção")
cat("polling places by type and round (seções):\n")
print(dcast(loc[, .N, by = .(NR_TURNO, DS_TIPO_LOCAL)], DS_TIPO_LOCAL ~ NR_TURNO, value.var = "N"))

d <- merge(d, loc[, !c("NR_LOCAL_VOTACAO")], by = key, all.x = TRUE)
cat("detalhe seções without a polling-place record:", d[is.na(CD_TIPO_LOCAL), .N], "\n")
d[, secao_id := secao_pack(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]
v[, secao_id := secao_pack(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]

# Exclusions, applied in order; each step counts seções (pairs of rounds) and round-1 aptos removed.
ex <- list()
drop <- function(ids, why) {
  ids <- unique(ids)
  gone <- d[secao_id %in% ids & !secao_id %in% dropped]
  ex[[length(ex) + 1L]] <<- data.table(motivo = why, secoes = uniqueN(gone$secao_id),
                                       aptos_t1 = gone[NR_TURNO == 1L, sum(QT_APTOS)],
                                       comparecimento_t1 = gone[NR_TURNO == 1L, sum(QT_COMPARECIMENTO)],
                                       comparecimento_t2 = gone[NR_TURNO == 2L, sum(QT_COMPARECIMENTO)])
  dropped <<- union(dropped, ids)
}
dropped <- bit64::integer64()
drop(d[SG_UF == "ZZ", secao_id], "exterior (ZZ)")
drop(d[CD_TIPO_LOCAL == 2L, secao_id], "voto em trânsito (CD_TIPO_LOCAL = 2, either round)")
drop(d[ST_SECAO_ANULADA != "Não", secao_id], "seção anulada (either round)")
drop(d[!secao_id %in% v$secao_id, secao_id], "no votes recorded in a round (comparecimento = 0)")
n_round <- d[, .(n = uniqueN(NR_TURNO)), by = secao_id]
drop(n_round[n < 2L, secao_id], "not present in both rounds")
# Aptos differ by a voter or two in ~11% of seções (per-round roll changes). Keep gaps up to 1% of aptos;
# voters absent from one round's roll are counted as abstention in that round (see wide()).
aptos_tol <- 0.01
ap <- dcast(d, secao_id ~ NR_TURNO, value.var = "QT_APTOS")
setnames(ap, c("1", "2"), c("aptos_t1", "aptos_t2"))
drop(ap[abs(aptos_t2 - aptos_t1) > aptos_tol * pmax(aptos_t1, aptos_t2), secao_id],
     sprintf("QT_APTOS differs between rounds by more than %g%%", 100 * aptos_tol))
ex <- rbindlist(ex)
ex <- rbind(data.table(motivo = "all seções", secoes = uniqueN(d$secao_id), aptos_t1 = d[NR_TURNO == 1L, sum(QT_APTOS)],
                       comparecimento_t1 = d[NR_TURNO == 1L, sum(QT_COMPARECIMENTO)],
                       comparecimento_t2 = d[NR_TURNO == 2L, sum(QT_COMPARECIMENTO)]), ex)
cat("\nexclusions:\n"); print(ex)
fwrite(ex, out_table("exclusoes.csv"))

d <- d[!secao_id %in% dropped]
v <- v[secao_id %in% d$secao_id]

# Wide table per round. Candidate columns are c<NR_VOTAVEL>.
wide <- function(turno) {
  vt <- v[NR_TURNO == turno & !NR_VOTAVEL %in% c(nr_branco, nr_nulo, nr_anulado)]
  w <- dcast(vt, secao_id ~ paste0("c", NR_VOTAVEL), value.var = "QT_VOTOS", fun.aggregate = sum, fill = 0L)
  dt <- d[NR_TURNO == turno, .(secao_id, uf = SG_UF, cd_municipio = CD_MUNICIPIO, nr_zona = NR_ZONA,
                               nr_secao = NR_SECAO, nr_local_votacao = NR_LOCAL_VOTACAO, cd_tipo_local = CD_TIPO_LOCAL,
                               lat = NR_LATITUDE, lon = NR_LONGITUDE, cep = NR_CEP, aptos = QT_APTOS,
                               branco = QT_VOTOS_BRANCOS, nulo = QT_VOTOS_NULOS, anulado = QT_VOTOS_ANULADOS_APU_SEP,
                               abstencao = QT_ABSTENCOES, comparecimento = QT_COMPARECIMENTO)]
  w <- merge(dt, w, by = "secao_id", all.x = TRUE)
  cc <- grep("^c[0-9]+$", names(w), value = TRUE)
  for (j in cc) set(w, which(is.na(w[[j]])), j, 0L)
  assert(w$anulado == 0L, sprintf("no anulados apurados em separado in round %d", turno))
  w[, anulado := NULL]
  assert(w$aptos - w$comparecimento == w$abstencao, sprintf("aptos - comparecimento = abstenção, round %d", turno))
  tot <- rowSums(w[, c(cc, "branco", "nulo", "abstencao"), with = FALSE])
  assert(tot == w$aptos, sprintf("categories sum to aptos in every seção, round %d", turno))
  # Pad abstention so both rounds share the larger roll; keep the published aptos and a strict-equality flag.
  w <- merge(w, ap[, .(secao_id, aptos_max = pmax(aptos_t1, aptos_t2), aptos_igual = aptos_t1 == aptos_t2)],
             by = "secao_id")
  setnames(w, "aptos", "aptos_tse")
  w[, `:=`(abstencao_tse = abstencao, abstencao = abstencao + aptos_max - aptos_tse, aptos = aptos_max)]
  w[, c("comparecimento", "aptos_max") := NULL]
  tot <- rowSums(w[, c(cc, "branco", "nulo", "abstencao"), with = FALSE])
  assert(tot == w$aptos, sprintf("categories sum to padded aptos in every seção, round %d", turno))
  setorder(w, secao_id)
  w
}
t1 <- wide(1L)
t2 <- wide(2L)
assert(identical(t1$secao_id, t2$secao_id) && identical(t1$aptos, t2$aptos), "same seções and aptos in both rounds")
cat("\nkept seções:", nrow(t1), "| aptos:", sum(t1$aptos), "\n")
cat("round 2 candidate columns:", paste(grep("^c[0-9]+$", names(t2), value = TRUE), collapse = ", "), "\n")

write_proc(t1, "secao_t1")
write_proc(t2, "secao_t2")

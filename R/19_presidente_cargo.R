# Step 19: presidential vote x vote for another office on the same ballot, round 1, 2022 and 2026.
# CARGO=5 (Senate: one seat in 2022, two in 2026, so each voter casts two Senate votes in 2026) or CARGO=6 (federal
# deputy: one vote; 2-digit party-only votes are valid and count for the party).
# Rows: presidential choice of each voter, counted once per vote (Lula, bolsonaro = Jair in 2022 and Flávio in 2026,
# outros, branco/nulo). Columns: votes for the office (PL; PT, or for deputies the PT/PCdoB/PV federation; for deputies
# also União + PP, federated in 2026; other parties; branco; nulo; `sem_voto` for in-transit voters from other UFs, who
# vote for president only). tslphom per zona, seções as units, summed to UF and BR.
# Senate: p[Lula, PL] is the share of Lula voters' Senate votes that went to PL. Where PL ran one candidate a voter can
# give PL at most one vote, so PL votes from Lula voters = Lula voters who voted PL; with two candidates in a two-seat
# year that count lies between votes / 2 and votes.
source(here::here("R", "functions.R"))
cargo <- as.integer(Sys.getenv("CARGO", "5"))
nome <- c(`5` = "senado", `6` = "deputado")[[as.character(cargo)]]
vagas <- if (cargo == 5L) c(`2022` = 1L, `2026` = 2L)[[as.character(ano)]] else 1L
grupos <- if (cargo == 5L) list(pl = 22L, pt = 13L) else list(pl = 22L, pt = c(13L, 65L, 43L), uniao_pp = c(44L, 11L))

only_uf <- Sys.getenv("ONLY_UF")   # e.g. ONLY_UF=AC,ES for a test run
ufs <- if (nzchar(only_uf)) strsplit(only_uf, ",")[[1]] else ufs_br
key <- c("SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")
v <- rbindlist(lapply(ufs, function(u) {
  cat(u, "")
  x <- read_tse_zip(raw_dir("gov", sprintf("votacao_secao_%d_%s.zip", ano, u)), sprintf("votacao_secao_%d_%s.csv", ano, u),
                    c("NR_TURNO", key, "NR_VOTAVEL", "NM_VOTAVEL", "QT_VOTOS"), cargo = cargo)[NR_TURNO == 1L]
  x[, partido := fcase(NR_VOTAVEL %in% c(nr_branco, nr_nulo, nr_anulado), NA_integer_,
                       NR_VOTAVEL < 100L, NR_VOTAVEL,                       # party-only vote (deputies)
                       NR_VOTAVEL < 1000L, NR_VOTAVEL %/% 10L,              # Senate candidate
                       default = NR_VOTAVEL %/% 100L)]                       # deputy candidate
  x
}))
cat("\n")
cand <- v[!is.na(partido), .(votos = sum(QT_VOTOS)), by = .(uf = SG_UF, NR_VOTAVEL, NM_VOTAVEL, partido)][order(uf, -votos)]
n_pl <- cand[NR_VOTAVEL >= 100L, .(n_pl = sum(partido == 22L)), by = uf]
fwrite(cand, out_table(paste0(nome, "_candidatos.csv")))

s <- v[, c(lapply(grupos, function(g) sum(QT_VOTOS[partido %in% g])),
           list(outros_c = sum(QT_VOTOS[!is.na(partido) & !partido %in% unlist(grupos)]),
                branco_c = sum(QT_VOTOS[NR_VOTAVEL == nr_branco]), nulo_c = sum(QT_VOTOS[NR_VOTAVEL == nr_nulo]),
                anulado = sum(QT_VOTOS[NR_VOTAVEL == nr_anulado]))), by = key]
s[, secao_id := secao_pack(as.integer(CD_MUNICIPIO), NR_ZONA, NR_SECAO)]

t1 <- read_proc("secao_t1")[uf %in% ufs]
cands <- grep("^c[0-9]+$", names(t1), value = TRUE)
p <- t1[, .(secao_id, uf, cd_municipio, zona = paste(uf, nr_zona), lula = c13, bolsonaro = c22, inval = branco + nulo,
            pres_branco = branco, pres_nulo = nulo)]
p[, outros := rowSums(t1[, cands, with = FALSE]) - lula - bolsonaro]
d <- merge(p, s[, !c(key), with = FALSE], by = "secao_id")
cols <- c(names(grupos), "outros_c", "branco_c", "nulo_c", "sem_voto")
d[, `:=`(votantes = lula + bolsonaro + outros + inval, votos_c = rowSums(.SD)), .SDcols = setdiff(cols, "sem_voto")]
# In-transit voters from other UFs vote for president only: their missing votes for the office go to `sem_voto`.
d[, sem_voto := vagas * votantes - votos_c]
chk <- d[, .(secoes = .N, igual = sum(sem_voto == 0L), negativo = sum(sem_voto < 0L), resto = sum(sem_voto %% vagas != 0L),
             anulado = sum(anulado > 0), sem_voto = sum(sem_voto) / vagas)]
cat("seções matched:", nrow(d), "of", nrow(t1), "\n"); print(chk)
d <- d[sem_voto >= 0L & sem_voto %% vagas == 0L & anulado == 0L]
write_proc(d, paste0(nome, "_secao"))

rows <- c("lula", "bolsonaro", "outros", "inval")
idx <- split(seq_len(nrow(d)), d$zona)
fits <- parallel::mclapply(names(idx), function(z) {
  i <- idx[[z]]
  X <- vagas * as.matrix(d[i, ..rows]); Y <- as.matrix(d[i, ..cols])
  r <- tryCatch(lphom::tslphom(X, Y, new_and_exit_voters = "raw", verbose = FALSE), error = function(e) NULL)
  if (is.null(r)) return(NULL)
  V <- r$VTM.votes; dimnames(V) <- list(rows, cols)
  as.data.table(as.table(V))[, zona := z]
}, mc.cores = max(1L, parallel::detectCores() - 1L))
cat("zonas:", length(idx), " failed:", sum(vapply(fits, is.null, TRUE)), "\n")
res <- rbindlist(fits)
setnames(res, c("V1", "V2", "N"), c("presidente", "voto", "votos"))
res[, uf := sub(" .*", "", zona)]
write_proc(res, paste0("lp_", nome))

# UF and BR tables: votes for the office by presidential choice.
uf_tab <- dcast(res, uf + presidente ~ voto, value.var = "votos", fun.aggregate = sum)
uf_tab[, eleitores := rowSums(.SD) / vagas, .SDcols = cols]
if (cargo == 5L) {
  uf_tab <- merge(uf_tab, n_pl, by = "uf")
  uf_tab[, `:=`(eleitores_pl_min = fifelse(n_pl <= 1L, pl, pl / vagas), eleitores_pl_max = pmin(pl, eleitores))]
}
fwrite(uf_tab, out_table(paste0(nome, "_presidente_uf.csv")))
br <- uf_tab[, lapply(.SD, sum), by = presidente, .SDcols = intersect(c(cols, "eleitores", "eleitores_pl_min", "eleitores_pl_max"), names(uf_tab))]
for (j in setdiff(cols, "sem_voto")) set(br, j = paste0("pct_", j), value = 100 * br[[j]] / (vagas * br$eleitores))
fwrite(br, out_table(paste0(nome, "_presidente_br.csv")))
options(width = 200, digits = 3)
print(br)

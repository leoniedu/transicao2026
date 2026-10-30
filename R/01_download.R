# Step 1: locate the seção files for `ano` via CKAN, download, check schema, reconcile the two sources.
source(here::here("R", "functions.R"))

pkg <- jsonlite::fromJSON(sprintf("%s/package_show?id=resultados-%d", ckan_api, ano))$result
res <- as.data.table(pkg$resources)[, .(name, url)]
f_v <- sprintf("votacao_secao_%d_BR.zip", ano)
f_d <- sprintf("detalhe_votacao_secao_%d.zip", ano)
want <- res[basename(url) %in% c(f_v, f_d)]
assert(nrow(want) == 2L, sprintf("CKAN lists %s and %s", f_v, f_d))

dir.create(raw_dir(), recursive = TRUE, showWarnings = FALSE)
options(timeout = 7200)
for (u in want$url) {
  dest <- raw_dir(basename(u))
  if (!file.exists(dest)) download.file(u, dest, mode = "wb", quiet = TRUE)
}
want[, `:=`(file = basename(url), bytes = file.size(raw_dir(basename(url))),
            md5 = unname(tools::md5sum(raw_dir(basename(url)))), retrieved = format(Sys.time()))]
fwrite(want, raw_dir("manifest.csv"))

zip_v <- raw_dir(f_v)
zip_d <- raw_dir(f_d)
mem_v <- grep("\\.csv$", zip_members(zip_v), value = TRUE)
mem_d <- sprintf("detalhe_votacao_secao_%d_BRASIL.csv", ano)
assert(mem_d %in% zip_members(zip_d), "detalhe zip has the BRASIL member")

cat("votacao_secao columns:\n"); print(zip_header(zip_v, mem_v))
cat("detalhe_votacao_secao columns:\n"); print(zip_header(zip_d, mem_d))

key <- c("NR_TURNO", "SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")
v <- read_tse_zip(zip_v, mem_v, c(key, "NR_VOTAVEL", "NM_VOTAVEL", "QT_VOTOS", "NR_LOCAL_VOTACAO"))
# ST_SECAO_ANULADA only exists from 2018 on; earlier files are taken as having no annulled seções.
cols_d <- c(key, "QT_APTOS", "QT_COMPARECIMENTO", "QT_ABSTENCOES", "QT_VOTOS_NOMINAIS", "QT_VOTOS_BRANCOS",
            "QT_VOTOS_NULOS", "QT_VOTOS_LEGENDA", "QT_VOTOS_ANULADOS_APU_SEP", "NR_LOCAL_VOTACAO")
tem_anulada <- "ST_SECAO_ANULADA" %in% zip_header(zip_d, mem_d)
d <- read_tse_zip(zip_d, mem_d, c(cols_d, if (tem_anulada) "ST_SECAO_ANULADA"))
if (!tem_anulada) d[, ST_SECAO_ANULADA := "Não"]
cat("\ncolumn types, votacao_secao:\n"); print(sapply(v, class))
cat("column types, detalhe:\n"); print(sapply(d, class))

assert(setequal(unique(v$NR_TURNO), 1:2), "votacao_secao has both rounds")
assert(setequal(unique(d$NR_TURNO), 1:2), "detalhe has both rounds")

cat("\nSG_UF values with seção counts, by round:\n")
print(dcast(d[, .N, by = .(NR_TURNO, SG_UF)], SG_UF ~ NR_TURNO, value.var = "N"))
cat("\nST_SECAO_ANULADA:\n"); print(d[, .N, by = .(NR_TURNO, ST_SECAO_ANULADA)])

cand <- v[, .(votos = sum(QT_VOTOS)), by = .(NR_TURNO, NR_VOTAVEL, NM_VOTAVEL)][order(NR_TURNO, -votos)]
cat("\nvotes per votável and round:\n"); print(cand)
fin <- cand[NR_TURNO == 2L & !NR_VOTAVEL %in% c(nr_branco, nr_nulo, nr_anulado), NR_VOTAVEL]
assert(length(fin) == 2L, "runoff has exactly two candidates")
assert(all(fin %in% cand[NR_TURNO == 1L, NR_VOTAVEL]), "finalists ran in round 1")

# Reconcile per seção: nominal, branco, nulo, anulado from votacao_secao vs detalhe.
vs <- v[, .(nom = sum(QT_VOTOS[NR_VOTAVEL < nr_branco | NR_VOTAVEL > nr_anulado]),
            bra = sum(QT_VOTOS[NR_VOTAVEL == nr_branco]),
            nul = sum(QT_VOTOS[NR_VOTAVEL == nr_nulo]),
            anu = sum(QT_VOTOS[NR_VOTAVEL == nr_anulado])), by = key]
rc <- merge(vs, d, by = key, all = TRUE)
cat("\nseções only in votacao_secao:", rc[is.na(QT_APTOS), .N],
    "| only in detalhe:", rc[is.na(nom), .N], "\n")
cat("detalhe-only seções by round and ST_SECAO_ANULADA / QT_COMPARECIMENTO==0:\n")
print(rc[is.na(nom), .N, by = .(NR_TURNO, ST_SECAO_ANULADA, sem_comparecimento = QT_COMPARECIMENTO == 0)])
both <- rc[!is.na(nom) & !is.na(QT_APTOS)]
for (n in c("nom", "bra", "nul", "anu")) set(both, which(is.na(both[[n]])), n, 0L)
mism <- both[nom != QT_VOTOS_NOMINAIS + QT_VOTOS_LEGENDA | bra != QT_VOTOS_BRANCOS | nul != QT_VOTOS_NULOS |
               anu != QT_VOTOS_ANULADOS_APU_SEP]
cat("seções where the two files disagree:", nrow(mism), "\n")
if (nrow(mism)) print(head(mism, 20))
assert(nrow(mism) == 0L, "votacao_secao and detalhe reconcile per seção")
assert(both[, all(QT_COMPARECIMENTO + QT_ABSTENCOES == QT_APTOS)], "comparecimento + abstenções = aptos")
assert(both[, all(nom + bra + nul + anu == QT_COMPARECIMENTO)], "votes sum to comparecimento")

cat("\nnational totals by round (detalhe):\n")
print(d[, lapply(.SD, sum), by = NR_TURNO, .SDcols = patterns("^QT_")])

write_proc(v, "votacao_secao_pres")
write_proc(d, "detalhe_secao_pres")
fwrite(cand, out_table("votos_por_candidato.csv"))

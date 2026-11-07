# Step 15: seção age composition (16-17, 18-69, 70+) from perfil_eleitor_secao_<ano>, matched to the cleaned seções.
source(here::here("R", "functions.R"))

read_uf <- function(uf) {
  f <- sprintf("perfil_eleitor_secao_%d_%s", ano, uf)
  zip <- fetch_raw(sprintf("%s/perfil_eleitor_secao/%s.zip", tse_base, f), raw_dir("perfil", paste0(f, ".zip")))
  qt <- intersect(c("QT_ELEITORES", "QT_ELEITORES_PERFIL"), zip_header(zip, paste0(f, ".csv")))[1]
  p <- read_tse_zip(zip, paste0(f, ".csv"), c("CD_MUNICIPIO", "NR_ZONA", "NR_SECAO", "DS_FAIXA_ETARIA", qt), cargo = NULL)
  p[, .(n = sum(get(qt))), by = .(secao_id = secao_pack(as.integer(CD_MUNICIPIO), NR_ZONA, NR_SECAO),
                                  idade = fcoalesce(idade3(faixa_inicio(DS_FAIXA_ETARIA)), "invalida"))]
}
p <- rbindlist(lapply(ufs_br, function(u) { cat(u, ""); read_uf(u) }))
w <- dcast(p, secao_id ~ idade, value.var = "n", fill = 0L)
setnames(w, c("16-17", "18-69", "70+"), c("n16", "n18", "n70"))
w[, perfil_n := n16 + n18 + n70 + invalida]

t1 <- read_proc("secao_t1")[, .(secao_id, aptos)]
m <- merge(t1, w, by = "secao_id", all.x = TRUE)
cat("\ncleaned seções:", nrow(t1), " without perfil:", m[is.na(perfil_n), .N],
    " perfil_n == aptos:", round(m[, mean(perfil_n == aptos, na.rm = TRUE)], 3), "\n")
print(w[secao_id %in% t1$secao_id, lapply(.SD, sum), .SDcols = c("n16", "n18", "n70", "invalida")])
write_proc(w[secao_id %in% t1$secao_id], "perfil_idade_secao")

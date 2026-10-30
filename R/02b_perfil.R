# Step 2b: seção-level covariates from perfil_eleitor_secao_2026 (roll as of July 2026), matched to the cleaned seções.
source(here::here("R", "functions.R"))

ufs <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR",
         "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")
cols <- c("DT_GERACAO", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO", "DS_GENERO", "DS_FAIXA_ETARIA",
          "DS_GRAU_ESCOLARIDADE", "DS_RACA_COR", "QT_ELEITORES")

idade_grupo <- function(ds) {
  lo <- ifelse(grepl("^[0-9]", ds), as.integer(sub("^([0-9]+).*", "\\1", ds)), NA_integer_)
  cut(lo, c(-Inf, 17, 24, 34, 44, 59, 69, Inf), right = TRUE,
      labels = c("16_17", "18_24", "25_34", "35_44", "45_59", "60_69", "70_mais"))
}
escol_grupo <- function(ds) {
  fcase(ds %in% c("ANALFABETO", "LÊ E ESCREVE"), "sem_fund",
        grepl("FUNDAMENTAL", ds), "fundamental",
        grepl("MÉDIO", ds), "medio",
        grepl("SUPERIOR", ds), "superior",
        default = "nao_informado")
}
raca_grupo <- function(ds) {
  fcase(ds == "Branca", "branca", ds == "Preta", "preta", ds == "Parda", "parda",
        ds == "Amarela", "amarela", ds == "Indígena", "indigena", default = "nao_informado")
}

read_uf <- function(uf) {
  zip <- raw_dir("perfil", sprintf("perfil_eleitor_secao_%d_%s.zip", ano, uf))
  p <- read_tse_zip(zip, sprintf("perfil_eleitor_secao_%d_%s.csv", ano, uf), cols, cargo = NULL)
  p[, secao_id := secao_pack(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]
  p[, `:=`(sexo = fcase(DS_GENERO == "FEMININO", "fem", DS_GENERO == "MASCULINO", "masc", default = "nao_informado"),
           idade = as.character(idade_grupo(DS_FAIXA_ETARIA)),
           escol = escol_grupo(DS_GRAU_ESCOLARIDADE), raca = raca_grupo(DS_RACA_COR))]
  p[is.na(idade), idade := "invalida"]
  long <- rbindlist(lapply(c("sexo", "idade", "escol", "raca"), function(v)
    p[, .(var = v, n = sum(QT_ELEITORES)), by = .(secao_id, cat = get(v))]))
  list(long = long, geracao = unique(p$DT_GERACAO))
}

res <- lapply(ufs, function(u) { cat(u, ""); read_uf(u) })
cat("\ngeneration dates in perfil files:", paste(unique(unlist(lapply(res, `[[`, "geracao"))), collapse = ", "), "\n")
long <- rbindlist(lapply(res, `[[`, "long"))

tot <- long[var == "sexo", .(perfil_n = sum(n)), by = secao_id]
assert(long[, .(s = sum(n)), by = .(secao_id, var)][tot, on = "secao_id"][, all(s == perfil_n)],
       "each perfil variable sums to the same seção total")

wide <- dcast(long, secao_id ~ paste(var, cat, sep = "_"), value.var = "n", fill = 0L)
sh <- setdiff(names(wide), "secao_id")
wide <- merge(wide, tot, by = "secao_id")
for (j in sh) set(wide, j = paste0("p_", j), value = wide[[j]] / wide$perfil_n)

t1 <- read_proc("secao_t1")[, .(secao_id, uf, aptos_tse)]
t2 <- read_proc("secao_t2")[, .(secao_id, aptos_tse_t2 = aptos_tse)]
m <- merge(merge(t1, t2, by = "secao_id"), wide[, .(secao_id, perfil_n)], by = "secao_id", all.x = TRUE)
cat("cleaned seções without a perfil record:", m[is.na(perfil_n), .N], "\n")
match_tab <- m[, .(secoes = .N, sem_perfil = sum(is.na(perfil_n)),
                   igual_t1 = mean(perfil_n == aptos_tse, na.rm = TRUE),
                   igual_t2 = mean(perfil_n == aptos_tse_t2, na.rm = TRUE),
                   dif_rel_mediana = median(abs(perfil_n - aptos_tse) / aptos_tse, na.rm = TRUE)), by = uf]
match_tab <- rbind(match_tab, m[, .(uf = "BR", secoes = .N, sem_perfil = sum(is.na(perfil_n)),
                                    igual_t1 = mean(perfil_n == aptos_tse, na.rm = TRUE),
                                    igual_t2 = mean(perfil_n == aptos_tse_t2, na.rm = TRUE),
                                    dif_rel_mediana = median(abs(perfil_n - aptos_tse) / aptos_tse, na.rm = TRUE))])
cat("\nperfil seção totals vs QT_APTOS (share of seções matching exactly):\n"); print(match_tab)
fwrite(match_tab, out_table("perfil_vs_aptos.csv"))

nat <- long[secao_id %in% t1$secao_id, .(n = sum(n)), by = .(var, cat)][, share := n / sum(n), by = var]
cat("\nnational composition (cleaned seções):\n"); print(nat[order(var, -share)])
raca_ni <- merge(long[var == "raca" & secao_id %in% t1$secao_id], t1[, .(secao_id, uf)], by = "secao_id")[
  , .(nao_informado = sum(n[cat == "nao_informado"]) / sum(n)), by = uf][order(-nao_informado)]
cat("\nrace/colour not informed, by UF:\n"); print(raca_ni)
fwrite(nat, out_table("perfil_nacional.csv"))

write_proc(wide[secao_id %in% t1$secao_id], "perfil_secao")

# Step 22: party-only (2-digit) federal deputy votes, round 1 of 2022 and 2026: totals by party (from step 19,
# CARGO=6) and seção regressions of the party-only share of voters on schooling, age, sex and presidential vote shares
# (zona FE, voters as weights, SEs clustered by zona). For the Senate a 2-digit entry is not a candidate number and
# ends up null, so these votes help read the Senate-deputy contrast in steps 20-21.
source(here::here("R", "functions.R"))
out <- list()
options(width = 200, digits = 3)
rp <- function(y, n) as.data.table(arrow::read_parquet(here::here("data", "processed", y, paste0(n, ".parquet"))))
key <- c("NR_TURNO", "SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")
ctrl <- c("p_idade_16_17", "p_idade_18_24", "p_idade_25_34", "p_idade_45_59", "p_idade_60_69", "p_idade_70_mais", "p_sexo_fem")
escol <- c("p_escol_sem_fund", "p_escol_fundamental", "p_escol_superior", "p_escol_nao_informado")
for (y in c("2022", "2026")) {
  cand <- fread(here::here("output", "tables", y, "deputado_candidatos.csv"))
  leg <- cand[NR_VOTAVEL < 100L, .(legenda = sum(votos)), by = partido]
  tot <- cand[, .(total = sum(votos)), by = partido]
  l <- merge(leg, tot, by = "partido")[, `:=`(pct_legenda_no_partido = 100 * legenda / total, pct_das_legendas = 100 * legenda / sum(legenda))]
  cat("\n====", y, "==== party-only share of all deputy votes:", round(100 * sum(leg$legenda) / sum(cand$votos), 2), "%\n")
  print(l[order(-legenda)][1:8])
  out[[paste0("partido_", y)]] <- l[, ano := as.integer(y)]
  zip <- here::here("data", "raw", y, sprintf("detalhe_votacao_secao_%s.zip", y))
  d <- read_tse_zip(zip, sprintf("detalhe_votacao_secao_%s_BRASIL.csv", y), c(key, "QT_VOTOS_LEGENDA", "QT_VOTOS_NOMINAIS"), cargo = 6L)[NR_TURNO == 1L]
  d[, secao_id := secao_pack(as.integer(CD_MUNICIPIO), NR_ZONA, NR_SECAO)]
  s <- merge(rp(y, "deputado_secao"), d[, .(secao_id, QT_VOTOS_LEGENDA, QT_VOTOS_NOMINAIS)], by = "secao_id")
  s <- merge(s, rp(y, "perfil_secao")[, c("secao_id", escol, ctrl, "p_idade_invalida"), with = FALSE], by = "secao_id")[p_idade_invalida < 0.01]
  s[, `:=`(legenda = QT_VOTOS_LEGENDA / votantes, s_lula = lula / votantes, s_bolsonaro = bolsonaro / votantes, s_inval = inval / votantes)]
  r <- rbind(fe_ols(s, "legenda", c(escol, ctrl), w = "votantes")[, modelo := "escolaridade"],
             fe_ols(s, "legenda", c("s_lula", "s_bolsonaro", "s_inval"), w = "votantes")[, modelo := "presidente"],
             fe_ols(s, "legenda", c("s_lula", "s_bolsonaro", "s_inval", escol, ctrl), w = "votantes")[, modelo := "ambos"])
  r[, `:=`(coef_pp = round(100 * coef, 2), ic95 = sprintf("[%.1f, %.1f]", 100 * (coef - 1.96 * se), 100 * (coef + 1.96 * se)))]
  cat("mean party-only share of voters:", round(100 * s[, weighted.mean(legenda, votantes)], 2), "%\n")
  print(r[termo %in% c("p_escol_sem_fund", "p_escol_superior", "s_lula", "s_bolsonaro"), .(modelo, termo, coef_pp, ic95)])
  out[[paste0("reg_", y)]] <- r[, ano := as.integer(y)]
}
dir.create(here::here("output", "tables", "comparacao"), showWarnings = FALSE)
fwrite(rbindlist(out[grep("^partido", names(out))]), here::here("output", "tables", "comparacao", "deputado_legenda_partido.csv"))
fwrite(rbindlist(out[grep("^reg", names(out))]), here::here("output", "tables", "comparacao", "deputado_legenda_regressao.csv"))

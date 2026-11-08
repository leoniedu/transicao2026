# Step 21: CARGO=5: does the two-seat Senate election (2026) cost less-schooled voters more of their Senate votes than
# the one-seat election (2022)? CARGO=6: the same for federal deputies (one vote both years). Seção regressions, per
# year, of blank and null shares of votes for the office and, as a same-day baseline, of presidential votes, on schooling shares (reference: ensino médio), with age, sex and presidential vote
# shares as controls; zona FE, voters as weights, SEs clustered by zona. The "excess" outcome is office minus
# presidential share for the same seção.
source(here::here("R", "functions.R"))

cargo <- as.integer(Sys.getenv("CARGO", "5"))
nome <- c(`5` = "senado", `6` = "deputado")[[as.character(cargo)]]
vagas <- if (cargo == 5L) c(`2022` = 1L, `2026` = 2L) else c(`2022` = 1L, `2026` = 1L)
ctrl <- c("p_idade_16_17", "p_idade_18_24", "p_idade_25_34", "p_idade_45_59", "p_idade_60_69", "p_idade_70_mais",
          "p_sexo_fem", "s_lula", "s_bolsonaro")
escol <- c("p_escol_sem_fund", "p_escol_fundamental", "p_escol_superior", "p_escol_nao_informado")
read_year <- function(y, name) as.data.table(arrow::read_parquet(here::here("data", "processed", y, paste0(name, ".parquet"))))

res <- rbindlist(lapply(names(vagas), function(y) {
  d <- merge(read_year(y, paste0(nome, "_secao")), read_year(y, "perfil_secao")[, c("secao_id", escol, ctrl[1:7], "p_idade_invalida"), with = FALSE],
             by = "secao_id")[p_idade_invalida < 0.01]
  v <- vagas[[y]]
  d[, `:=`(s_lula = lula / votantes, s_bolsonaro = bolsonaro / votantes,
           sen_nulo = nulo_c / (v * votantes), sen_branco = branco_c / (v * votantes),
           pres_nulo = pres_nulo / votantes, pres_branco = pres_branco / votantes)]
  d[, `:=`(exc_nulo = sen_nulo - pres_nulo, exc_branco = sen_branco - pres_branco)]
  cat(y, "seções:", nrow(d), " zonas:", uniqueN(d$zona), "\n")
  ys <- c("sen_nulo", "sen_branco", "pres_nulo", "pres_branco", "exc_nulo", "exc_branco")
  means <- d[, lapply(.SD, weighted.mean, w = votantes), .SDcols = ys]
  rbindlist(lapply(ys, function(o) fe_ols(d, o, c(escol, ctrl), w = "votantes")[termo %in% escol][
    , `:=`(ano = as.integer(y), desfecho = o, media = means[[o]])]))
}))
res[, `:=`(coef_pp = round(100 * coef, 2), ic95_pp = sprintf("[%.1f, %.1f]", 100 * (coef - 1.96 * se), 100 * (coef + 1.96 * se)),
           media_pct = round(100 * media, 2))]
options(width = 200)
print(dcast(res[termo %in% c("p_escol_sem_fund", "p_escol_superior")], desfecho + termo ~ ano, value.var = c("coef_pp", "ic95_pp"))
      [order(termo, desfecho)])
print(unique(res[, .(ano, desfecho, media_pct)])[order(desfecho, ano)])

# 2026 - 2022 difference in the low-schooling gradient (independent samples).
dd <- dcast(res, desfecho + termo ~ ano, value.var = c("coef", "se"))
dd[, `:=`(dif_pp = round(100 * (coef_2026 - coef_2022), 2), ic95_pp = sprintf("[%.1f, %.1f]",
  100 * (coef_2026 - coef_2022 - 1.96 * sqrt(se_2026^2 + se_2022^2)), 100 * (coef_2026 - coef_2022 + 1.96 * sqrt(se_2026^2 + se_2022^2))))]
cat("\n2026 - 2022:\n"); print(dd[termo %in% c("p_escol_sem_fund", "p_escol_superior"), .(desfecho, termo, dif_pp, ic95_pp)][order(termo, desfecho)])
dir.create(here::here("output", "tables", "comparacao"), showWarnings = FALSE)
fwrite(res, here::here("output", "tables", "comparacao", paste0(nome, "_nulos_escolaridade.csv")))

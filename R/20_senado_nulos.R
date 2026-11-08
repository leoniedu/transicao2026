# Step 20: blank and null Senate votes by presidential vote, round 1 (2022 and 2026). Two checks on the step-19 finding that
# Lula voters cast far more blank/null Senate votes than Bolsonaro voters:
# (A) LP estimates (step 19, CARGO=5) by whether the voter's party ran a Senate candidate in the UF (PT for Lula, PL for Bolsonaro);
# (B) seção regressions of the blank and null share of Senate votes on presidential vote shares (reference: other
#     candidates), with zona FE, voters as weights, SEs clustered by zona, with and without age and schooling controls,
#     and the Lula (Bolsonaro) share interacted with PT (PL) running for the Senate in the UF.
source(here::here("R", "functions.R"))
vagas <- c(`2022` = 1L, `2026` = 2L)[[as.character(ano)]]

cand <- fread(out_table("senado_candidatos.csv"))
n_part <- cand[, .(n_pt = sum(partido == 13L), n_pl = sum(partido == 22L)), by = uf]

# (A) LP estimates by UF, pooled by candidacy.
u <- merge(fread(out_table("senado_presidente_uf.csv"))[, !"n_pl"], n_part, by = "uf")
u[, `:=`(pt_concorre = n_pt > 0L, pl_concorre = n_pl > 0L, votos = vagas * eleitores)]
pool <- function(by) {
  u[presidente %in% c("lula", "bolsonaro"),
    .(ufs = .N, eleitores_m = sum(eleitores) / 1e6, branco = 100 * sum(branco_c) / sum(votos),
      nulo = 100 * sum(nulo_c) / sum(votos), pt = 100 * sum(pt) / sum(votos), pl = 100 * sum(pl) / sum(votos)),
    by = c("presidente", by)]
}
a <- rbind(pool("pt_concorre")[, `:=`(corte = "PT concorre", valor = pt_concorre)][, pt_concorre := NULL],
           pool("pl_concorre")[, `:=`(corte = "PL concorre", valor = pl_concorre)][, pl_concorre := NULL])
setcolorder(a, c("corte", "valor", "presidente"))
options(width = 200, digits = 3)
cat("(A) LP estimates, % of each group's Senate votes:\n"); print(a[order(corte, presidente, valor)])
fwrite(a, out_table("senado_nulos_lp.csv"))

# (B) seção regressions.
d <- read_proc("senado_secao")
d <- merge(d, n_part, by = "uf")
pf <- read_proc("perfil_secao")
ctrl <- c("p_escol_sem_fund", "p_escol_fundamental", "p_escol_superior", "p_escol_nao_informado",
          "p_idade_16_17", "p_idade_18_24", "p_idade_25_34", "p_idade_45_59", "p_idade_60_69", "p_idade_70_mais",
          "p_sexo_fem")
d <- merge(d, pf[, c("secao_id", ctrl, "p_idade_invalida"), with = FALSE], by = "secao_id")[p_idade_invalida < 0.01]
d[, `:=`(s_lula = lula / votantes, s_bolsonaro = bolsonaro / votantes, s_inval = inval / votantes,
         nulo = nulo_c / (vagas * votantes), branco = branco_c / (vagas * votantes),
         pt_c = as.integer(n_pt > 0L), pl_c = as.integer(n_pl > 0L))]
d[, `:=`(s_lula_pt = s_lula * pt_c, s_bolsonaro_pl = s_bolsonaro * pl_c)]
cat("\nseções:", nrow(d), " zonas:", uniqueN(d$zona), "\n")

base <- c("s_lula", "s_bolsonaro", "s_inval")
specs <- list(`sem controles` = base, `com controles` = c(base, ctrl),
              `interação` = c(base, c("s_lula_pt", "s_bolsonaro_pl")[c(uniqueN(d$pt_c), uniqueN(d$pl_c)) > 1L], ctrl))
b <- rbindlist(lapply(c("nulo", "branco"), function(y) rbindlist(lapply(names(specs), function(s)
  fe_ols(d, y, specs[[s]], w = "votantes")[, `:=`(desfecho = y, modelo = s)]))))
b[, `:=`(coef_pp = round(100 * coef, 2), ic95_pp = sprintf("[%.1f, %.1f]", 100 * (coef - 1.96 * se), 100 * (coef + 1.96 * se)))]
cat("\n(B) coefficients, pp of Senate votes for a seção entirely of that group vs other-candidate voters:\n")
print(b[termo %in% c(base, "s_lula_pt", "s_bolsonaro_pl", "p_escol_sem_fund", "p_escol_superior"),
        .(desfecho, modelo, termo, coef_pp, ic95_pp)][order(desfecho, modelo)])

# Lula - Bolsonaro gap with its SE needs the covariance; refit with the Bolsonaro share as reference instead.
gap <- rbindlist(lapply(c("nulo", "branco"), function(y) rbindlist(lapply(names(specs)[1:2], function(s) {
  x <- c("s_lula", "s_outros", "s_inval", if (s == "com controles") ctrl)
  d[, s_outros := 1 - s_lula - s_bolsonaro - s_inval]
  fe_ols(d, y, x, w = "votantes")[termo == "s_lula"][, `:=`(desfecho = y, modelo = s, termo = "lula - bolsonaro")]
}))))
gap[, `:=`(coef_pp = round(100 * coef, 2), ic95_pp = sprintf("[%.1f, %.1f]", 100 * (coef - 1.96 * se), 100 * (coef + 1.96 * se)))]
cat("\nLula - Bolsonaro gap:\n"); print(gap[, .(desfecho, modelo, coef_pp, ic95_pp)])
fwrite(rbind(b, gap), out_table("senado_nulos_regressao.csv"))

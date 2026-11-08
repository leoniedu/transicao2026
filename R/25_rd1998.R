# Step 25: paper ballot vs voting machine, 1998 round 1. Municipalities with more than 40,500 voters in 1996 voted on
# the machine, the rest on paper (Fujiwara 2015). Local-linear regression discontinuity (triangular kernel, HC1 SEs) on
# the 1996 electorate, per office: blank, null, blank + null and (deputies) party-only votes, as % of turnout. States
# that used the machine everywhere in 1998 (AL, AP, RJ, RR, DF) are left out.
source(here::here("R", "functions.R"))
raw98 <- here::here("data", "raw", "1998")
fetch_raw(sprintf("%s/detalhe_votacao_munzona/detalhe_votacao_munzona_1998.zip", tse_base), file.path(raw98, "detalhe_votacao_munzona_1998.zip"))
fetch_raw(sprintf("%s/detalhe_votacao_munzona/detalhe_votacao_munzona_1996.zip", tse_base), file.path(raw98, "detalhe_votacao_munzona_1996.zip"))
rd_read <- function(y) fread(cmd = sprintf("unzip -p %s detalhe_votacao_munzona_%d_BRASIL.csv",
                                           shQuote(file.path(raw98, sprintf("detalhe_votacao_munzona_%d.zip", y))), y),
                             encoding = "Latin-1", sep = ";")[NR_TURNO == 1]
d <- rd_read(1998)
m <- d[!SG_UF %in% c("AL", "AP", "RJ", "RR", "DF"),
       .(aptos = sum(QT_APTOS), comp = sum(QT_COMPARECIMENTO), br = sum(QT_VOTOS_BRANCOS), nu = sum(QT_TOTAL_VOTOS_NULOS),
         leg = sum(QT_TOTAL_VOTOS_LEG_VALIDOS)), by = .(uf = SG_UF, mun = as.integer(CD_MUNICIPIO), cargo = CD_CARGO)]
e96 <- rd_read(1996)[CD_CARGO == 11, .(x = sum(QT_APTOS) - 40500), by = .(mun = as.integer(CD_MUNICIPIO))]
cat("1998 municipalities matched to 1996:", round(m[cargo == 1, mean(mun %in% e96$mun)], 3), "\n")
m <- merge(m[cargo %in% c(1, 3, 5, 6, 7)], e96, by = "mun")
m[, `:=`(resid = 100 * (br + nu) / comp, nulo = 100 * nu / comp, branco = 100 * br / comp, legenda = 100 * leg / comp)]
cargos <- c(`1` = "Presidente", `3` = "Governador", `5` = "Senador", `6` = "Deputado federal", `7` = "Deputado estadual")
rd <- function(dt, y, h) {
  s <- dt[abs(x) <= h]; s[, `:=`(w = 1 - abs(x) / h, D = as.numeric(x >= 0))]
  f <- lm(as.formula(paste(y, "~ D * x")), data = s, weights = w)
  X <- model.matrix(f); e <- resid(f); W <- s$w; B <- solve(crossprod(X * W, X))
  V <- B %*% crossprod(X * (W * e)) %*% B * nrow(X) / (nrow(X) - ncol(X))
  data.table(efeito = coef(f)[["D"]], se = sqrt(V["D", "D"]), municipios = nrow(s), media_papel = s[D == 0, weighted.mean(get(y), w)])
}
res <- rbindlist(lapply(c(10000, 20000, 30000), function(h) rbindlist(lapply(names(cargos), function(cg)
  rbindlist(lapply(c("resid", "nulo", "branco", if (cg %in% c("6", "7")) "legenda"), function(y)
    rd(m[cargo == as.integer(cg)], y, h)[, `:=`(cargo = cargos[[cg]], desfecho = y, banda = h)]))))))
res[, `:=`(lo = efeito - 1.96 * se, hi = efeito + 1.96 * se)]
options(width = 200, digits = 3)
print(res[banda == 20000, .(cargo, desfecho, efeito, lo, hi, media_papel, municipios)])
cat("\nbandwidth robustness:\n"); print(dcast(res, cargo + desfecho ~ banda, value.var = "efeito"))
dir.create(here::here("output", "tables", "comparacao"), showWarnings = FALSE)
fwrite(res, here::here("output", "tables", "comparacao", "rd1998_urna.csv"))

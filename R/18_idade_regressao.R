# Step 18: seção-level regressions of the round-1 -> round-2 change on age composition (shares of 16-17 and 70+;
# reference 18-69), zona FE, weights aptos, SEs clustered by zona. (A) change score; (B) round-2 level controlling
# for the round-1 level. Under fairly general conditions the two bracket the effect (Angrist & Pischke 2009, 5.4).
# votos_m = coef x group size: change relative to the group behaving like 18-69, in millions.
source(here::here("R", "functions.R"))
fe_ols <- function(dt, y, x, cl = "zona", w = "aptos") {
  cols <- c(y, x)
  wv <- dt[[w]]; mu <- dt[, lapply(.SD, function(v) sum(v * wv[.I]) / sum(wv[.I])), by = cl, .SDcols = cols]
  m <- as.matrix(dt[, ..cols]) - as.matrix(mu[match(dt[[cl]], mu[[cl]]), ..cols])
  X <- m[, x, drop = FALSE]; Y <- m[, y]; W <- dt[[w]]
  XtWX <- crossprod(X * W, X); b <- solve(XtWX, crossprod(X * W, Y))[, 1]
  e <- Y - X %*% b
  S <- rowsum(X * as.vector(W * e), dt[[cl]])
  G <- nrow(S); n <- nrow(X); k <- ncol(X) + uniqueN(dt[[cl]])
  V <- solve(XtWX) %*% crossprod(S) %*% solve(XtWX) * G / (G - 1) * (n - 1) / (n - k)
  data.table(termo = x, coef = b, se = sqrt(diag(V)))
}
run_year <- function(yr) {
  p <- function(n) as.data.table(read_parquet(here::here("data", "processed", yr, paste0(n, ".parquet"))))
  t1 <- p("secao_t1"); t2 <- p("secao_t2"); pf <- p("perfil_idade_secao")
  d <- merge(t1[, .(secao_id, zona = paste(uf, nr_zona), aptos, m1 = (c13 - c22) / aptos, c1 = 1 - abstencao / aptos,
                    i1 = (branco + nulo) / aptos)],
             t2[, .(secao_id, m2 = (c13 - c22) / aptos, c2 = 1 - abstencao / aptos, i2 = (branco + nulo) / aptos)], by = "secao_id")
  d <- merge(d, pf, by = "secao_id")[aptos > 0 & invalida / perfil_n < 0.01]
  d[, `:=`(p16 = n16 / perfil_n, p70 = n70 / perfil_n)]
  d[, `:=`(dm = m2 - m1, dc = c2 - c1, di = i2 - i1)]
  n_g <- d[, .(`16-17` = sum(aptos * p16), `70+` = sum(aptos * p70))]
  out <- rbindlist(list(
    fe_ols(d, "dm", c("p16", "p70"))[, `:=`(desfecho = "margem Lula", modelo = "A: mudança")],
    fe_ols(d, "m2", c("p16", "p70", "m1"))[, `:=`(desfecho = "margem Lula", modelo = "B: controle 1º turno")],
    fe_ols(d, "dc", c("p16", "p70"))[, `:=`(desfecho = "comparecimento", modelo = "A: mudança")],
    fe_ols(d, "c2", c("p16", "p70", "c1"))[, `:=`(desfecho = "comparecimento", modelo = "B: controle 1º turno")],
    fe_ols(d, "di", c("p16", "p70"))[, `:=`(desfecho = "branco/nulo", modelo = "A: mudança")],
    fe_ols(d, "i2", c("p16", "p70", "i1"))[, `:=`(desfecho = "branco/nulo", modelo = "B: controle 1º turno")]))
  out[, ano := yr]
  # implied votes relative to 18-69 composition: coef x group size (millions)
  out[termo == "p16", votos_m := coef * n_g$`16-17` / 1e6][termo == "p70", votos_m := coef * n_g$`70+` / 1e6]
  out[, nat := c(dm = d[, weighted.mean(dm, aptos)], dc = d[, weighted.mean(dc, aptos)], di = d[, weighted.mean(di, aptos)])[
    fcase(desfecho == "margem Lula", "dm", desfecho == "comparecimento", "dc", default = "di")]]
  cat(yr, "seções:", nrow(d), " zonas:", uniqueN(d$zona), "\n")
  out[]
}
res <- rbind(run_year(2022L), run_year(2026L))
res[, `:=`(coef_pp = round(100 * coef, 2), ic95_pp = sprintf("[%.1f, %.1f]", 100 * (coef - 1.96 * se), 100 * (coef + 1.96 * se)))]
options(width = 200)
print(res[termo != "m1" & termo != "c1" & termo != "i1",
          .(ano, desfecho, modelo, termo, coef_pp, ic95_pp, votos_m = round(votos_m, 2), mudanca_media_pp = round(100 * nat, 2))][order(desfecho, termo, ano, modelo)])
print(res[termo %in% c("m1", "c1", "i1"), .(ano, desfecho, termo, coef = round(coef, 3))])
dir.create(here::here("output", "tables", "comparacao"), showWarnings = FALSE)
fwrite(res[, .(ano, desfecho, modelo, termo, coef, se, votos_m, mudanca_media = nat)],
       here::here("output", "tables", "comparacao", "regressao_idade.csv"))

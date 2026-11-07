# Step 17: voluntary voters (16-17, 70+) vs 18-69, rounds 1 and 2 of 2022 and 2026, in votes (step 16 estimates).
# DF is left out of both years because the 2022 perfil_comparecimento_abstencao file has no DF.
source(here::here("R", "functions.R"))

read_lp <- function(y, tn) {
  f <- here::here("data", "processed", y, sprintf("lp_idade_t%d.parquet", tn))
  if (file.exists(f)) as.data.table(read_parquet(f))[, `:=`(ano = y, turno = tn)][]
}
a <- rbindlist(lapply(c(2022L, 2026L), function(y) rbindlist(lapply(1:2, read_lp, y = y))))[uf != "DF"]
a[destino == "bolsonaro", destino := "direita"]
w <- dcast(a, idade + ano + turno ~ destino, value.var = "votos", fun.aggregate = sum, fill = 0)
w[, `:=`(eleitorado = abstencao + invalido + lula + direita + outros, votaram = invalido + lula + direita + outros)]
w[, `:=`(abst_pct = 100 * abstencao / eleitorado, saldo_lula = lula - direita)]
tab <- w[, .(idade, ano, turno, abst_pct, eleitorado, votaram, lula, direita, outros, invalido, saldo_lula)]
for (j in c("eleitorado", "votaram", "lula", "direita", "outros", "invalido", "saldo_lula")) set(tab, j = j, value = tab[[j]] / 1e6)
options(width = 200, digits = 3)
print(tab)
tot <- w[, .(facultativo = sum(saldo_lula[idade != "18-69"]) / 1e6, obrigatorio = sum(saldo_lula[idade == "18-69"]) / 1e6,
             total = sum(saldo_lula) / 1e6), by = .(ano, turno)]
print(tot)
dir.create(here::here("output", "tables", "comparacao"), showWarnings = FALSE)
fwrite(tab, here::here("output", "tables", "comparacao", "voto_idade_milhoes.csv"))
fwrite(tot, here::here("output", "tables", "comparacao", "saldo_idade_milhoes.csv"))

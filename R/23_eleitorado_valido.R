# Step 23: who casts the valid votes, president vs another office on the same ballot (CARGO=5 Senate, 6 federal
# deputy), round 1 of 2022 and 2026, from the step-19 LP estimates: blank/null votes for the office by presidential
# vote, and the composition of the valid electorate by presidential vote (votes for the office per seat, so a two-seat
# Senate year counts in voter-equivalents).
source(here::here("R", "functions.R"))
cargo <- as.integer(Sys.getenv("CARGO", "5"))
nome <- c(`5` = "senado", `6` = "deputado")[[as.character(cargo)]]
vagas <- if (cargo == 5L) c(`2022` = 1, `2026` = 2) else c(`2022` = 1, `2026` = 1)
rp <- function(y, n) as.data.table(arrow::read_parquet(here::here("data", "processed", y, paste0(n, ".parquet"))))

out <- rbindlist(lapply(names(vagas), function(y) {
  v <- vagas[[y]]
  lp <- dcast(rp(y, paste0("lp_", nome)), presidente ~ voto, value.var = "votos", fun.aggregate = sum)
  partes <- setdiff(names(lp), c("presidente", "branco_c", "nulo_c", "sem_voto"))
  lp[, validos := rowSums(.SD), .SDcols = partes]
  lp[, eleitores := (validos + branco_c + nulo_c + sem_voto) / v]
  lp[, `:=`(ano = as.integer(y), perda_pct = 100 * (branco_c + nulo_c) / (v * eleitores),
            pct_dos_nulos = 100 * nulo_c / sum(nulo_c), pct_dos_brancos = 100 * branco_c / sum(branco_c))]
  val <- lp[presidente != "inval"]
  val[, `:=`(pct_valido_pres = 100 * eleitores / sum(eleitores), pct_valido_cargo = 100 * validos / sum(validos))]
  merge(lp, val[, .(presidente, pct_valido_pres, pct_valido_cargo)], by = "presidente", all.x = TRUE)
}), fill = TRUE)
options(width = 200, digits = 3)
print(out[, .(ano, presidente, eleitores_m = eleitores / 1e6, nulo_m = nulo_c / 1e6, branco_m = branco_c / 1e6,
              perda_pct, pct_dos_nulos, pct_valido_pres, pct_valido_cargo)][order(ano, presidente)])
gap <- out[presidente %in% c("lula", "bolsonaro"), .(pres = diff(pct_valido_pres[order(presidente)]),
                                                     cargo = diff(pct_valido_cargo[order(presidente)])), by = ano]
cat("Lula minus bolsonaro voters, share of valid votes (pp):\n"); print(gap)
dir.create(here::here("output", "tables", "comparacao"), showWarnings = FALSE)
fwrite(out, here::here("output", "tables", "comparacao", paste0(nome, "_eleitorado_valido.csv")))

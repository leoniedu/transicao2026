# Step 8: compare presidential runoffs across years by role (government-side finalist vs opposition), national
# per-zona LP estimates: row-standardised matrix, in/out shares, net change decomposition.
source(here::here("R", "functions.R"))

years <- as.integer(names(governo))
years <- years[file.exists(here::here("output", "tables", years, "swing.csv"))]
read_year <- function(y, name) as.data.table(arrow::read_parquet(here::here("data", "processed", y, paste0(name, ".parquet"))))[, ano := y][]
csv_year <- function(y, name) fread(here::here("output", "tables", y, name))[, ano := y][]
papel <- function(x, y) fifelse(x == governo[as.character(y)], "governo",
                         fifelse(x %in% names(finalistas[[as.character(y)]]), "oposicao", x))

est <- rbindlist(lapply(years, read_year, name = "lp_agregado"))[geo == "BR" & spec == "A"]
est[, `:=`(row = papel(row, ano), col = papel(col, ano)), by = ano]
io <- rbindlist(lapply(years, csv_year, name = "inout.csv"))[geo == "BR"]
io[, `:=`(finalista = papel(finalista, ano), contraparte = papel(contraparte, ano)), by = ano]
sw <- rbindlist(lapply(years, csv_year, name = "swing.csv"))[geo == "BR"]
sw[, `:=`(finalista = papel(finalista, ano), contraparte = papel(contraparte, ano)), by = ano]

dir.create(here::here("output", "tables", "comparacao"), showWarnings = FALSE)
fwrite(est, here::here("output", "tables", "comparacao", "matriz_BR.csv"))
fwrite(io, here::here("output", "tables", "comparacao", "inout_BR.csv"))
fwrite(sw, here::here("output", "tables", "comparacao", "swing_BR.csv"))

pct <- function(d) d[, (as.character(years)) := lapply(.SD, function(x) round(100 * x, 1)), .SDcols = as.character(years)][]
for (m in c("tslphom", "lphom")) {
  cat(sprintf("\n==== %s: row-standardised transitions (%%), by year ====\n", m))
  print(pct(dcast(est[method == m], row + col ~ ano, value.var = "p")))
  cat(sprintf("\n%s: net change decomposition (million votes)\n", m))
  d <- dcast(sw[method == m], finalista + contraparte ~ ano, value.var = "liquido")
  print(d[, (as.character(years)) := lapply(.SD, function(x) round(x / 1e6, 2)), .SDcols = as.character(years)][])
  cat(sprintf("\n%s: total change and government-side edge (million votes)\n", m))
  t <- sw[method == m, .(delta = delta_obs[1]), by = .(ano, finalista)]
  print(dcast(t, ano ~ finalista, value.var = "delta")[, `:=`(governo = round(governo / 1e6, 2), oposicao = round(oposicao / 1e6, 2),
                                                              vantagem_governo = round((governo - oposicao) / 1e6, 2))][])
}

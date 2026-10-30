# Step 7: in/out flows for the two finalists (labels from `finalistas` in functions.R) from the per-zona LP counts,
# and the decomposition of each finalist's change between rounds into net exchanges with every other category.
source(here::here("R", "functions.R"))

est <- read_proc("lp_agregado")[spec == "A" & method %in% c("lphom", "tslphom")]
fin <- names(fin_ano)

inout <- function(e) {
  rbindlist(lapply(fin, function(f) {
    out <- e[row == f, .(finalista = f, fluxo = "out", contraparte = col, votos)]
    inn <- e[col == f, .(finalista = f, fluxo = "in", contraparte = row, votos)]
    rbind(out, inn)[, share := votos / sum(votos), by = fluxo]
  }))
}

by_geo <- function(fun) est[, fun(.SD), by = .(method, geo)]
io <- by_geo(inout)
sw <- by_geo(function(e) swing_tab(e, fin))
fwrite(io, out_table("inout.csv"))
fwrite(sw, out_table("swing.csv"))

for (m in unique(est$method)) {
  cat(sprintf("\n==== %d, %s (per-zona LP), national ====\n", ano, m))
  for (f in fin) {
    cat(sprintf("\n%s - OUT: where round-1 voters went (%%)\n", f))
    print(io[method == m & geo == "BR" & finalista == f & fluxo == "out", .(contraparte, votos = round(votos), pct = round(100 * share, 1))])
    cat(sprintf("%s - IN: origin of round-2 votes (%%)\n", f))
    print(io[method == m & geo == "BR" & finalista == f & fluxo == "in", .(contraparte, votos = round(votos), pct = round(100 * share, 1))])
  }
  cat("\nnet change decomposition (votes):\n")
  print(sw[method == m & geo == "BR", .(finalista, contraparte, ganho = round(ganho), perda = round(perda),
                                        liquido = round(liquido), delta_obs)])
}

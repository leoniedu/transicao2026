# Step 14: official turnout by age (perfil_comparecimento_abstencao), aggregated to zona x age band x round.
# In-country UFs only (no ZZ). The 2022 file has no DF; the 2026 file (21 Oct 2026) has round 1 only.
source(here::here("R", "functions.R"))

zip <- raw_dir("perfil_comp", sprintf("perfil_comparecimento_abstencao_%d.zip", ano))
fetch_raw(sprintf("%s/perfil_comparecimento_abstencao/perfil_comparecimento_abstencao_%d.zip", tse_base, ano), zip)
members <- grep(sprintf("_%d_[A-Z]{2}\\.csv$", ano), zip_members(zip), value = TRUE)
members <- members[!grepl("_ZZ\\.csv$", members)]
cat("UFs in file:", length(members), " missing:", setdiff(ufs_br, sub(".*_([A-Z]{2})\\.csv$", "\\1", members)), "\n")

cols <- c("NR_TURNO", "SG_UF", "NR_ZONA", "DS_FAIXA_ETARIA", "QT_APTOS", "QT_ABSTENCAO")
comp <- rbindlist(lapply(members, function(m) {
  read_tse_zip(zip, m, cols, cargo = NULL)[, .(aptos = sum(QT_APTOS), abstencao = sum(QT_ABSTENCAO)),
                                           by = .(turno = NR_TURNO, uf = SG_UF, nr_zona = NR_ZONA, DS_FAIXA_ETARIA)]
}))
comp[, idade_ini := faixa_inicio(DS_FAIXA_ETARIA)][, DS_FAIXA_ETARIA := NULL]
comp <- comp[, .(aptos = sum(aptos), abstencao = sum(abstencao)), by = .(turno, uf, nr_zona, idade_ini)]
cat("rounds:", sort(unique(comp$turno)), " invalid age:", comp[is.na(idade_ini), sum(aptos)], "\n")
write_proc(comp, "comparecimento_idade")

nat <- comp[!is.na(idade_ini), .(aptos = sum(aptos), abstencao = sum(abstencao)), by = .(turno, idade = idade5(idade_ini))]
nat <- rbind(nat, comp[!is.na(idade_ini), .(idade = "total", aptos = sum(aptos), abstencao = sum(abstencao)), by = turno])
nat[, abst_pct := round(100 * abstencao / aptos, 2)]
print(nat[order(turno, idade)])
fwrite(nat[order(turno, idade)], out_table("abstencao_idade.csv"))

# Round 1 -> round 2 by zona (when both rounds are in the file).
if (all(1:2 %in% comp$turno)) {
  z <- dcast(comp[!is.na(idade_ini), .(r = sum(abstencao) / sum(aptos), n = sum(aptos)),
                  by = .(turno, zona = paste(uf, nr_zona), idade = idade5(idade_ini))],
             zona + idade ~ turno, value.var = c("r", "n"))[, d := r_2 - r_1]
  rel <- z[, .(zonas = .N, abst_t1 = 100 * weighted.mean(r_1, n_1), abst_t2 = 100 * weighted.mean(r_2, n_1),
               delta_pp = 100 * weighted.mean(d, n_1), cor = cov.wt(cbind(r_1, r_2), n_1, cor = TRUE)$cor[1, 2],
               inclinacao = coef(lm(r_2 ~ r_1, weights = n_1))[[2]],
               dp_delta_pp = 100 * sqrt(weighted.mean((d - weighted.mean(d, n_1))^2, n_1))), by = idade][order(idade)]
  print(rel)
  fwrite(rel, out_table("abstencao_idade_t1_t2.csv"))
}

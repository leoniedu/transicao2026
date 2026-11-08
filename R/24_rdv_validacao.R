# Step 24: what was typed in the Senate null votes (RDV, 2026 round 1, random sample of seções of one UF; files from
# py/rdv_fetch.py, table from py/rdv_secao.py) and whether party-only federal deputy votes track the Senate
# "party number + guessed digit" nulls: seção regressions with zona FE (and schooling/age/sex controls), and
# party-specific regressions given the party's deputy vote. UF=BA by default.
source(here::here("R", "functions.R"))
stopifnot(ano == 2026L)
uf_rdv <- Sys.getenv("UF", "BA")
dir_rdv <- raw_dir("rdv")
x <- fread(file.path(dir_rdv, paste0(tolower(uf_rdv), "_secao_partido.csv")), colClasses = list(character = c("mu", "zo", "se", "partido")))
x[, `:=`(secao_id = secao_pack(as.integer(mu), as.integer(zo), as.integer(se)), zona = paste(uf_rdv, as.integer(zo)))]
s <- unique(x[, .(secao_id, zona, sen_votos, eleitores, sen_zeros, sen_rep, sen_nulo, sen_branco, sen_valido, gov_nulo,
                  pres_nulo, depf_leg, depe_leg, depe_leg_sen)])
s <- merge(s, x[, .(sen_pref = sum(sen_pref)), by = secao_id], by = "secao_id")

# Composition of Senate votes in the sample.
comp <- s[, .(secoes = .N, votos = sum(sen_votos), valido = sum(sen_valido), branco = sum(sen_branco),
              nulo_partido_digito = sum(sen_pref), nulo_zeros = sum(sen_zeros), nulo_repeticao = sum(sen_rep),
              nulo_outro = sum(sen_nulo) - sum(sen_pref) - sum(sen_zeros),
              dep_est_legenda_com_numero_senador = sum(depe_leg_sen), eleitores = sum(eleitores))]
cat("Senate votes in the sample (% of Senate votes):\n")
print(comp[, lapply(.SD, function(v) round(100 * v / votos, 2)), .SDcols = c("valido", "branco", "nulo_partido_digito", "nulo_zeros", "nulo_repeticao", "nulo_outro")])
fwrite(comp, out_table(paste0("rdv_", tolower(uf_rdv), "_senado_composicao.csv")))

# (A) seção level: outcome shares on the party-only federal deputy share, zona FE.
pf <- read_proc("perfil_secao")
ctrl <- c("p_escol_sem_fund", "p_escol_fundamental", "p_escol_superior", "p_idade_16_17", "p_idade_18_24", "p_idade_25_34",
          "p_idade_45_59", "p_idade_60_69", "p_idade_70_mais", "p_sexo_fem")
s <- merge(s, pf[, c("secao_id", ctrl), with = FALSE], by = "secao_id")
s[, `:=`(leg = depf_leg / eleitores, pref = sen_pref / sen_votos, zeros = sen_zeros / sen_votos, rep = sen_rep / sen_votos,
         cross = depe_leg_sen / eleitores, gov = gov_nulo / eleitores, pres = pres_nulo / eleitores)]
cat("seções:", nrow(s), " zonas:", uniqueN(s$zona), " raw correlation (party-only, party+digit):", round(s[, cor(leg, pref)], 3), "\n")
A <- rbindlist(lapply(c("pref", "rep", "zeros", "cross", "gov", "pres"), function(y) rbind(
  fe_ols(s, y, "leg", w = "eleitores")[, `:=`(desfecho = y, controles = FALSE)],
  fe_ols(s, y, c("leg", ctrl), w = "eleitores")[termo == "leg"][, `:=`(desfecho = y, controles = TRUE)])))

# (B) party-specific: the party's Senate "party + digit" nulls on its party-only deputy share, given its deputy vote.
p <- x[, .(secao_id, zona, partido, sen_pref, depf_leg_p, depf_p, sen_votos, eleitores)]
p <- p[partido %in% p[, .(n = sum(depf_p)), by = partido][n > 5000, partido]]
p[, `:=`(y = sen_pref / sen_votos, leg_p = depf_leg_p / eleitores, sup_p = depf_p / eleitores, id = as.character(secao_id))]
for (q in unique(p$partido)) set(p, j = paste0("d", q), value = as.numeric(p$partido == q))
dums <- grep("^d[0-9]+$", names(p), value = TRUE)[-1]
B <- rbind(fe_ols(p, "y", c("leg_p", "sup_p", dums), cl = "id", w = "eleitores")[termo == "leg_p"][, desfecho := "partido+dígito, empilhado (FE seção)"],
           rbindlist(lapply(c("13", "44", "22"), function(q)
             fe_ols(p[partido == q], "y", c("leg_p", "sup_p"), w = "eleitores")[termo == "leg_p"][, desfecho := paste0("partido+dígito, partido ", q)])),
           fill = TRUE)
res <- rbind(A, B, fill = TRUE)[, `:=`(lo = coef - 1.96 * se, hi = coef + 1.96 * se)]
options(width = 200, digits = 3)
print(res[termo %in% c("leg", "leg_p"), .(desfecho, controles, coef, lo, hi)])
fwrite(res, out_table(paste0("rdv_", tolower(uf_rdv), "_validacao.csv")))

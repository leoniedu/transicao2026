# Figure: Lula's/Dilma's net advantage among round-1 non-choosers (abstention + branco/nulo) above what the PT
# candidate's round-1 share predicts, by município, Bahia vs the rest of Brazil, 2014/2022/2026 (per-zona tslphom).
source(here::here("R", "functions.R"))
library(ggplot2)

nc <- c("invalido", "abstencao")
# Região Metropolitana de Salvador (13 municípios).
rms <- c("SALVADOR", "LAURO DE FREITAS", "SIMÕES FILHO", "CANDEIAS", "CAMAÇARI", "DIAS D'ÁVILA", "MADRE DE DEUS",
         "SÃO FRANCISCO DO CONDE", "VERA CRUZ", "ITAPARICA", "MATA DE SÃO JOÃO", "POJUCA", "SÃO SEBASTIÃO DO PASSÉ")

mun_ano <- function(y) {
  ano <<- y
  f <- finalistas[[as.character(y)]]
  pt <- names(f)[f == "c13"]; op <- names(f)[f != "c13"]
  e <- read_proc("lp_municipio")[method == "tslphom"]
  m <- e[, .(aptos = sum(votos),
             vant = sum(votos[row %in% nc & col == pt]) - sum(votos[row == pt & col %in% nc]) -
               (sum(votos[row %in% nc & col == op]) - sum(votos[row == op & col %in% nc])),
             pt1 = sum(votos[row == pt])), by = .(uf, cd_municipio)]
  m[, `:=`(vant_pp = 100 * vant / aptos, pt1_pp = 100 * pt1 / aptos)]
  ajuste <- lm(vant_pp ~ pt1_pp, m, weights = aptos)
  m[, resid_pp := resid(ajuste)]
  zip_l <- raw_dir(sprintf("eleitorado_local_votacao_%d.zip", y))
  nm <- unique(read_tse_zip(zip_l, grep("(BRASIL|_[0-9]{4})\\.csv$", zip_members(zip_l), value = TRUE),
                            c("CD_MUNICIPIO", "NM_MUNICIPIO"), cargo = NULL))
  setnames(nm, c("cd_municipio", "nome"))
  nm[, cd_municipio := as.integer(cd_municipio)]
  assert(!anyDuplicated(nm$cd_municipio), sprintf("one name per município (%d)", y))
  m <- merge(m, nm, by = "cd_municipio")
  assert(m[uf == "BA" & nome %in% rms, .N] == length(rms), sprintf("all RMS municípios found (%d)", y))
  cat(sprintf("%d: vant_pp = %.2f + %.3f * pt1_pp (R2 %.2f)\n", y, coef(ajuste)[1], coef(ajuste)[2], summary(ajuste)$r.squared))
  m[, ano := y][]
}
m <- rbindlist(lapply(c(2014L, 2022L, 2026L), mun_ano))
m[, grupo := factor(fifelse(uf == "BA", "Bahia", "Resto do Brasil"), levels = c("Resto do Brasil", "Bahia"))]
m[, painel := factor(ano, levels = c(2014, 2022, 2026),
                     labels = c("2014 · Dilma (PT) no governo", "2022 · Bolsonaro no governo", "2026 · Lula (PT) no governo"))]
dir.create(here::here("output", "tables", "comparacao"), showWarnings = FALSE)
fwrite(m, here::here("output", "tables", "comparacao", "residuo_municipio.csv"))
print(m[, .(rms = round(weighted.mean(resid_pp[uf == "BA" & nome %in% rms], aptos[uf == "BA" & nome %in% rms]), 1),
            bahia_resto = round(weighted.mean(resid_pp[uf == "BA" & !nome %in% rms], aptos[uf == "BA" & !nome %in% rms]), 1),
            brasil_resto = round(weighted.mean(resid_pp[uf != "BA"], aptos[uf != "BA"]), 1)), by = ano])

nome_bonito <- function(x) gsub(" (De|Do|Da|Dos|Das) ", " \\L\\1 ", tools::toTitleCase(tolower(x)), perl = TRUE)
rot <- m[uf == "BA" & nome %in% c("SALVADOR", "LAURO DE FREITAS", "FEIRA DE SANTANA")][, nome := nome_bonito(nome)]

cores <- c(Bahia = "#eb6834", `Resto do Brasil` = "#8a8984")
ink <- "#0b0b0b"; ink2 <- "#52514e"; grid <- "#e4e3df"; surface <- "#fcfcfb"

p <- ggplot(m[order(grupo)], aes(aptos, resid_pp)) +
  geom_hline(yintercept = 0, colour = ink2, linewidth = 0.4) +
  geom_point(aes(size = aptos, fill = grupo), shape = 21, colour = surface, stroke = 0.2, alpha = 0.45) +
  geom_smooth(aes(weight = aptos, group = grupo), method = "loess", span = 0.8, se = FALSE, colour = surface, linewidth = 2.4) +
  geom_smooth(aes(weight = aptos, colour = grupo), method = "loess", span = 0.8, se = FALSE, linewidth = 1.1) +
  ggrepel::geom_text_repel(data = rot, aes(label = nome), size = 2.8, colour = ink, min.segment.length = 0,
                           segment.colour = ink2, segment.size = 0.25, box.padding = 0.5, nudge_y = 1.5, seed = 1) +
  facet_wrap(~painel, nrow = 1) +
  scale_x_log10(name = "eleitores aptos no município (escala log)",
                labels = scales::label_number(big.mark = ".", decimal.mark = ",", scale_cut = scales::cut_short_scale())) +
  scale_y_continuous(name = "vantagem do PT acima do esperado (pontos percentuais do eleitorado)",
                     labels = scales::label_number(decimal.mark = ",", style_positive = "plus")) +
  coord_cartesian(ylim = c(-10, 10)) +
  scale_fill_manual(values = cores, breaks = names(cores), name = NULL, aesthetics = c("fill", "colour")) +
  scale_size(range = c(0.4, 7), trans = "sqrt", guide = "none") +
  guides(fill = guide_legend(override.aes = list(size = 3.5, alpha = 1, linewidth = 1.1))) +
  labs(title = "A Grande Salvador ficou muito acima do esperado em 2014 e 2026, com o PT no governo, mas não em 2022",
       subtitle = paste0("Vantagem do candidato do PT (Dilma em 2014, Lula em 2022 e 2026) entre quem se absteve ou votou branco/nulo ",
                         "no 1º turno, menos a esperada\npela votação dele no 1º turno no município (regressão ponderada pelo ",
                         "eleitorado, ajustada a cada ano). Cada ponto é um município; as linhas são a média local\n",
                         "da Bahia e do resto do país, ponderadas pelo eleitorado."),
       caption = paste0("Inferência ecológica (tslphom, uma matriz por zona eleitoral, seções como unidades), dados do TSE. ",
                        "Estimativas por município são somas das estimativas por seção e são ruidosas em municípios pequenos.\n",
                        "Eixo vertical cortado em ±10 pontos. Exclui exterior, voto em trânsito e seções com eleitorado diferente ",
                        "entre os turnos (>1%). Análise e gráfico: Claude (Anthropic).")) +
  theme_minimal(base_size = 11) +
  theme(plot.background = element_rect(fill = surface, colour = NA),
        panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = grid, linewidth = 0.3),
        strip.text = element_text(face = "bold", size = 11, hjust = 0, colour = ink),
        axis.text = element_text(colour = ink2), axis.title = element_text(colour = ink2, size = 9.5),
        plot.title = element_text(face = "bold", size = 14, colour = ink),
        plot.subtitle = element_text(colour = ink2, size = 10.5, margin = margin(b = 10)),
        plot.caption = element_text(colour = ink2, size = 8, hjust = 0),
        plot.title.position = "plot", plot.caption.position = "plot",
        legend.position = "top", legend.justification = "left", legend.text = element_text(colour = ink),
        panel.spacing.x = unit(1.6, "lines"), plot.margin = margin(14, 20, 10, 14))

dir.create(here::here("output", "figures"), showWarnings = FALSE)
ggsave(here::here("output", "figures", "bahia_residuo_2014_2022_2026.png"), p, width = 14, height = 6.5, dpi = 200, bg = surface)

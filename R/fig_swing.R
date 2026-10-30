# Figure: net between-round gain by source, government-side finalist vs opposition, presidential runoffs with the
# incumbent running (per-zona tslphom, national).
source(here::here("R", "functions.R"))
library(ggplot2)

nomes <- list(`2014` = c(governo = "Dilma", oposicao = "Aécio"),
              `2022` = c(governo = "Bolsonaro", oposicao = "Lula"),
              `2026` = c(governo = "Lula", oposicao = "Flávio"))
sw <- fread(here::here("output", "tables", "comparacao", "swing_BR.csv"))[method == "tslphom"]
sw[, fonte := fcase(contraparte == "outros", "Eleitores de outros\ncandidatos",
                    contraparte %in% c("invalido", "abstencao"), "Não escolheram candidato\n(abstenção, branco, nulo)",
                    default = "Troca direta entre\nos finalistas")]
d <- sw[, .(liq = sum(liquido) / 1e6), by = .(ano, finalista, fonte)]
d <- rbind(d, sw[, .(fonte = "Ganho total", liq = sum(liquido) / 1e6), by = .(ano, finalista)][, .(ano, finalista, fonte, liq)])
d[, fonte := factor(fonte, levels = rev(c("Eleitores de outros\ncandidatos", "Não escolheram candidato\n(abstenção, branco, nulo)",
                                          "Troca direta entre\nos finalistas", "Ganho total")))]
d[, nome := mapply(function(a, f) nomes[[as.character(a)]][[f]], ano, finalista)]
d[, finalista := factor(finalista, levels = c("oposicao", "governo"), labels = c("Oposição", "Governo"))]
d[, painel := factor(ano, levels = c(2014, 2022, 2026),
                     labels = sprintf("%d · %s no governo", c(2014, 2022, 2026), c("Dilma", "Bolsonaro", "Lula")))]
d[, rotulo := paste0(sub(".", ",", sprintf("%s%.1f", fifelse(liq > 0.05, "+", fifelse(liq < -0.05, "−", "")), abs(liq)),
                         fixed = TRUE), " ", nome)]

cores <- c(Governo = "#2a78d6", Oposição = "#eb6834")
ink <- "#0b0b0b"; ink2 <- "#52514e"; grid <- "#e4e3df"; surface <- "#fcfcfb"

p <- ggplot(d, aes(x = liq, y = fonte, fill = finalista)) +
  geom_vline(xintercept = 0, colour = ink2, linewidth = 0.4) +
  geom_col(position = position_dodge(width = 0.78), width = 0.7, colour = surface, linewidth = 0.6) +
  annotate("segment", x = -2.4, xend = 19.5, y = 1.5, yend = 1.5, colour = grid, linewidth = 0.5) +
  geom_text(aes(label = rotulo, hjust = fifelse(liq >= -0.3, -0.08, 1.08)),
            position = position_dodge(width = 0.78), size = 3.1, colour = ink) +
  facet_wrap(~painel, nrow = 1) +
  scale_fill_manual(values = cores, breaks = c("Governo", "Oposição"), name = NULL) +
  scale_x_continuous(name = "milhões de votos (saldo líquido entre os turnos)",
                     limits = c(-2.4, 19.5), breaks = seq(0, 15, 5), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(y = NULL,
       title = "Em 2026, a vantagem do governo entre quem não tinha escolhido candidato foi de três a seis vezes a de 2014 e 2022",
       subtitle = paste0("De onde veio o ganho de votos de cada finalista do 1º para o 2º turno, nos três segundos turnos com o presidente ",
                         "candidato à reeleição.\nEntre quem não tinha escolhido candidato, Lula teve +6,2 milhões contra +0,5 de Flávio. ",
                         "Os eleitores dos outros candidatos se dividiram ao meio."),
       caption = paste0("Inferência ecológica (tslphom, uma matriz por zona eleitoral, seções como unidades), dados do TSE. ",
                        "Saldos líquidos estimados, não comportamento individual.\n",
                        "Exclui exterior, voto em trânsito e seções com eleitorado diferente entre os turnos (>1%). ",
                        "Análise e gráfico: Claude (Anthropic).")) +
  theme_minimal(base_size = 11) +
  theme(plot.background = element_rect(fill = surface, colour = NA),
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = grid, linewidth = 0.3),
        strip.text = element_text(face = "bold", size = 11, hjust = 0, colour = ink),
        axis.text = element_text(colour = ink2), axis.title.x = element_text(colour = ink2, size = 9.5),
        plot.title = element_text(face = "bold", size = 14, colour = ink),
        plot.subtitle = element_text(colour = ink2, size = 10.5, margin = margin(b = 10)),
        plot.caption = element_text(colour = ink2, size = 8, hjust = 0),
        plot.title.position = "plot", plot.caption.position = "plot",
        legend.position = "top", legend.justification = "left", legend.text = element_text(colour = ink),
        panel.spacing.x = unit(2.2, "lines"), plot.margin = margin(14, 30, 10, 14))

dir.create(here::here("output", "figures"), showWarnings = FALSE)
ggsave(here::here("output", "figures", "swing_2014_2022_2026.png"), p, width = 13.5, height = 6, dpi = 200, bg = surface)

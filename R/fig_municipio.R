# Figures: municipality electorate vs Lula's net advantage among round-1 non-choosers (abstention + branco/nulo),
# in pp of the electorate, raw and net of Lula's round-1 share (2026, per-zona tslphom unit estimates summed by município).
source(here::here("R", "functions.R"))
library(ggplot2)

nc <- c("invalido", "abstencao")
e <- read_proc("lp_municipio")[method == "tslphom"]
m <- e[, .(aptos = sum(votos),
           lula = sum(votos[row %in% nc & col == "lula"]) - sum(votos[row == "lula" & col %in% nc]),
           flavio = sum(votos[row %in% nc & col == "bolsonaro"]) - sum(votos[row == "bolsonaro" & col %in% nc]),
           lula1 = sum(votos[row == "lula"])),
       by = .(uf, cd_municipio)]
m[, `:=`(vant_pp = 100 * (lula - flavio) / aptos, lula1_pp = 100 * lula1 / aptos)]
# Expected advantage given Lula's round-1 share of the electorate (linear, weighted by aptos); residual in pp.
ajuste <- lm(vant_pp ~ lula1_pp, m, weights = aptos)
m[, resid_pp := resid(ajuste)]
cat(sprintf("vant_pp = %.2f + %.3f * lula1_pp (R2 %.2f)\n", coef(ajuste)[1], coef(ajuste)[2], summary(ajuste)$r.squared))
cat(sprintf("municípios: %d; vantagem nacional: %.2f milhões\n", nrow(m), m[, sum(lula - flavio)] / 1e6))

zip_l <- raw_dir(sprintf("eleitorado_local_votacao_%d.zip", ano))
nm <- unique(read_tse_zip(zip_l, grep("(BRASIL|_[0-9]{4})\\.csv$", zip_members(zip_l), value = TRUE),
                          c("CD_MUNICIPIO", "NM_MUNICIPIO"), cargo = NULL))
setnames(nm, c("cd_municipio", "nome"))
nm[, cd_municipio := as.integer(cd_municipio)]
assert(!anyDuplicated(nm$cd_municipio), "one name per município")
m <- merge(m, nm, by = "cd_municipio", all.x = TRUE)
assert(!anyNA(m$nome), "every município has a name")

regiao <- c(AC = "Norte", AM = "Norte", AP = "Norte", PA = "Norte", RO = "Norte", RR = "Norte", TO = "Norte",
            AL = "Nordeste", BA = "Nordeste", CE = "Nordeste", MA = "Nordeste", PB = "Nordeste", PE = "Nordeste",
            PI = "Nordeste", RN = "Nordeste", SE = "Nordeste", DF = "Centro-Oeste", GO = "Centro-Oeste",
            MS = "Centro-Oeste", MT = "Centro-Oeste", ES = "Sudeste", MG = "Sudeste", RJ = "Sudeste", SP = "Sudeste",
            PR = "Sul", RS = "Sul", SC = "Sul")
assert(all(m$uf %in% names(regiao)), "every UF has a region")
m[, regiao := factor(regiao[uf], levels = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"))]
m[, bahia := factor(fifelse(uf == "BA", "Bahia", "Resto do Brasil"), levels = c("Resto do Brasil", "Bahia"))]
fwrite(m, out_table("vantagem_municipio.csv"))

cores_regiao <- c(Norte = "#2a78d6", Nordeste = "#eb6834", `Centro-Oeste` = "#1baf7a", Sudeste = "#eda100", Sul = "#e87ba4")
ink <- "#0b0b0b"; ink2 <- "#52514e"; grid <- "#e4e3df"; surface <- "#fcfcfb"
cores_bahia <- c(Bahia = "#eb6834", `Resto do Brasil` = "#8a8984")
nome_bonito <- function(x) gsub(" (De|Do|Da|Dos|Das) ", " \\L\\1 ", tools::toTitleCase(tolower(x)), perl = TRUE)

fig <- function(y, ylab, title, subtitle, rot, ref = NULL, grupo = "regiao", cores = cores_regiao, media_br = TRUE) {
  rot <- copy(rot)[, nome := nome_bonito(nome)]
  ggplot(m[order(m[[grupo]])], aes(aptos, .data[[y]])) +
    geom_hline(yintercept = 0, colour = ink2, linewidth = 0.4) +
    (if (!is.null(ref)) geom_hline(yintercept = ref, colour = ink2, linewidth = 0.3, linetype = "22")) +
    geom_point(aes(size = aptos, fill = .data[[grupo]]), shape = 21, colour = surface, stroke = 0.25, alpha = 0.45) +
    geom_smooth(aes(weight = aptos, group = .data[[grupo]]), method = "loess", span = 0.8, se = FALSE, colour = surface, linewidth = 2.4) +
    geom_smooth(aes(weight = aptos, colour = .data[[grupo]]), method = "loess", span = 0.8, se = FALSE, linewidth = 1.1) +
    (if (media_br) geom_smooth(aes(weight = aptos), method = "loess", span = 0.6, se = FALSE, colour = ink, linewidth = 0.7, linetype = "42")) +
    ggrepel::geom_text_repel(data = rot, aes(label = nome), size = 2.9, colour = ink, min.segment.length = 0,
                             segment.colour = ink2, segment.size = 0.25, box.padding = 0.5, seed = 1) +
    (if (!is.null(ref)) annotate("text", x = 1.2e3, y = ref, label = sprintf("Brasil: %s pp", sub(".", ",", sprintf("%.1f", ref), fixed = TRUE)),
                                 hjust = 0, vjust = -0.5, size = 3, colour = ink2)) +
    scale_x_log10(name = "eleitores aptos no município (escala log)",
                  labels = scales::label_number(big.mark = ".", decimal.mark = ",", scale_cut = scales::cut_short_scale())) +
    scale_y_continuous(name = ylab, labels = scales::label_number(decimal.mark = ",", style_positive = "plus")) +
    scale_fill_manual(values = cores, breaks = names(cores), name = NULL, aesthetics = c("fill", "colour")) +
    scale_size(range = c(0.6, 9), trans = "sqrt", guide = "none") +
    guides(fill = guide_legend(override.aes = list(size = 3.5, alpha = 1, linewidth = 1.1))) +
    labs(title = title, subtitle = subtitle,
         caption = paste0("Inferência ecológica (tslphom, uma matriz por zona eleitoral, seções como unidades), dados do TSE. ",
                          "Estimativas por município são somas das estimativas por seção e são ruidosas em municípios pequenos.\n",
                          "Exclui exterior, voto em trânsito e seções com eleitorado diferente entre os turnos (>1%). ",
                          "Análise e gráfico: Claude (Anthropic).")) +
    theme_minimal(base_size = 11) +
    theme(plot.background = element_rect(fill = surface, colour = NA),
          panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = grid, linewidth = 0.3),
          axis.text = element_text(colour = ink2), axis.title = element_text(colour = ink2, size = 9.5),
          plot.title = element_text(face = "bold", size = 14, colour = ink),
          plot.subtitle = element_text(colour = ink2, size = 10.5, margin = margin(b = 10)),
          plot.caption = element_text(colour = ink2, size = 8, hjust = 0),
          plot.title.position = "plot", plot.caption.position = "plot",
          legend.position = "top", legend.justification = "left", legend.text = element_text(colour = ink),
          plot.margin = margin(14, 20, 10, 14))
}

linhas <- "\nCada ponto é um município; as linhas coloridas são a média local de cada região e a tracejada, a do Brasil, ponderadas pelo eleitorado."
p <- fig("vant_pp", "vantagem de Lula, em pontos percentuais do eleitorado",
         "Lula ganhou entre quem não tinha escolhido candidato em municípios de todos os tamanhos",
         paste0("Saldo de Lula menos saldo de Flávio entre os eleitores que se abstiveram ou votaram branco/nulo ",
                "no 1º turno, em % do eleitorado do município, 2026.", linhas),
         m[order(-aptos)][1:10], ref = 100 * m[, sum(lula - flavio) / sum(aptos)])
coef_txt <- sub(".", ",", sprintf("%.2f", coef(ajuste)[2]), fixed = TRUE)
p_res <- fig("resid_pp", "vantagem de Lula acima do esperado, em pontos percentuais do eleitorado",
             "Descontada a força de Lula no 1º turno, as cidades grandes da Bahia ficam muito acima do resto do país",
             paste0("Vantagem de Lula entre quem se absteve ou votou branco/nulo no 1º turno, menos a esperada pela votação de Lula ",
                    "no 1º turno no município\n(cada ponto do eleitorado com Lula no 1º turno vale +", coef_txt,
                    " pp; regressão ponderada pelo eleitorado), 2026.\n",
                    "Cada ponto é um município; as linhas são a média local da Bahia e do resto do país, ponderadas pelo eleitorado."),
             unique(rbind(m[order(-aptos)][1:6], m[uf == "BA" & aptos > 5e4][order(-resid_pp)][1:6],
                          m[uf == "BA"][order(-aptos)][1:3])),
             grupo = "bahia", cores = cores_bahia, media_br = FALSE)

dir.create(here::here("output", "figures"), showWarnings = FALSE)
ggsave(here::here("output", "figures", "vantagem_municipio_2026.png"), p, width = 11, height = 7, dpi = 200, bg = surface)
ggsave(here::here("output", "figures", "vantagem_municipio_residuo_2026.png"), p_res, width = 11, height = 7, dpi = 200, bg = surface)

library(dplyr)
library(tidyr)
library(ggplot2)

if (!exists("data") || !is.data.frame(data)) stop("Objeto 'data' não está na sessão: rode o modelo final antes.")
if (!exists("modelo_final"))                 stop("Objeto 'modelo_final' não está na sessão: rode o modelo final antes.")

SAIDA     <- "figuras/"
dir.create(SAIDA, showWarnings = FALSE, recursive = TRUE)

# As 15 variáveis climáticas do modelo final
vars_clima <- c(
  "precip_sum_veg", "dry_days_veg", "dry_spell_max_veg", "wbal_veg",
  "precip_sum_rep", "dry_days_rep", "dry_spell_max_rep", "wbal_rep",
  "wdemand_days_rep", "vpd_high_days_rep", "vpd_p90_rep", "tmax_p90_rep",
  "precip_sum_fill", "wbal_fill", "wdemand_days_fill"
)

dados <- data

faltam <- setdiff(c("Solo", "Classe_Produtividade", vars_clima), names(dados))
if (length(faltam) > 0) stop("Colunas não encontradas na base: ", paste(faltam, collapse = ", "))

dados <- dados %>%
  mutate(
    # aceita "Média" ou "Media" e deixa sem acento (o radar procura "Media")
    Classe_Produtividade = as.character(Classe_Produtividade),
    Classe_Produtividade = ifelse(grepl("^M", Classe_Produtividade), "Media", Classe_Produtividade),
    Classe_Produtividade = factor(Classe_Produtividade, levels = c("Baixa", "Media", "Alta"))
  )
if (any(is.na(dados$Classe_Produtividade))) stop("Há classes diferentes de Baixa/Media/Alta na base.")

cat("Parcelas por solo e classe:\n")
print(table(dados$Solo, dados$Classe_Produtividade))

dados_heatmap <- dados %>%
  select(Solo, Classe_Produtividade, all_of(vars_clima)) %>%
  pivot_longer(all_of(vars_clima), names_to = "Variavel", values_to = "Valor") %>%
  group_by(Solo, Variavel) %>%
  mutate(
    desvio = sd(Valor, na.rm = TRUE),
    # se a variável tem o mesmo valor em todas as parcelas do solo (desvio = 0),
    # não dá para padronizar: fica NA ("sem variação")
    z = if_else(desvio > 0, (Valor - mean(Valor, na.rm = TRUE)) / desvio, NA_real_)
  ) %>%
  group_by(Solo, Classe_Produtividade, Variavel) %>%
  summarise(Media = if (all(is.na(z))) NA_real_ else mean(z, na.rm = TRUE),
            n = n(), .groups = "drop")

# Variáveis sem variação dentro de algum solo (aparecem cinza no heatmap)
sem_var <- dados_heatmap %>% filter(is.na(Media)) %>% distinct(Solo, Variavel)
if (nrow(sem_var) > 0) {
  cat("\nSem variação dentro do solo (célula cinza no heatmap):\n"); print(sem_var)
}

# Conferência: dentro de cada solo, a média das classes ponderada pelo n
# tem que dar 0 (porque o z-score foi centrado no solo).
# As variáveis sem variação ficam de fora da conferência.
conf <- dados_heatmap %>%
  filter(!is.na(Media)) %>%
  group_by(Solo, Variavel) %>%
  summarise(media_ponderada = sum(Media * n) / sum(n), .groups = "drop")
stopifnot(all(abs(conf$media_ponderada) < 1e-8))
cat("\nConferência OK: em cada solo a média ponderada das classes é 0.\n")


# ORDEM DAS VARIÁVEIS (importância no modelo final)
cat("Modelo na sessão:", modelo_final$splitrule, "| num.trees:", modelo_final$num.trees,
    "| mtry:", modelo_final$mtry, "\n")
imp   <- modelo_final$variable.importance
ordem <- names(sort(imp[vars_clima], decreasing = FALSE))     # mais importante por último = em cima
dados_heatmap$Variavel <- factor(dados_heatmap$Variavel, levels = ordem)

lim <- ceiling(max(abs(dados_heatmap$Media), na.rm = TRUE) * 10) / 10   # escala simétrica

dados_heatmap <- dados_heatmap %>%
  mutate(rotulo = if_else(is.na(Media), "sem variação", sprintf("%.2f", Media)))

g <- ggplot(dados_heatmap, aes(x = Classe_Produtividade, y = Variavel, fill = Media)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = rotulo), size = 3.3, color = "grey15") +
  facet_wrap(~ Solo) +
  scale_fill_gradient2(low = "#c0392b", mid = "grey92", high = "#2471a3",
                       midpoint = 0, limits = c(-lim, lim), na.value = "grey75",
                       name = "Média\npadronizada") +
  labs(title = "Perfil médio das variáveis climáticas por classe de produtividade",
       x = "Classe de produtividade", y = "Variável climática") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    axis.text       = element_text(face = "bold", color = "grey30"),
    axis.title      = element_text(face = "bold"),
    strip.text      = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "grey90", color = "black"),
    panel.border    = element_rect(fill = NA, color = "black"),
    panel.grid      = element_blank(),
    legend.position = "top"
  )

print(g)
ggsave(paste0(SAIDA, "HEATMAP_IMPORTANCIA_CLIMA.png"), g,
       width = 13, height = 9, dpi = 300, bg = "white")
cat("Figura salva em", paste0(SAIDA, "HEATMAP_IMPORTANCIA_CLIMA.png"), "\n")

# dados_heatmap fica na sessão: agora é só rodar o script do RADAR.

####################################################################################
library(fmsb)
library(dplyr)

dir.create("figuras", showWarnings = FALSE)


# FUNCTION — Generate a radar chart for a soil type (in English)

plotar_radar <- function(tipo_solo, dados_heatmap) {
  
  # Translate soil type display name
  solo_en <- recode(tipo_solo, "Latossolo" = "Latosol", "Plintossolo" = "Plinthosol")
  
  radar_wide <- dados_heatmap %>%
    filter(Solo == tipo_solo) %>%
    mutate(
      # Recode yield classes to English
      Classe_Produtividade = recode(Classe_Produtividade,
                                    "Baixa" = "Low",
                                    "Media" = "Medium",
                                    "Alta"  = "High"),
      # Recode climate variables to English for the radar axis labels (com quebra de linha \n)
      Variavel = recode(Variavel,
                        "precip_sum_veg"      = "Precipitation\n(veg)",
                        "precip_sum_rep"      = "Precipitation\n(rep)",
                        "precip_sum_fill"     = "Precipitation\n(fill)",
                        "dry_days_veg"        = "Dry days\n(veg)",
                        "dry_days_rep"        = "Dry days\n(rep)",
                        "dry_spell_max_veg"   = "Max dry spell\n(veg)",
                        "dry_spell_max_rep"   = "Max dry spell\n(rep)",
                        "wbal_veg"            = "Water balance\n(veg)",
                        "wbal_rep"            = "Water balance\n(rep)",
                        "wbal_fill"           = "Water balance\n(fill)",
                        "wdemand_days_rep"    = "Water demand\ndays (rep)",
                        "wdemand_days_fill"   = "Water demand\ndays (fill)",
                        "vpd_high_days_rep"   = "High VPD\ndays (rep)",
                        "vpd_p90_rep"         = "VPD P90\n(rep)",
                        "tmax_p90_rep"        = "Max temp.\nP90 (rep)"
      )
    ) %>%
    select(Classe_Produtividade, Variavel, Media) %>%
    tidyr::pivot_wider(
      names_from  = Variavel,
      values_from = Media
    ) %>%
    tibble::column_to_rownames("Classe_Produtividade")
  
  # Ensure exact row ordering
  radar_wide <- radar_wide[c("Low", "Medium", "High"), ]
  
  max_val <- 2.0
  min_val <- -2.0
  
  radar_data <- rbind(
    rep(max_val, ncol(radar_wide)),
    rep(min_val, ncol(radar_wide)),
    radar_wide
  )
  
  cores <- c(
    "#d73027",  # Low — red
    "#fdae61",  # Medium — orange
    "#4575b4"   # High — blue
  )
  
  radarchart(
    radar_data,
    axistype    = 1,
    caxislabels = c("-2", "-1", "0", "1", "2"),  # z-score scale
    
    # Polygon styling
    pcol  = cores,
    pfcol = scales::alpha(cores, 0.15),
    plwd  = 2.5,
    plty  = c(1, 2, 3),   # solid, dashed, dotted
    
    # Grid styling
    cglcol = "grey70",
    cglty  = 1,
    cglwd  = 0.8,
    
    # Axis styling
    axislabcol = "grey40",
    vlcex      = 0.72,
    
    # Title
    title = paste0("Climate profile — ", solo_en)
  )
  
  legend(
    x        = "bottomright",
    legend   = c("Low", "Medium", "High"),
    col      = cores,
    lty      = c(1, 2, 3),
    lwd      = 2.5,
    pch      = 20,
    bty      = "n",
    text.col = "grey20",
    cex      = 0.85
  )
}


# INDIVIDUAL RADAR PLOTS

for (tipo_solo in c("Latossolo", "Plintossolo")) {
  nome_arquivo <- paste0("figuras/RADAR_", toupper(tipo_solo), ".png")
  png(nome_arquivo, width = 900, height = 850, res = 150, bg = "white")
  par(mar = c(2, 2, 3, 2))
  plotar_radar(tipo_solo, dados_heatmap)
  dev.off()
  cat("Radar saved:", nome_arquivo, "\n")
}


# COMBINED FIGURE — Latosol | Plinthosol side by side


png("figuras/RADAR_AMBOS_SOLOS.png",
    width = 1800, height = 900, res = 150, bg = "white")

par(mfrow = c(1, 2),           # 1 row, 2 columns
    mar   = c(2, 2, 4, 2),     # inner margins
    oma   = c(3, 0, 3, 0))     # outer margins for main title

plotar_radar("Latossolo",   dados_heatmap)
plotar_radar("Plintossolo", dados_heatmap)

# Main title
mtext(
  "Climate profile by yield class and soil type — Tocantins (2017–2024)",
  outer = TRUE, side = 3, line = 0.5,
  font = 2, cex = 1.1, col = "#20364C"
)

# Footer / Subtitle
mtext(
  "",
  outer = TRUE, side = 1, line = 1,
  font = 3, cex = 0.75, col = "grey50"
)

dev.off()
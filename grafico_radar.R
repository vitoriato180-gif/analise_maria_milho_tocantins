# =============================================================================
# 37b — PERFIL CLIMÁTICO POR CLASSE: MÉDIA DO EXPERIMENTO (SEM SEPARAR POR SOLO)
#
# Pedido do Leonardo (09/11): o gráfico de teia (radar) deve ser para a média
# do experimento, e não por tipo de solo. O heatmap de perfil segue a mesma lógica.
#
# A ÚNICA mudança em relação ao 37: o "Solo" saiu dos dois group_by.
#   - group_by(Variavel): padroniza cada variável com TODAS as 1434 parcelas
#        z = (valor - média do experimento) / desvio-padrão do experimento
#   - group_by(Classe_Produtividade, Variavel): média do z-score de cada classe
#     usando todas as parcelas daquela classe
#
# Saídas: o heatmap (um painel só) e o radar (um gráfico só), em inglês.
# Como ler: valor positivo = a classe teve valores ACIMA da média do experimento;
#           negativo = ABAIXO. A cor não diz se é bom ou ruim para a planta.
#
# ATENÇÃO: rode DEPOIS do modelo final, com "data" e "modelo_final" na sessão.
# Não use rm(list = ls()) aqui, senão esses objetos são apagados.
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(fmsb)

if (!exists("data") || !is.data.frame(data)) stop("Objeto 'data' não está na sessão: rode o modelo final antes.")
if (!exists("modelo_final"))                 stop("Objeto 'modelo_final' não está na sessão: rode o modelo final antes.")

SAIDA <- "figuras/"
dir.create(SAIDA, showWarnings = FALSE, recursive = TRUE)

# As 15 variáveis climáticas do modelo final
vars_clima <- c(
  "precip_sum_veg", "dry_days_veg", "dry_spell_max_veg", "wbal_veg",
  "precip_sum_rep", "dry_days_rep", "dry_spell_max_rep", "wbal_rep",
  "wdemand_days_rep", "vpd_high_days_rep", "vpd_p90_rep", "tmax_p90_rep",
  "precip_sum_fill", "wbal_fill", "wdemand_days_fill"
)

# Nomes em inglês para o gráfico
nome_var <- c(
  "precip_sum_veg"    = "Precipitation (veg)",
  "precip_sum_rep"    = "Precipitation (rep)",
  "precip_sum_fill"   = "Precipitation (fill)",
  "dry_days_veg"      = "Dry days (veg)",
  "dry_days_rep"      = "Dry days (rep)",
  "dry_spell_max_veg" = "Max dry spell (veg)",
  "dry_spell_max_rep" = "Max dry spell (rep)",
  "wbal_veg"          = "Water balance (veg)",
  "wbal_rep"          = "Water balance (rep)",
  "wbal_fill"         = "Water balance (fill)",
  "wdemand_days_rep"  = "Water demand days (rep)",
  "wdemand_days_fill" = "Water demand days (fill)",
  "vpd_high_days_rep" = "High VPD days (rep)",
  "vpd_p90_rep"       = "VPD P90 (rep)",
  "tmax_p90_rep"      = "Max temp. P90 (rep)"
)
nome_classe <- c("Baixa" = "Low", "Media" = "Medium", "Alta" = "High")

# -----------------------------------------------------------------------------
# 1. BASE
# -----------------------------------------------------------------------------
dados <- data

faltam <- setdiff(c("Classe_Produtividade", vars_clima), names(dados))
if (length(faltam) > 0) stop("Colunas não encontradas na base: ", paste(faltam, collapse = ", "))

dados <- dados %>%
  mutate(
    # aceita "Média" ou "Media" e deixa sem acento
    Classe_Produtividade = as.character(Classe_Produtividade),
    Classe_Produtividade = ifelse(grepl("^M", Classe_Produtividade), "Media", Classe_Produtividade),
    Classe_Produtividade = factor(Classe_Produtividade, levels = c("Baixa", "Media", "Alta"))
  )
if (any(is.na(dados$Classe_Produtividade))) stop("Há classes diferentes de Baixa/Media/Alta na base.")

cat("Parcelas por classe (experimento todo):\n")
print(table(dados$Classe_Produtividade))
cat("Total:", nrow(dados), "parcelas\n")

# -----------------------------------------------------------------------------
# 2. Z-SCORE COM TODAS AS PARCELAS E MÉDIA POR CLASSE
# -----------------------------------------------------------------------------
dados_heatmap <- dados %>%
  select(Classe_Produtividade, all_of(vars_clima)) %>%
  pivot_longer(all_of(vars_clima), names_to = "Variavel", values_to = "Valor") %>%
  group_by(Variavel) %>%                                   # antes: group_by(Solo, Variavel)
  mutate(
    desvio = sd(Valor, na.rm = TRUE),
    # se a variável tivesse o mesmo valor em todas as parcelas, ficaria NA
    z = if_else(desvio > 0, (Valor - mean(Valor, na.rm = TRUE)) / desvio, NA_real_)
  ) %>%
  group_by(Classe_Produtividade, Variavel) %>%             # antes: group_by(Solo, Classe_Produtividade, Variavel)
  summarise(Media = if (all(is.na(z))) NA_real_ else mean(z, na.rm = TRUE),
            n = n(), .groups = "drop")

sem_var <- dados_heatmap %>% filter(is.na(Media)) %>% distinct(Variavel)
if (nrow(sem_var) > 0) { cat("\nSem variação no experimento:\n"); print(sem_var) }

# Conferência: a média das 3 classes ponderada pelo n tem que dar 0
# (porque o z-score foi centrado na média do experimento)
conf <- dados_heatmap %>%
  filter(!is.na(Media)) %>%
  group_by(Variavel) %>%
  summarise(media_ponderada = sum(Media * n) / sum(n), .groups = "drop")
stopifnot(all(abs(conf$media_ponderada) < 1e-8))
cat("\nConferência OK: a média ponderada das classes é 0 em todas as variáveis.\n")

# -----------------------------------------------------------------------------
# 3. ORDEM DAS VARIÁVEIS (importância no modelo final)
# -----------------------------------------------------------------------------
cat("Modelo na sessão:", modelo_final$splitrule, "| num.trees:", modelo_final$num.trees,
    "| mtry:", modelo_final$mtry, "\n")
imp   <- modelo_final$variable.importance
ordem <- names(sort(imp[vars_clima], decreasing = FALSE))     # mais importante por último = em cima

# -----------------------------------------------------------------------------
# 4. HEATMAP (um painel só, em inglês)
# -----------------------------------------------------------------------------
df_heat <- dados_heatmap %>%
  mutate(Variavel = factor(Variavel, levels = ordem),
         Classe   = factor(nome_classe[as.character(Classe_Produtividade)],
                           levels = c("Low", "Medium", "High")),
         rotulo   = if_else(is.na(Media), "no variation", sprintf("%.2f", Media)))

lim <- ceiling(max(abs(df_heat$Media), na.rm = TRUE) * 10) / 10   # escala simétrica

g <- ggplot(df_heat, aes(x = Classe, y = Variavel, fill = Media)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = rotulo), size = 3.3, color = "grey15") +
  scale_y_discrete(labels = nome_var) +
  scale_fill_gradient2(low = "#c0392b", mid = "grey92", high = "#2471a3",
                       midpoint = 0, limits = c(-lim, lim), na.value = "grey75",
                       name = "Standardized\nmean") +
  labs(title = "Mean climate profile by yield class",
       x = "Yield class", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    axis.text       = element_text(face = "bold", color = "grey30"),
    axis.title      = element_text(face = "bold"),
    panel.border    = element_rect(fill = NA, color = "black"),
    panel.grid      = element_blank(),
    legend.position = "top"
  )

print(g)
ggsave(paste0(SAIDA, "HEATMAP_PERFIL_CLIMA_GERAL.png"), g,
       width = 8, height = 9, dpi = 300, bg = "white")
cat("Figura salva em", paste0(SAIDA, "HEATMAP_PERFIL_CLIMA_GERAL.png"), "\n")

# -----------------------------------------------------------------------------
# 5. RADAR (um gráfico só, em inglês)
# -----------------------------------------------------------------------------
radar_wide <- dados_heatmap %>%
  mutate(Classe   = nome_classe[as.character(Classe_Produtividade)],
         Variavel = nome_var[Variavel]) %>%
  select(Classe, Variavel, Media) %>%
  pivot_wider(names_from = Variavel, values_from = Media) %>%
  tibble::column_to_rownames("Classe")
radar_wide <- radar_wide[c("Low", "Medium", "High"), ]

# Eixos agrupados por fase (veg -> rep -> fill), na ordem de vars_clima
radar_wide <- radar_wide[, unname(nome_var[vars_clima])]
# Rótulos em duas linhas: nome em cima, fase embaixo (evita sobreposição)
colnames(radar_wide) <- sub(" \\(", "\n(", colnames(radar_wide))

# Escala: os valores ficam perto de ±0,5; ±1 deixa as diferenças visíveis
max_val <- 1.0
min_val <- -1.0
if (any(abs(as.matrix(radar_wide)) > max_val, na.rm = TRUE))
  warning("Algum valor passa de ±1: aumente max_val/min_val para o polígono não sair da escala.")

radar_data <- rbind(
  rep(max_val, ncol(radar_wide)),
  rep(min_val, ncol(radar_wide)),
  radar_wide
)

cores <- c("#d73027",  # Low — red
           "#fdae61",  # Medium — orange
           "#4575b4")  # High — blue

png(paste0(SAIDA, "RADAR_GERAL.png"), width = 1500, height = 1400, res = 150, bg = "white")
par(mar = c(1, 4, 3, 4), xpd = TRUE)   # margens laterais maiores: rótulos não são cortados
radarchart(
  radar_data,
  axistype    = 1,
  caxislabels = c("-1", "-0.5", "0", "0.5", "1"),   # escala do z-score
  pcol  = cores,
  pfcol = scales::alpha(cores, 0.15),
  plwd  = 2.5,
  plty  = c(1, 2, 3),
  cglcol = "grey70", cglty = 1, cglwd = 0.8,
  axislabcol = "grey40",
  vlcex      = 0.68,
  calcex     = 0.7,
  title = "Climate profile by yield class"
)
legend(x = "bottomright", legend = c("Low", "Medium", "High"),
       col = cores, lty = c(1, 2, 3), lwd = 2.5, pch = 20,
       bty = "n", text.col = "grey20", cex = 0.85)
dev.off()
cat("Radar salvo em", paste0(SAIDA, "RADAR_GERAL.png"), "\n")
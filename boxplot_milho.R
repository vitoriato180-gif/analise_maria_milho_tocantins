###########################################################################################
### box plot

library(dplyr)
library(ggplot2)

dados_box <- data %>%
  filter(!is.na(kgha)) %>%
  mutate(Local = factor(Local, levels = c("Aparecida do Rio Negro", 
                                          "Paraíso do Tocantins", 
                                          "Pedro Afonso", 
                                          "Pium", 
                                          "Porto Nacional")))

# BOXPLOT (Sem a cor preta)
boxplot_prod_to <- ggplot(dados_box, aes(x = Local, y = kgha, fill = Local)) +
  # Trocamos o 'color = "black"' por '"gray40"' para suavizar as bordas
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, alpha = 0.8, color = "gray40") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "",
    subtitle = "",
    x     = "municipality",
    y     = "Observed yield (kg/ha)"
  ) +
  theme_bw() +
  theme(
    # Textos em tons de cinza escuro e azul acinzentado em vez de preto puro
    plot.title   = element_text(face = "bold", size = 14, hjust = 0.5, color = "#2C3E50"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#7F8C8D"),
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 11, face = "bold", color = "#4D4D4D"),
    axis.text.y  = element_text(size = 11, face = "bold", color = "#4D4D4D"),
    axis.title   = element_text(face = "bold", size = 12, color = "#2C3E50"),
    legend.position = "none",
    
    # Suavizando também as bordas do painel
    panel.border = element_rect(color = "gray70", linewidth = 0.8)
  )

print(boxplot_prod_to)

if(!dir.exists("figuras")) {
  dir.create("figuras")
}
ggsave("figuras/BOXPLOT_PRODUTIVIDADE_TO.png", boxplot_prod_to,
       width = 10, height = 6, dpi = 300)
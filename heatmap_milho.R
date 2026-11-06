# HEATMAP POR SOLO (LATOSSOLO × PLINTOSSOLO)
############ O GRAFICO QUE IREMOS USAR ESTÁ MAIS EM BAIXO  #########################

library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(lubridate)

setwd("/Users/mariavitoriadiastorres/Downloads")
TBASE <- 10
source("funcoes_clima.R")
dir.create("figuras", showWarnings = FALSE)

ARQ_DADOS  <- "/Users/mariavitoriadiastorres/Downloads/dados_modelo_final.xlsx"
ANOS_CLIMA <- 2017:2024
ATRASOS    <- c(0, 10, 20, 30)
MODA_SOLO  <- c("Latossolo" = "02-24", "Plintossolo" = "03-03")
ARQ_CACHE  <- paste0("figuras/clima_heatmap_moda_", min(ANOS_CLIMA), "_", max(ANOS_CLIMA), ".csv")
PYTHON     <- "/usr/local/bin/python3"

VARS <- c(
  "precip_sum_veg", "dry_days_veg", "dry_spell_max_veg", "wbal_veg",
  "precip_sum_rep", "dry_days_rep", "dry_spell_max_rep", "wbal_rep",
  "wdemand_days_rep", "vpd_high_days_rep", "vpd_p90_rep", "tmax_p90_rep",
  "precip_sum_fill", "wbal_fill", "wdemand_days_fill"
)

unidade <- function(v) case_when(
  grepl("^precip|^wbal", v) ~ "mm",
  grepl("days|spell", v)    ~ "days",
  grepl("^vpd_p90", v)      ~ "kPa",
  grepl("^tmax_p90", v)     ~ "°C",
  TRUE                      ~ ""
)
casas <- function(v) ifelse(grepl("^vpd_p90", v), 2, ifelse(grepl("^tmax_p90", v), 1, 0))

VARS_AUMENTO_RUIM <- c("dry_days_veg", "dry_spell_max_veg", "dry_days_rep", "dry_spell_max_rep",
                       "wdemand_days_rep", "vpd_high_days_rep", "vpd_p90_rep", "tmax_p90_rep",
                       "wdemand_days_fill")

fase_de <- function(v) factor(case_when(
  grepl("_veg$",  v) ~ "Vegetative",
  grepl("_rep$",  v) ~ "Reproductive",
  grepl("_fill$", v) ~ "Grain filling"
), levels = c("Vegetative", "Reproductive", "Grain filling"))

dados <- read_excel(ARQ_DADOS) %>%
  mutate(Plantio  = as.Date(Plantio),
         Colheita = as.Date(Colheita),
         Ano_num  = as.numeric(as.character(Ano)),
         ciclo    = as.integer(Colheita - Plantio)) %>%
  filter(!is.na(Plantio), !is.na(Colheita), !is.na(Local), !is.na(Solo))

melhor_ano <- dados %>%
  group_by(Local, Solo, Ano_num) %>%
  summarise(kgha_medio = mean(kgha), .groups = "drop") %>%
  group_by(Local, Solo) %>%
  slice_max(kgha_medio, n = 1, with_ties = FALSE) %>%
  ungroup()

opcoes <- dados %>%
  semi_join(melhor_ano, by = c("Local", "Solo", "Ano_num")) %>%
  count(Local, Solo, Ano_num, LATITUDE, LONGITUDE, Plantio, ciclo, name = "n_parcelas")

locais <- opcoes %>%
  group_by(Local, Solo) %>%
  slice_max(n_parcelas, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Solo, Local, LATITUDE, LONGITUDE, Ano_ref_parcelas = Ano_num, ciclo)

cenarios <- locais %>%
  crossing(Ano_clima = ANOS_CLIMA) %>%
  mutate(plantio_ref = as.Date(paste0(Ano_clima, "-", MODA_SOLO[Solo])))

chaves <- c("Solo", "Local", "LATITUDE", "LONGITUDE", "Ano_clima", "ciclo", "Dias_Atraso")

usar_cache <- FALSE
if (file.exists(ARQ_CACHE)) {
  clima <- read.csv(ARQ_CACHE) %>% mutate(plantio_ref = as.Date(plantio_ref))
  faltando <- anti_join(cenarios %>% crossing(Dias_Atraso = ATRASOS), clima, by = chaves)
  usar_cache <- nrow(faltando) == 0
}

if (!usar_cache) {
  library(reticulate)
  if (!py_available(initialize = FALSE)) use_python(PYTHON, required = TRUE)
  ee <- import("ee")
  ee$Initialize(project = "projeto-pesquisa-ufg")
  
  clima <- list()
  for (i in seq_len(nrow(cenarios))) {
    s <- cenarios[i, ]
    ser <- baixar_series_safra(s$LATITUDE, s$LONGITUDE,
                               s$plantio_ref, s$plantio_ref + s$ciclo, max(ATRASOS))
    if (any(sapply(ser, is.null))) stop("Empty series: ", s$Local, " ", s$Ano_clima)
    
    for (d in ATRASOS) {
      clima[[length(clima) + 1]] <-
        calcular_clima_cenario(ser$sp, ser$stx, ser$stn, s$LATITUDE,
                               s$plantio_ref + d, s$plantio_ref + d + s$ciclo) %>%
        mutate(Solo = s$Solo, Local = s$Local, LATITUDE = s$LATITUDE, LONGITUDE = s$LONGITUDE,
               Ano_clima = s$Ano_clima, plantio_ref = s$plantio_ref, ciclo = s$ciclo,
               Dias_Atraso = d, .before = 1)
    }
  }
  clima <- bind_rows(clima)
  write.csv(clima, ARQ_CACHE, row.names = FALSE)
}

long <- clima %>%
  select(Solo, Local, Ano_clima, Dias_Atraso, all_of(VARS)) %>%
  pivot_longer(all_of(VARS), names_to = "var", values_to = "valor")

ref <- long %>%
  filter(Dias_Atraso == 0) %>%
  select(Solo, Local, Ano_clima, var, x0 = valor)

dif <- long %>%
  filter(Dias_Atraso > 0) %>%
  left_join(ref, by = c("Solo", "Local", "Ano_clima", "var")) %>%
  mutate(d = valor - x0)

resumo_ref <- ref %>%
  group_by(Solo, var) %>%
  summarise(media = mean(x0), dp = sd(x0), n = n(), .groups = "drop")

resumo_dif <- dif %>%
  group_by(Solo, var, Dias_Atraso) %>%
  summarise(media = mean(d), dp = sd(d), minimo = min(d), maximo = max(d),
            n = n(), .groups = "drop")

tabela <- bind_rows(
  resumo_ref %>% mutate(Cenario = "Sowing date"),
  resumo_dif %>% mutate(Cenario = paste0("+", Dias_Atraso, " days"))
) %>%
  mutate(unidade = unidade(var)) %>%
  select(Solo, var, unidade, Cenario, media, dp, minimo, maximo, n)

write.csv(tabela, "figuras/24v2_tabela_heatmap_moda.csv", row.names = FALSE)

rotulo_ref  <- c("Latossolo" = "Feb 24\n(Sowing date)", "Plintossolo" = "Mar 03\n(Sowing date)")
niveis_cen  <- c(rotulo_ref, "+10 days", "+20 days", "+30 days")
ordem_vars  <- rev(VARS)
rotulo_var  <- setNames(paste0(VARS, " (", unidade(VARS), ")"), VARS)

fmt <- function(x, v, sinal) {
  k <- as.integer(casas(v))
  x <- ifelse(abs(x) < 0.5 * 10^(-k), 0, x)
  s <- sprintf(if (sinal) "%+.*f" else "%.*f", k, x)
  gsub(".", ",", s, fixed = TRUE)
}

MIN_RELEVANTE <- c("mm" = 5, "days" = 1, "kPa" = 0.05, "°C" = 0.5)
LIM_DP        <- 3
COR_REDUCAO   <- "red4"   
COR_AUMENTO   <- "green4"   

df_ref <- resumo_ref %>%
  mutate(Cenario = factor(rotulo_ref[Solo], levels = niveis_cen),
         rotulo  = paste0(fmt(media, var, FALSE), "\n(", fmt(dp, var, FALSE), ")"))

df_dif <- resumo_dif %>%
  left_join(resumo_ref %>% select(Solo, var, dp_ref = dp), by = c("Solo", "var")) %>%
  mutate(Cenario   = factor(paste0("+", Dias_Atraso, " days"), levels = niveis_cen),
         relevante = abs(media) >= MIN_RELEVANTE[unidade(var)],
         cor       = ifelse(relevante & dp_ref > 0, pmax(pmin(media / dp_ref, LIM_DP), -LIM_DP), 0),
         cor_txt   = ifelse(abs(cor) > 2, "white", "black"),
         rotulo    = paste0(fmt(media, var, TRUE), "\n(", fmt(dp, var, FALSE), ")"))

prep <- function(df) df %>% mutate(Fase = fase_de(var), var = factor(var, levels = ordem_vars))
df_ref <- prep(df_ref)
df_dif <- prep(df_dif)

g_24v2 <- ggplot() +
  geom_tile(data = df_ref, aes(Cenario, var), fill = "grey93", color = "white", linewidth = 0.5) +
  geom_tile(data = df_dif, aes(Cenario, var, fill = cor), color = "white", linewidth = 0.5) +
  geom_text(data = df_ref, aes(Cenario, var, label = rotulo), size = 2.6, lineheight = 0.85) +
  geom_text(data = df_dif, aes(Cenario, var, label = rotulo, colour = cor_txt), size = 2.6, lineheight = 0.85) +
  scale_colour_identity() +
  facet_grid(Fase ~ Solo, scales = "free", space = "free") +
  scale_y_discrete(labels = rotulo_var) +
  scale_fill_gradient2(low = COR_REDUCAO, mid = "white", high = COR_AUMENTO, midpoint = 0,
                       limits = c(-LIM_DP, LIM_DP), breaks = c(-LIM_DP, 0, LIM_DP),
                       labels = c("Reduction", "No change", "Increase"),
                       name = "Intensity: up to 3 standard deviations across years",
                       guide = guide_colorbar(title.position = "top", title.hjust = 0.5,
                                              barwidth = unit(7, "cm"), barheight = unit(0.35, "cm"))) +
  labs(
    title = "Climate changes with sowing delay from soil modal date",
    x = "Sowing scenario", y = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title        = element_text(face = "bold", color = "#20364C"),
    plot.subtitle     = element_text(size = 8, color = "grey35"),
    plot.caption      = element_text(size = 7.5, color = "grey40", hjust = 0),
    plot.caption.position = "plot",
    strip.background  = element_rect(fill = "#20364C"),
    strip.text        = element_text(color = "white", face = "bold", size = 8.5),
    strip.text.y      = element_text(angle = 0),
    panel.grid        = element_blank(),
    axis.text.y       = element_text(family = "mono", size = 7.5, color = "grey10"),
    axis.text.x       = element_text(size = 8.5, color = "grey10"),
    legend.position   = "bottom",
    legend.title      = element_text(size = 8, color = "grey25"),
    legend.text       = element_text(size = 8)
  )

print(g_24v2)
ggsave(paste0("figuras/24v2_HEATMAP_MODA_", min(ANOS_CLIMA), "_", max(ANOS_CLIMA), ".png"), g_24v2,
       width = 10, height = 8, dpi = 300, bg = "white")



##################### CICLO SUPERPRECOCE E PRECOCE COM SOLO ###############
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(lubridate)

TBASE <- 10
setwd("/Users/mariavitoriadiastorres/Downloads")
source("funcoes_clima.R")
dir.create("figuras", showWarnings = FALSE)

ARQ_DADOS  <- "/Users/mariavitoriadiastorres/Downloads/dados_modelo_final.xlsx"
ANOS_CLIMA <- 2017:2024
ATRASOS    <- c(0, 10, 20, 30)
MODA_SOLO  <- c("Latossolo" = "02-24", "Plintossolo" = "03-03")
ARQ_CACHE  <- paste0("figuras/clima_heatmap_ciclo_", min(ANOS_CLIMA), "_", max(ANOS_CLIMA), ".csv")
PYTHON     <- "/usr/local/bin/python3"

CICLOS <- tibble::tribble(
  ~Ciclo,          ~ciclo, ~dias_veg,
  "Super-early",    121,        45,
  "Early",          136,        50
)
DIAS_FILL <- 20

VARS <- c(
  "precip_sum_veg", "dry_days_veg", "dry_spell_max_veg", "wbal_veg",
  "precip_sum_rep", "dry_days_rep", "dry_spell_max_rep", "wbal_rep",
  "wdemand_days_rep", "vpd_high_days_rep", "vpd_p90_rep", "tmax_p90_rep",
  "precip_sum_fill", "wbal_fill", "wdemand_days_fill"
)

unidade <- function(v) case_when(
  grepl("^precip|^wbal", v) ~ "mm",
  grepl("days|spell", v)    ~ "days",
  grepl("^vpd_p90", v)      ~ "kPa",
  grepl("^tmax_p90", v)     ~ "°C",
  TRUE                      ~ ""
)
casas <- function(v) ifelse(grepl("^vpd_p90", v), 2, ifelse(grepl("^tmax_p90", v), 1, 0))

fase_de <- function(v) factor(case_when(
  grepl("_veg$",  v) ~ "Vegetative",
  grepl("_rep$",  v) ~ "Reproductive",
  grepl("_fill$", v) ~ "Grain filling"
), levels = c("Vegetative", "Reproductive", "Grain filling"))

calcular_clima_cenario_ciclo <- function(sp, stx, stn, lat, plantio, colheita,
                                         dias_veg = 45, dias_fill = 20) {
  veg_ini  <- plantio;         veg_fim  <- plantio + dias_veg
  rep_ini  <- veg_fim;         rep_fim  <- colheita - dias_fill
  fill_ini <- rep_fim;         fill_fim <- colheita
  
  p_veg  <- recorte(sp,  veg_ini,  veg_fim)
  p_rep  <- recorte(sp,  rep_ini,  rep_fim)
  p_fill <- recorte(sp,  fill_ini, fill_fim)
  tx_veg <- recorte(stx, veg_ini,  veg_fim);  tn_veg <- recorte(stn, veg_ini,  veg_fim)
  tx_rep <- recorte(stx, rep_ini,  rep_fim);  tn_rep <- recorte(stn, rep_ini,  rep_fim)
  tx_fil <- recorte(stx, fill_ini, fill_fim); tn_fil <- recorte(stn, fill_ini, fill_fim)
  
  m_veg  <- calcular_metricas_fase(p_veg,  tx_veg, tn_veg, lat, TBASE)
  m_rep  <- calcular_metricas_fase(p_rep,  tx_rep, tn_rep, lat, TBASE)
  m_fill <- calcular_metricas_fase(p_fill, tx_fil, tn_fil, lat, TBASE)
  tmax_rep_vec <- tx_rep$valor
  
  data.frame(
    precip_sum_veg    = m_veg$precip_sum,    precip_sum_rep    = m_rep$precip_sum,
    precip_sum_fill   = m_fill$precip_sum,
    dry_days_veg      = m_veg$dry_days,    dry_days_rep      = m_rep$dry_days,
    dry_spell_max_veg = m_veg$dry_spell_max, dry_spell_max_rep = m_rep$dry_spell_max,
    wbal_veg          = m_veg$wbal,        wbal_rep          = m_rep$wbal,
    wbal_fill         = m_fill$wbal,
    wdemand_days_rep  = m_rep$wdemand_days, wdemand_days_fill = m_fill$wdemand_days,
    tmax_p90_rep      = as.numeric(quantile(tmax_rep_vec, 0.90, na.rm = TRUE)),
    vpd_high_days_rep = m_rep$vpd_high_days, vpd_p90_rep     = m_rep$vpd_p90
  )
}

dados <- read_excel(ARQ_DADOS) %>%
  mutate(Plantio  = as.Date(Plantio),
         Colheita = as.Date(Colheita),
         Ano_num  = as.numeric(as.character(Ano)),
         ciclo_ensaio = as.integer(Colheita - Plantio)) %>%
  filter(!is.na(Plantio), !is.na(Colheita), !is.na(Local), !is.na(Solo))

melhor_ano <- dados %>%
  group_by(Local, Solo, Ano_num) %>%
  summarise(kgha_medio = mean(kgha), .groups = "drop") %>%
  group_by(Local, Solo) %>%
  slice_max(kgha_medio, n = 1, with_ties = FALSE) %>%
  ungroup()

locais <- dados %>%
  semi_join(melhor_ano, by = c("Local", "Solo", "Ano_num")) %>%
  count(Local, Solo, LATITUDE, LONGITUDE, ciclo_ensaio, name = "n_parcelas") %>%
  group_by(Local, Solo) %>%
  slice_max(n_parcelas, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Solo, Local, LATITUDE, LONGITUDE, ciclo_ensaio)

cenarios <- locais %>%
  crossing(CICLOS) %>%
  crossing(Ano_clima = ANOS_CLIMA) %>%
  mutate(plantio_ref = as.Date(paste0(Ano_clima, "-", MODA_SOLO[Solo])))

chaves <- c("Solo", "Local", "LATITUDE", "LONGITUDE", "Ciclo", "Ano_clima", "Dias_Atraso")

usar_cache <- FALSE
if (file.exists(ARQ_CACHE)) {
  clima <- read.csv(ARQ_CACHE) %>% mutate(plantio_ref = as.Date(plantio_ref))
  faltando <- anti_join(cenarios %>% crossing(Dias_Atraso = ATRASOS), clima, by = chaves)
  usar_cache <- nrow(faltando) == 0
}

if (!usar_cache) {
  library(reticulate)
  if (!py_available(initialize = FALSE)) use_python(PYTHON, required = TRUE)
  ee <- import("ee")
  ee$Initialize(project = "projeto-pesquisa-ufg")
  
  series <- cenarios %>% distinct(Solo, Local, LATITUDE, LONGITUDE, Ano_clima, plantio_ref)
  clima <- list()
  for (i in seq_len(nrow(series))) {
    s <- series[i, ]
    ser <- baixar_series_safra(s$LATITUDE, s$LONGITUDE,
                               s$plantio_ref, s$plantio_ref + max(CICLOS$ciclo), max(ATRASOS))
    if (any(sapply(ser, is.null))) stop("Empty series: ", s$Local, " ", s$Ano_clima)
    
    for (k in seq_len(nrow(CICLOS))) {
      cl <- CICLOS[k, ]
      for (d in ATRASOS) {
        clima[[length(clima) + 1]] <-
          calcular_clima_cenario_ciclo(ser$sp, ser$stx, ser$stn, s$LATITUDE,
                                       s$plantio_ref + d, s$plantio_ref + d + cl$ciclo,
                                       dias_veg = cl$dias_veg, dias_fill = DIAS_FILL) %>%
          mutate(Solo = s$Solo, Local = s$Local, LATITUDE = s$LATITUDE, LONGITUDE = s$LONGITUDE,
                 Ciclo = cl$Ciclo, ciclo = cl$ciclo, dias_veg = cl$dias_veg,
                 Ano_clima = s$Ano_clima, plantio_ref = s$plantio_ref,
                 Dias_Atraso = d, .before = 1)
      }
    }
  }
  clima <- bind_rows(clima)
  write.csv(clima, ARQ_CACHE, row.names = FALSE)
}

long <- clima %>%
  select(Solo, Local, Ciclo, Ano_clima, Dias_Atraso, all_of(VARS)) %>%
  pivot_longer(all_of(VARS), names_to = "var", values_to = "valor")

ref <- long %>%
  filter(Dias_Atraso == 0) %>%
  select(Solo, Local, Ciclo, Ano_clima, var, x0 = valor)

dif <- long %>%
  filter(Dias_Atraso > 0) %>%
  left_join(ref, by = c("Solo", "Local", "Ciclo", "Ano_clima", "var")) %>%
  mutate(d = valor - x0)

resumo_ref <- ref %>%
  group_by(Solo, Ciclo, var) %>%
  summarise(media = mean(x0), dp = sd(x0), n = n(), .groups = "drop")

resumo_dif <- dif %>%
  group_by(Solo, Ciclo, var, Dias_Atraso) %>%
  summarise(media = mean(d), dp = sd(d), minimo = min(d), maximo = max(d),
            n = n(), .groups = "drop")

tabela <- bind_rows(
  resumo_ref %>% mutate(Cenario = "Sowing date (soil mode)"),
  resumo_dif %>% mutate(Cenario = paste0("+", Dias_Atraso, " days"))
) %>%
  mutate(unidade = unidade(var)) %>%
  select(Solo, Ciclo, var, unidade, Cenario, media, dp, minimo, maximo, n)

write.csv(tabela, "figuras/24v3_tabela_heatmap_ciclo.csv", row.names = FALSE)

rotulo_ref   <- c("Latossolo" = "Feb 24\n(Sowing date)", "Plintossolo" = "Mar 03\n(Sowing date)")
niveis_cen   <- c(rotulo_ref, "+10 days", "+20 days", "+30 days")
niveis_ciclo <- CICLOS$Ciclo
ordem_vars   <- rev(VARS)
rotulo_var   <- setNames(paste0(VARS, " (", unidade(VARS), ")"), VARS)

fmt <- function(x, v, sinal) {
  k <- as.integer(casas(v))
  x <- ifelse(abs(x) < 0.5 * 10^(-k), 0, x)
  s <- sprintf(if (sinal) "%+.*f" else "%.*f", k, x)
  gsub(".", ",", s, fixed = TRUE)
}

MIN_RELEVANTE <- c("mm" = 5, "days" = 1, "kPa" = 0.05, "°C" = 0.5)
LIM_DP        <- 3
COR_REDUCAO   <- "red4"   
COR_AUMENTO   <- "green4" 

df_ref <- resumo_ref %>%
  mutate(Cenario = factor(rotulo_ref[Solo], levels = niveis_cen),
         rotulo  = paste0(fmt(media, var, FALSE), "\n(", fmt(dp, var, FALSE), ")"))

df_dif <- resumo_dif %>%
  left_join(resumo_ref %>% select(Solo, Ciclo, var, dp_ref = dp), by = c("Solo", "Ciclo", "var")) %>%
  mutate(Cenario   = factor(paste0("+", Dias_Atraso, " days"), levels = niveis_cen),
         relevante = abs(media) >= MIN_RELEVANTE[unidade(var)],
         cor       = ifelse(relevante & dp_ref > 0, pmax(pmin(media / dp_ref, LIM_DP), -LIM_DP), 0),
         cor_txt   = ifelse(abs(cor) > 2, "white", "black"),
         rotulo    = paste0(fmt(media, var, TRUE), "\n(", fmt(dp, var, FALSE), ")"))

prep <- function(df) df %>%
  mutate(Fase   = fase_de(var), var = factor(var, levels = ordem_vars),
         Ciclo  = factor(Ciclo, levels = niveis_ciclo),
         Painel = factor(paste(Solo, "·", Ciclo),
                         levels = as.vector(t(outer(names(MODA_SOLO), niveis_ciclo, paste, sep = " · ")))))
df_ref <- prep(df_ref)
df_dif <- prep(df_dif)

plot_heat <- function(r, d, titulo, facet) {
  ggplot() +
    geom_tile(data = r, aes(Cenario, var), fill = "grey93", color = "white", linewidth = 0.5) +
    geom_tile(data = d, aes(Cenario, var, fill = cor), color = "white", linewidth = 0.5) +
    geom_text(data = r, aes(Cenario, var, label = rotulo), size = 2.6, lineheight = 0.85) +
    geom_text(data = d, aes(Cenario, var, label = rotulo, colour = cor_txt), size = 2.6, lineheight = 0.85) +
    scale_colour_identity() +
    facet +
    scale_y_discrete(labels = rotulo_var) +
    scale_fill_gradient2(low = COR_REDUCAO, mid = "white", high = COR_AUMENTO, midpoint = 0,
                         limits = c(-LIM_DP, LIM_DP), breaks = c(-LIM_DP, 0, LIM_DP),
                         labels = c("Reduction", "No change", "Increase"),
                         name = "Intensity: up to 3 standard deviations across years",
                         guide = guide_colorbar(title.position = "top", title.hjust = 0.5,
                                                barwidth = unit(7, "cm"), barheight = unit(0.35, "cm"))) +
    labs(
      title = titulo,
      x = "Sowing scenario", y = NULL
    ) +
    theme_bw(base_size = 10) +
    theme(
      plot.title        = element_text(face = "bold", color = "#20364C"),
      plot.subtitle     = element_text(size = 8, color = "grey35"),
      plot.caption      = element_text(size = 7.5, color = "grey40", hjust = 0),
      plot.caption.position = "plot",
      strip.background  = element_rect(fill = "#20364C"),
      strip.text        = element_text(color = "white", face = "bold", size = 8.5),
      strip.text.y      = element_text(angle = 0),
      panel.grid        = element_blank(),
      axis.text.y       = element_text(family = "mono", size = 7.5, color = "grey10"),
      axis.text.x       = element_text(size = 8.5, color = "grey10"),
      legend.position   = "bottom",
      legend.title      = element_text(size = 8, color = "grey25"),
      legend.text       = element_text(size = 8)
    )
}

g_todos <- plot_heat(df_ref, df_dif,
                     "Climate changes with sowing delay by soil and crop cycle class",
                     facet_grid(Fase ~ Painel, scales = "free", space = "free"))
print(g_todos)
ggsave("figuras/24v3_HEATMAP_CICLO_4PAINEIS.png", g_todos, width = 17, height = 9, dpi = 300, bg = "white")

for (sl in names(MODA_SOLO)) for (cl in niveis_ciclo) {
  r <- df_ref %>% filter(Solo == sl, Ciclo == cl)
  d <- df_dif %>% filter(Solo == sl, Ciclo == cl)
  if (nrow(r) == 0) next
  g <- plot_heat(r, d, paste0(sl, " · ", cl, ": climate changes with sowing delay"),
                 facet_grid(Fase ~ Painel, scales = "free", space = "free"))
  arq <- paste0("figuras/24v3_HEATMAP_", toupper(sl), "_", toupper(cl), ".png")
  ggsave(arq, g, width = 7, height = 8.5, dpi = 300, bg = "white")
}


############## GRAFICO QUE SERÁ USADO #################################
##################### HEATMAP SUPERPRECOCE × PRECOCE (GERAL)     ####################################

library(dplyr)
library(tidyr)
library(ggplot2)

setwd("/Users/mariavitoriadiastorres/Downloads")
dir.create("figuras", showWarnings = FALSE)

ANOS_CLIMA <- 2017:2024
ARQ_CACHE  <- paste0("figuras/clima_heatmap_ciclo_", min(ANOS_CLIMA), "_", max(ANOS_CLIMA), ".csv")

CICLOS <- tibble::tribble(
  ~Ciclo,          ~ciclo, ~dias_veg,
  "Super-early",    121,        45,
  "Early",          136,        50
)

VARS <- c(
  "precip_sum_veg", "dry_days_veg", "dry_spell_max_veg", "wbal_veg",
  "precip_sum_rep", "dry_days_rep", "dry_spell_max_rep", "wbal_rep",
  "wdemand_days_rep", "vpd_high_days_rep", "vpd_p90_rep", "tmax_p90_rep",
  "precip_sum_fill", "wbal_fill", "wdemand_days_fill"
)

unidade <- function(v) case_when(
  grepl("^precip|^wbal", v) ~ "mm",
  grepl("days|spell", v)    ~ "days",
  grepl("^vpd_p90", v)      ~ "kPa",
  grepl("^tmax_p90", v)     ~ "°C",
  TRUE                      ~ ""
)
casas <- function(v) ifelse(grepl("^vpd_p90", v), 2, ifelse(grepl("^tmax_p90", v), 1, 0))

fase_de <- function(v) factor(case_when(
  grepl("_veg$",  v) ~ "Vegetative",
  grepl("_rep$",  v) ~ "Reproductive",
  grepl("_fill$", v) ~ "Grain filling"
), levels = c("Vegetative", "Reproductive", "Grain filling"))

MIN_RELEVANTE <- c("mm" = 5, "days" = 1, "kPa" = 0.05, "°C" = 0.5)
LIM_DP        <- 3
COR_REDUCAO   <- "red4"   
COR_AUMENTO   <- "green4" 

if (!file.exists(ARQ_CACHE))
  stop("Cache file not found: ", ARQ_CACHE)

clima <- read.csv(ARQ_CACHE) %>%
  filter(Ano_clima %in% ANOS_CLIMA) %>%
  mutate(Ciclo = recode(Ciclo, "Superprecoce" = "Super-early", "Precoce" = "Early"))

long <- clima %>%
  select(Solo, Local, Ciclo, Ano_clima, Dias_Atraso, all_of(VARS)) %>%
  pivot_longer(all_of(VARS), names_to = "var", values_to = "valor")

ref <- long %>%
  filter(Dias_Atraso == 0) %>%
  select(Solo, Local, Ciclo, Ano_clima, var, x0 = valor)

dif <- long %>%
  filter(Dias_Atraso > 0) %>%
  left_join(ref, by = c("Solo", "Local", "Ciclo", "Ano_clima", "var")) %>%
  mutate(d = valor - x0)

resumo_ref <- ref %>%
  group_by(Ciclo, var) %>%
  summarise(media = mean(x0), dp = sd(x0), n = n(), .groups = "drop")

resumo_dif <- dif %>%
  group_by(Ciclo, var, Dias_Atraso) %>%
  summarise(media = mean(d), dp = sd(d), minimo = min(d), maximo = max(d),
            n = n(), .groups = "drop")

tabela <- bind_rows(
  resumo_ref %>% mutate(Cenario = "Sowing date"),
  resumo_dif %>% mutate(Cenario = paste0("+", Dias_Atraso, " days"))
) %>%
  mutate(unidade = unidade(var)) %>%
  select(Ciclo, var, unidade, Cenario, media, dp, minimo, maximo, n)

write.csv(tabela, "figuras/24v4_tabela_superprecoce_x_precoce.csv", row.names = FALSE)

niveis_cen   <- c("Sowing date", "+10 days", "+20 days", "+30 days")
niveis_ciclo <- CICLOS$Ciclo
ordem_vars   <- rev(VARS)
rotulo_var   <- setNames(paste0(VARS, " (", unidade(VARS), ")"), VARS)
rotulo_ciclo <- setNames(CICLOS$Ciclo, CICLOS$Ciclo)

fmt <- function(x, v, sinal) {
  k <- as.integer(casas(v))
  x <- ifelse(abs(x) < 0.5 * 10^(-k), 0, x)
  s <- sprintf(if (sinal) "%+.*f" else "%.*f", k, x)
  gsub(".", ",", s, fixed = TRUE)
}

df_ref <- resumo_ref %>%
  mutate(Cenario = factor("Sowing date", levels = niveis_cen),
         rotulo  = paste0(fmt(media, var, FALSE), "\n(", fmt(dp, var, FALSE), ")"))

df_dif <- resumo_dif %>%
  left_join(resumo_ref %>% select(Ciclo, var, dp_ref = dp), by = c("Ciclo", "var")) %>%
  mutate(Cenario   = factor(paste0("+", Dias_Atraso, " days"), levels = niveis_cen),
         relevante = abs(media) >= MIN_RELEVANTE[unidade(var)],
         cor       = ifelse(relevante & dp_ref > 0, pmax(pmin(media / dp_ref, LIM_DP), -LIM_DP), 0),
         cor_txt   = ifelse(abs(cor) > 2, "white", "black"),
         rotulo    = paste0(fmt(media, var, TRUE), "\n(", fmt(dp, var, FALSE), ")"))

prep <- function(df) df %>%
  mutate(Fase  = fase_de(var),
         var   = factor(var, levels = ordem_vars),
         Ciclo = factor(Ciclo, levels = niveis_ciclo))
df_ref <- prep(df_ref)
df_dif <- prep(df_dif)

g_24v4 <- ggplot() +
  geom_tile(data = df_ref, aes(Cenario, var), fill = "grey93", color = "white", linewidth = 0.5) +
  geom_tile(data = df_dif, aes(Cenario, var, fill = cor), color = "white", linewidth = 0.5) +
  geom_text(data = df_ref, aes(Cenario, var, label = rotulo), size = 2.7, lineheight = 0.85) +
  geom_text(data = df_dif, aes(Cenario, var, label = rotulo, colour = cor_txt),
            size = 2.7, lineheight = 0.85) +
  scale_colour_identity() +
  facet_grid(Fase ~ Ciclo, scales = "free_y", space = "free_y",
             labeller = labeller(Ciclo = rotulo_ciclo)) +
  scale_y_discrete(labels = rotulo_var) +
  scale_fill_gradient2(low = COR_REDUCAO, mid = "white", high = COR_AUMENTO, midpoint = 0,
                       limits = c(-LIM_DP, LIM_DP), breaks = c(-LIM_DP, 0, LIM_DP),
                       labels = c("Reduction", "No change", "Increase"),
                       name = "Intensity: up to 3 standard deviations across years",
                       guide = guide_colorbar(title.position = "top", title.hjust = 0.5,
                                              barwidth = unit(7, "cm"), barheight = unit(0.35, "cm"))) +
  labs(
    title   = "Climate changes with sowing delay: Super-early × Early",
    caption = "",
    x = "Sowing scenario", y = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title        = element_text(face = "bold", color = "#20364C"),
    plot.caption      = element_text(size = 7.5, color = "grey40", hjust = 0),
    plot.caption.position = "plot",
    strip.background  = element_rect(fill = "#20364C"),
    strip.text        = element_text(color = "white", face = "bold", size = 8.5),
    strip.text.y      = element_text(angle = 0),
    panel.grid        = element_blank(),
    axis.text.y       = element_text(family = "mono", size = 7.5, color = "grey10"),
    axis.text.x       = element_text(size = 8.5, color = "grey10"),
    legend.position   = "bottom",
    legend.title      = element_text(size = 8, color = "grey25"),
    legend.text       = element_text(size = 8)
  )

print(g_24v4)
ggsave("figuras/24v4_HEATMAP_SUPERPRECOCE_X_PRECOCE.png", g_24v4,
       width = 11, height = 8.5, dpi = 300, bg = "white")
#########################################################################################

dir.create("figuras", showWarnings = FALSE)

enso <- data.frame(
  Ano        = 2017:2024,
  Classe     = c("Neutro", "La Niña", "El Niño", "Neutro", "La Niña", "La Niña", "Neutro", "Neutro"),
  n_elnino   = c(0, 0, 3, 0, 0, 0, 0, 2),
  n_neutro   = c(4, 2, 2, 3, 0, 0, 4, 2),
  n_lanina   = c(1, 3, 0, 2, 5, 5, 1, 1),
  roni_medio = c(-0.14, -0.58, 0.44, -0.32, -0.78, -1.20, -0.14, 0.14),
  empate     = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
)

write.csv(enso, "figuras/classificacao_enso_safrinha.csv", row.names = FALSE, fileEncoding = "UTF-8")
file.exists("figuras/classificacao_enso_safrinha.csv")




library(dplyr)
library(tidyr)
library(ggplot2)

setwd("/Users/mariavitoriadiastorres/Downloads")
dir.create("figuras", showWarnings = FALSE)

ANOS_CLIMA  <- 2017:2024
ARQ_CACHE   <- paste0("figuras/clima_heatmap_ciclo_", min(ANOS_CLIMA), "_", max(ANOS_CLIMA), ".csv")
ARQ_ENSO    <- "figuras/classificacao_enso_safrinha.csv"
CICLOS_USAR <- c("Super-early", "Early")      # para usar só um ciclo: c("Early")

VARS <- c(
  "precip_sum_veg", "dry_days_veg", "dry_spell_max_veg", "wbal_veg",
  "precip_sum_rep", "dry_days_rep", "dry_spell_max_rep", "wbal_rep",
  "wdemand_days_rep", "vpd_high_days_rep", "vpd_p90_rep", "tmax_p90_rep",
  "precip_sum_fill", "wbal_fill", "wdemand_days_fill"
)

unidade <- function(v) case_when(
  grepl("^precip|^wbal", v) ~ "mm",
  grepl("days|spell", v)    ~ "days",
  grepl("^vpd_p90", v)      ~ "kPa",
  grepl("^tmax_p90", v)     ~ "°C",
  TRUE                      ~ ""
)
casas <- function(v) ifelse(grepl("^vpd_p90", v), 2, ifelse(grepl("^tmax_p90", v), 1, 0))

fase_de <- function(v) factor(case_when(
  grepl("_veg$",  v) ~ "Vegetative",
  grepl("_rep$",  v) ~ "Reproductive",
  grepl("_fill$", v) ~ "Grain filling"
), levels = c("Vegetative", "Reproductive", "Grain filling"))

# Códigos internos SEM acento/ñ (evita problema de codificação); o "Niño" só aparece no rótulo
NIVEIS_ENSO   <- c("ElNino", "LaNina", "Neutro")
NOME_ENSO     <- c("ElNino" = "El Ni\u00f1o", "LaNina" = "La Ni\u00f1a", "Neutro" = "Neutral")
MIN_RELEVANTE <- c("mm" = 5, "days" = 1, "kPa" = 0.05, "°C" = 0.5)
LIM_DP        <- 3
COR_REDUCAO   <- "red4"     # redução (valor menor que na data de plantio)
COR_AUMENTO   <- "green4"   # aumento (valor maior que na data de plantio)

if (!file.exists(ARQ_CACHE)) stop("Cache não encontrado: ", ARQ_CACHE, ". Rode o 24v3 uma vez.")
if (!file.exists(ARQ_ENSO))  stop("Classificação do ENSO não encontrada: ", ARQ_ENSO, ". Rode o 29.")

enso <- read.csv(ARQ_ENSO, encoding = "UTF-8") %>%
  transmute(Ano_clima = as.integer(Ano), ENSO = as.character(Classe))
# converte para os códigos internos olhando só o começo do texto (sem depender do "ñ")
enso$ENSO <- case_when(
  grepl("^El",  enso$ENSO, useBytes = TRUE) ~ "ElNino",
  grepl("^La",  enso$ENSO, useBytes = TRUE) ~ "LaNina",
  grepl("^Neu", enso$ENSO, useBytes = TRUE) ~ "Neutro",
  TRUE ~ NA_character_
)
if (any(is.na(enso$ENSO))) stop("Classe do ENSO não reconhecida no arquivo ", ARQ_ENSO)

clima_cache <- read.csv(ARQ_CACHE) %>%
  # o cache pode estar em português ou em inglês: deixa tudo em inglês
  mutate(Ciclo = recode(Ciclo, "Superprecoce" = "Super-early", "Precoce" = "Early"))
faltam_ciclos <- setdiff(CICLOS_USAR, unique(clima_cache$Ciclo))
if (length(faltam_ciclos) > 0)
  stop("Ciclo(s) não encontrado(s) no cache: ", paste(faltam_ciclos, collapse = ", "),
       ". Nomes que existem no cache: ", paste(unique(clima_cache$Ciclo), collapse = " | "))

clima <- clima_cache %>%
  mutate(Ano_clima = as.integer(Ano_clima)) %>%
  filter(Ano_clima %in% ANOS_CLIMA, Ciclo %in% CICLOS_USAR) %>%
  left_join(enso, by = "Ano_clima")
if (nrow(clima) == 0) stop("Nenhuma linha do cache sobrou depois do filtro de anos e ciclos.")

if (any(is.na(clima$ENSO)))
  stop("Ano sem fase do ENSO: ", paste(unique(clima$Ano_clima[is.na(clima$ENSO)]), collapse = ", "))
stopifnot(all(clima$ENSO %in% NIVEIS_ENSO))

cat("Fase do ENSO de cada ano:\n")
print(clima %>% distinct(Ano_clima, ENSO) %>% arrange(Ano_clima))

# Uma linha por município × ciclo × ano × atraso (4 cenários)
cont <- clima %>% count(Local, Ciclo, Ano_clima)
stopifnot(all(cont$n == 4))

# Cenários por fase = municípios × anos da fase × ciclos
n_cen <- clima %>% filter(Dias_Atraso == 0) %>% count(ENSO, name = "cenarios")
esperado <- clima %>% filter(Dias_Atraso == 0) %>%
  group_by(ENSO) %>%
  summarise(esperado = n_distinct(Local) * n_distinct(Ano_clima) * n_distinct(Ciclo), .groups = "drop")
n_cen <- left_join(n_cen, esperado, by = "ENSO")
print(n_cen)
stopifnot(all(n_cen$cenarios == n_cen$esperado))

# Sem NA
na_check <- colSums(is.na(clima[VARS]))
if (any(na_check > 0)) { print(na_check[na_check > 0]); stop("NA no clima.") }
cat("OK: base completa e sem NA.\n")

long <- clima %>%
  select(ENSO, Solo, Local, Ciclo, Ano_clima, Dias_Atraso, all_of(VARS)) %>%
  pivot_longer(all_of(VARS), names_to = "var", values_to = "valor")

ref <- long %>%
  filter(Dias_Atraso == 0) %>%
  select(ENSO, Solo, Local, Ciclo, Ano_clima, var, x0 = valor)

dif <- long %>%
  filter(Dias_Atraso > 0) %>%
  left_join(ref, by = c("ENSO", "Solo", "Local", "Ciclo", "Ano_clima", "var")) %>%
  mutate(d = valor - x0)
stopifnot(!any(is.na(dif$x0)))

resumo_ref <- ref %>%
  group_by(ENSO, var) %>%
  summarise(media = mean(x0), dp = sd(x0), n = n(), .groups = "drop")

resumo_dif <- dif %>%
  group_by(ENSO, var, Dias_Atraso) %>%
  summarise(media = mean(d), dp = sd(d), minimo = min(d), maximo = max(d),
            n = n(), .groups = "drop")

# DP de referência para a COR: o mesmo nos 3 painéis (todos os cenários juntos)
dp_cor <- ref %>% group_by(var) %>% summarise(dp_ref = sd(x0), .groups = "drop")

# Validação: a média das 3 fases ponderada pelo n tem que dar a média geral
geral <- dif %>% group_by(var, Dias_Atraso) %>% summarise(m_geral = mean(d), .groups = "drop")
chk <- resumo_dif %>%
  group_by(var, Dias_Atraso) %>%
  summarise(m_pond = sum(media * n) / sum(n), .groups = "drop") %>%
  inner_join(geral, by = c("var", "Dias_Atraso"))
cat("\nMédia ponderada das fases = média geral? Maior diferença:",
    signif(max(abs(chk$m_pond - chk$m_geral)), 3), "\n")
stopifnot(max(abs(chk$m_pond - chk$m_geral)) < 1e-8)

# Uma célula refeita à mão (La Niña, chuva reprodutiva, +20)
cel <- list(ENSO = "LaNina", var = "precip_sum_rep", atraso = 20)
sel <- function(at) clima[clima$ENSO == cel$ENSO & clima$Dias_Atraso == at,
                          c("Local", "Ciclo", "Ano_clima", cel$var)]
m <- merge(sel(0), sel(cel$atraso), by = c("Local", "Ciclo", "Ano_clima"), suffixes = c("_0", "_d"))
manual <- mean(m[[paste0(cel$var, "_d")]] - m[[paste0(cel$var, "_0")]])
script <- resumo_dif$media[resumo_dif$ENSO == cel$ENSO & resumo_dif$var == cel$var &
                             resumo_dif$Dias_Atraso == cel$atraso]
cat("Célula conferida à mão —", cel$ENSO, cel$var, "+", cel$atraso, "dias:",
    "manual =", round(manual, 2), "| script =", round(script, 2), "| n =", nrow(m), "\n")
stopifnot(abs(manual - script) < 1e-8)

tabela <- bind_rows(
  resumo_ref %>% mutate(Cenario = "Sowing date"),
  resumo_dif %>% mutate(Cenario = paste0("+", Dias_Atraso, " days"))
) %>%
  mutate(unidade = unidade(var), ENSO = NOME_ENSO[ENSO]) %>%
  select(ENSO, var, unidade, Cenario, media, dp, minimo, maximo, n)
write.csv(tabela, "figuras/24v5_tabela_enso.csv", row.names = FALSE)
cat("Tabela salva em figuras/24v5_tabela_enso.csv\n")


niveis_cen <- c("Sowing date", "+10 days", "+20 days", "+30 days")
ordem_vars <- rev(VARS)
rotulo_var <- setNames(paste0(VARS, " (", unidade(VARS), ")"), VARS)

anos_fase   <- clima %>% distinct(ENSO, Ano_clima) %>% group_by(ENSO) %>%
  summarise(anos = paste(sort(Ano_clima), collapse = ", "), .groups = "drop")
rotulo_enso <- setNames(paste0(NOME_ENSO[anos_fase$ENSO], " (", anos_fase$anos, ")"), anos_fase$ENSO)

fmt <- function(x, v, sinal) {
  k <- as.integer(casas(v))
  x <- ifelse(abs(x) < 0.5 * 10^(-k), 0, x)
  s <- sprintf(if (sinal) "%+.*f" else "%.*f", k, x)
  s <- ifelse(is.na(x), "–", s)
  gsub(".", ",", s, fixed = TRUE)
}

df_ref <- resumo_ref %>%
  mutate(Cenario = factor("Sowing date", levels = niveis_cen),
         rotulo  = paste0(fmt(media, var, FALSE), "\n(", fmt(dp, var, FALSE), ")"))

df_dif <- resumo_dif %>%
  left_join(dp_cor, by = "var") %>%
  mutate(Cenario   = factor(paste0("+", Dias_Atraso, " days"), levels = niveis_cen),
         relevante = abs(media) >= MIN_RELEVANTE[unidade(var)],
         cor       = ifelse(relevante & dp_ref > 0, pmax(pmin(media / dp_ref, LIM_DP), -LIM_DP), 0),
         cor_txt   = ifelse(abs(cor) > 2, "white", "black"),
         rotulo    = paste0(fmt(media, var, TRUE), "\n(", fmt(dp, var, FALSE), ")"))

prep <- function(df) df %>%
  mutate(Fase = fase_de(var),
         var  = factor(var, levels = ordem_vars),
         ENSO = factor(ENSO, levels = NIVEIS_ENSO))
df_ref <- prep(df_ref)
df_dif <- prep(df_dif)

g <- ggplot() +
  geom_tile(data = df_ref, aes(Cenario, var), fill = "grey93", color = "white", linewidth = 0.5) +
  geom_tile(data = df_dif, aes(Cenario, var, fill = cor), color = "white", linewidth = 0.5) +
  geom_text(data = df_ref, aes(Cenario, var, label = rotulo), size = 2.5, lineheight = 0.85) +
  geom_text(data = df_dif, aes(Cenario, var, label = rotulo, colour = cor_txt),
            size = 2.5, lineheight = 0.85) +
  scale_colour_identity() +
  facet_grid(Fase ~ ENSO, scales = "free_y", space = "free_y",
             labeller = labeller(ENSO = rotulo_enso)) +
  scale_y_discrete(labels = rotulo_var) +
  scale_fill_gradient2(low = COR_REDUCAO, mid = "white", high = COR_AUMENTO, midpoint = 0,
                       limits = c(-LIM_DP, LIM_DP), breaks = c(-LIM_DP, 0, LIM_DP),
                       labels = c("Reduction", "No change", "Increase"),
                       name = "Intensity: up to 3 standard deviations",
                       guide = guide_colorbar(title.position = "top", title.hjust = 0.5,
                                              barwidth = unit(7, "cm"), barheight = unit(0.35, "cm"))) +
  labs(
    title   = "Climate changes with sowing delay by ENSO phase",
    caption = "",
    x = "Sowing scenario", y = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", color = "#20364C"),
    plot.caption     = element_text(size = 7.5, color = "grey40", hjust = 0),
    plot.caption.position = "plot",
    strip.background = element_rect(fill = "#20364C"),
    strip.text       = element_text(color = "white", face = "bold", size = 8.5),
    strip.text.y     = element_text(angle = 0),
    panel.grid       = element_blank(),
    axis.text.y      = element_text(family = "mono", size = 7.5, color = "grey10"),
    axis.text.x      = element_text(size = 8.5, color = "grey10"),
    legend.position  = "bottom",
    legend.title     = element_text(size = 8, color = "grey25"),
    legend.text      = element_text(size = 8)
  )

print(g)
ggsave("figuras/24v5_HEATMAP_ENSO.png", g, width = 14, height = 8.5, dpi = 300, bg = "white")

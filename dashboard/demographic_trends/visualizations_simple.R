# ---------- TREND DEMOGRAFICI ISTAT (2002-2024) - SIMPLIFIED VERSION ------------
# Only Parma, Emilia-Romagna, Italia - No interactivity
# Each plot is self-contained and can be used independently in Quarto code chunks

# Packages ----
library(here)
library(fs)
library(dplyr)
library(tidyr)
library(ggplot2)

# Directories ----
dir_rds <- here::here("data", "data_out", "istat_demo_2002_2024")
dir_plots <- here::here("data", "plots")

# Colors ----
burg_md <- "#873C4A"
blu_md <- "#005d82"
grn_md <- "#539d90"

# Common caption
CAP <- "Fonte: Istat, Demografia in cifre, Nov. 2025 | Rielaborazione: Fondazione Cariparma"

# Helper function to prep data ----
prep_data <- function(df) {
  df |>
    dplyr::filter(territorio %in% c("Parma", "Emilia-Romagna", "ITALIA")) |>
    dplyr::mutate(territorio = factor(territorio, levels = c("Parma", "Emilia-Romagna", "ITALIA")))
}

# Theme template ----
theme_demografico <- function() {
  theme_minimal(base_size = 13) +
    theme(
      panel.grid.major = element_line(color = "grey90", linewidth = rel(0.3)),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
      axis.text.y = element_text(size = rel(0.85)),
      axis.title = element_text(size = rel(1), face = "bold"),
      plot.title = element_text(size = rel(1.3), face = "bold", margin = margin(b = 10)),
      plot.subtitle = element_text(size = rel(0.95), lineheight = 1.2, margin = margin(b = 10)),
      strip.text = element_text(size = rel(1.1), face = "bold"),
      legend.text = element_text(size = rel(0.9)),
      legend.title = element_blank(),
      legend.position = "bottom",
      panel.spacing = unit(1, "lines")
    )
}

# ==============================================================================
# DATASET 1: indicatori_di_struttura
# ==============================================================================

indicatori_di_struttura <- readRDS(fs::path(dir_rds, "indicatori_di_struttura.rds"))
data1 <- prep_data(indicatori_di_struttura)

# Plot: p01_e_m [Età Media] ----
p01_e_m <- data1 |>
  dplyr::filter(indicatore == "Età media") |>
  ggplot(aes(x = anno, y = valore, color = territorio, group = territorio)) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(breaks = seq(min(data1$anno), max(data1$anno), by = 1),
                     limits = c(min(data1$anno), max(data1$anno)),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_manual(values = c("Parma" = burg_md, "Emilia-Romagna" = grn_md, "ITALIA" = blu_md)) +
  theme_demografico() +
  labs(title = "Trend demografici: Età media",
       subtitle = "Indicatore espresso in: anni e decimi di anno",
       caption = CAP, x = "", y = "")

saveRDS(p01_e_m, here::here("data", "plots", "p01_e_m.rds"))

# Plot: p02_i_v [Indice di vecchiaia] ----
p02_i_v <- data1 |>
  dplyr::filter(indicatore == "Indice di vecchiaia") |>
  ggplot(aes(x = anno, y = valore, color = territorio, group = territorio)) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(breaks = seq(min(data1$anno), max(data1$anno), by = 1),
                     limits = c(min(data1$anno), max(data1$anno)),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_manual(values = c("Parma" = burg_md, "Emilia-Romagna" = grn_md, "ITALIA" = blu_md)) +
  theme_demografico() +
  labs(title = "Trend demografici: Indice di vecchiaia",
       subtitle = "Indicatore espresso in: % tra popolazione di 65+ anni e in età 0-14.",
       caption = CAP, x = "", y = "")

saveRDS(p02_i_v, here::here("data", "plots", "p02_i_v.rds"))

# Plot: p03_i_d_s [Indice di dipendenza strutturale] ----
p03_i_d_s <- data1 |>
  dplyr::filter(indicatore == "Indice di dipendenza strutturale") |>
  ggplot(aes(x = anno, y = valore, color = territorio, group = territorio)) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(breaks = seq(min(data1$anno), max(data1$anno), by = 1),
                     limits = c(min(data1$anno), max(data1$anno)),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_manual(values = c("Parma" = burg_md, "Emilia-Romagna" = grn_md, "ITALIA" = blu_md)) +
  theme_demografico() +
  labs(title = "Trend demografici: Indice di dipendenza strutturale",
       subtitle = "Indicatore espresso in: % tra popolazione in età non attiva (0-14 e 65+ anni) e in età attiva (15-64 anni)",
       caption = CAP, x = "", y = "")

saveRDS(p03_i_d_s, here::here("data", "plots", "p03_i_d_s.rds"))

# Plot: p04_i_d_a [Indice di dipendenza anziani] ----
p04_i_d_a <- data1 |>
  dplyr::filter(indicatore == "Indice di dipendenza anziani") |>
  ggplot(aes(x = anno, y = valore, color = territorio, group = territorio)) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(breaks = seq(min(data1$anno), max(data1$anno), by = 1),
                     limits = c(min(data1$anno), max(data1$anno)),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_manual(values = c("Parma" = burg_md, "Emilia-Romagna" = grn_md, "ITALIA" = blu_md)) +
  theme_demografico() +
  labs(title = "Trend demografici: Indice di dipendenza anziani",
       subtitle = "Indicatore espresso in: % tra popolazione di 65+ e in età attiva (15-64 anni)",
       caption = CAP, x = "", y = "")

saveRDS(p04_i_d_a, here::here("data", "plots", "p04_i_d_a.rds"))

# ==============================================================================
# DATASET 2: indicatori_struttura_popolazione
# ==============================================================================

indicatori_struttura_pop <- readRDS(fs::path(dir_rds, "indicatori_struttura_popolazione.rds"))
data2 <- prep_data(indicatori_struttura_pop)

# Plot: p06_0_14_anni ----
p06_0_14_anni <- data2 |>
  dplyr::filter(indicatore == "0-14 anni") |>
  ggplot(aes(x = anno, y = valore, color = territorio, group = territorio)) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(breaks = seq(min(data2$anno), max(data2$anno), by = 1),
                     limits = c(min(data2$anno), max(data2$anno)),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_manual(values = c("Parma" = burg_md, "Emilia-Romagna" = grn_md, "ITALIA" = blu_md)) +
  theme_demografico() +
  labs(title = "Trend demografici: 0-14 anni",
       subtitle = "Indicatore espresso in: % della popolazione per classe di età",
       caption = CAP, x = "", y = "")

saveRDS(p06_0_14_anni, here::here("data", "plots", "p06_0_14_anni.rds"))

# Plot: p06_15_64_anni ----
p06_15_64_anni <- data2 |>
  dplyr::filter(indicatore == "15-64 anni") |>
  ggplot(aes(x = anno, y = valore, color = territorio, group = territorio)) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(breaks = seq(min(data2$anno), max(data2$anno), by = 1),
                     limits = c(min(data2$anno), max(data2$anno)),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_manual(values = c("Parma" = burg_md, "Emilia-Romagna" = grn_md, "ITALIA" = blu_md)) +
  theme_demografico() +
  labs(title = "Trend demografici: 15-64 anni",
       subtitle = "Indicatore espresso in: % della popolazione per classe di età",
       caption = CAP, x = "", y = "")

saveRDS(p06_15_64_anni, here::here("data", "plots", "p06_15_64_anni.rds"))

# Plot: p06_65piu_anni ----
p06_65piu_anni <- data2 |>
  dplyr::filter(indicatore == "65 anni e oltre") |>
  ggplot(aes(x = anno, y = valore, color = territorio, group = territorio)) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(breaks = seq(min(data2$anno), max(data2$anno), by = 1),
                     limits = c(min(data2$anno), max(data2$anno)),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_manual(values = c("Parma" = burg_md, "Emilia-Romagna" = grn_md, "ITALIA" = blu_md)) +
  theme_demografico() +
  labs(title = "Trend demografici: 65 anni e oltre",
       subtitle = "Indicatore espresso in: % della popolazione per classe di età",
       caption = CAP, x = "", y = "")

saveRDS(p06_65piu_anni, here::here("data", "plots", "p06_65piu_anni.rds"))

# ==============================================================================
# INDIVIDUAL INDICATOR FILES
# ==============================================================================

# p07: crescita_naturale ----
crescita_naturale <- readRDS(fs::path(dir_rds, "crescita_naturale.rds")) |> prep_data()
p07_crescita_naturale <- crescita_naturale |>
  dplyr::filter(indicatore == "crescita_naturale") |>
  ggplot(aes(x = anno, y = valore, color = territorio, group = territorio)) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(breaks = seq(min(crescita_naturale$anno), max(crescita_naturale$anno), by = 1),
                     limits = c(min(crescita_naturale$anno), max(crescita_naturale$anno)),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_manual(values = c("Parma" = burg_md, "Emilia-Romagna" = grn_md, "ITALIA" = blu_md)) +
  theme_demografico() +
  labs(title = "Trend demografici: Crescita naturale",
       subtitle = "Indicatore espresso in: differenza tra il tasso di natalità e il tasso di mortalità",
       caption = CAP, x = "", y = "")
saveRDS(p07_crescita_naturale, here::here("data", "plots", "p07_crescita_naturale.rds"))

# p08: età_media_al_parto ----
età_media_al_parto <- readRDS(fs::path(dir_rds, "età_media_al_parto.rds")) |> prep_data()
p08_età_media_al_parto <- età_media_al_parto |>
  dplyr::filter(indicatore == "età_media_al_parto") |>
  ggplot(aes(x = anno, y = valore, color = territorio, group = territorio)) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(breaks = seq(min(età_media_al_parto$anno), max(età_media_al_parto$anno), by = 1),
                     limits = c(min(età_media_al_parto$anno), max(età_media_al_parto$anno)),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_manual(values = c("Parma" = burg_md, "Emilia-Romagna" = grn_md, "ITALIA" = blu_md)) +
  theme_demografico() +
  labs(title = "Trend demografici: Età media al parto",
       subtitle = "Indicatore espresso in: anni e decimi di anno",
       caption = CAP, x = "", y = "")
saveRDS(p08_età_media_al_parto, here::here("data", "plots", "p08_età_media_al_parto.rds"))

# p09: quoziente_di_mortalità ----
quoziente_di_mortalità <- readRDS(fs::path(dir_rds, "quoziente_di_mortalità.rds")) |> prep_data()
p09_quoziente_di_mortalità <- quoziente_di_mortalità |>
  dplyr::filter(indicatore == "quoziente_di_mortalità") |>
  ggplot(aes(x = anno, y = valore, color = territorio, group = territorio)) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(breaks = seq(min(quoziente_di_mortalità$anno), max(quoziente_di_mortalità$anno), by = 1),
                     limits = c(min(quoziente_di_mortalità$anno), max(quoziente_di_mortalità$anno)),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_manual(values = c("Parma" = burg_md, "Emilia-Romagna" = grn_md, "ITALIA" = blu_md)) +
  theme_demografico() +
  labs(title = "Trend demografici: Quoziente di mortalità",
       subtitle = "Indicatore espresso in: rapporto num. decessi nell'anno e num. medio popolazione residente per 1.000",
       caption = CAP, x = "", y = "")
saveRDS(p09_quoziente_di_mortalità, here::here("data", "plots", "p09_quoziente_di_mortalità.rds"))

# p10: quoziente_di_natalità ----
quoziente_di_natalità <- readRDS(fs::path(dir_rds, "quoziente_di_natalità.rds")) |> prep_data()
p10_quoziente_di_natalità <- quoziente_di_natalità |>
  dplyr::filter(indicatore == "quoziente_di_natalità") |>
  ggplot(aes(x = anno, y = valore, color = territorio, group = territorio)) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(breaks = seq(min(quoziente_di_natalità$anno), max(quoziente_di_natalità$anno), by = 1),
                     limits = c(min(quoziente_di_natalità$anno), max(quoziente_di_natalità$anno)),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_manual(values = c("Parma" = burg_md, "Emilia-Romagna" = grn_md, "ITALIA" = blu_md)) +
  theme_demografico() +
  labs(title = "Trend demografici: Quoziente di natalità",
       subtitle = "Indicatore espresso in: rapporto num. nati vivi nell'anno e num. medio popolazione residente per 1.000",
       caption = CAP, x = "", y = "")
saveRDS(p10_quoziente_di_natalità, here::here("data", "plots", "p10_quoziente_di_natalità.rds"))

# p11: quoziente_di_nuzialità ----
quoziente_di_nuzialità <- readRDS(fs::path(dir_rds, "quoziente_di_nuzialità.rds")) |> prep_data()
p11_quoziente_di_nuzialità <- quoziente_di_nuzialità |>
  dplyr::filter(indicatore == "quoziente_di_nuzialità") |>
  ggplot(aes(x = anno, y = valore, color = territorio, group = territorio)) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(breaks = seq(min(quoziente_di_nuzialità$anno), max(quoziente_di_nuzialità$anno), by = 1),
                     limits = c(min(quoziente_di_nuzialità$anno), max(quoziente_di_nuzialità$anno)),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_manual(values = c("Parma" = burg_md, "Emilia-Romagna" = grn_md, "ITALIA" = blu_md)) +
  theme_demografico() +
  labs(title = "Trend demografici: Quoziente di nuzialità",
       subtitle = "Indicatore espresso in: rapporto num. matrimoni celebrati nell'anno e num. medio popolazione residente per 1.000",
       caption = CAP, x = "", y = "")
saveRDS(p11_quoziente_di_nuzialità, here::here("data", "plots", "p11_quoziente_di_nuzialità.rds"))

# p12: saldo_migratorio_totale ----
saldo_migratorio_totale <- readRDS(fs::path(dir_rds, "saldo_migratorio_totale.rds")) |> prep_data()
p12_saldo_migratorio_totale <- saldo_migratorio_totale |>
  dplyr::filter(indicatore == "saldo_migratorio_totale") |>
  ggplot(aes(x = anno, y = valore, color = territorio, group = territorio)) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(breaks = seq(min(saldo_migratorio_totale$anno), max(saldo_migratorio_totale$anno), by = 1),
                     limits = c(min(saldo_migratorio_totale$anno), max(saldo_migratorio_totale$anno)),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_manual(values = c("Parma" = burg_md, "Emilia-Romagna" = grn_md, "ITALIA" = blu_md)) +
  theme_demografico() +
  labs(title = "Trend demografici: Saldo migratorio totale",
       subtitle = "Indicatore espresso in: differenza tra num. iscritti ed num. cancellati dai registri anagrafici per trasferimento di residenza",
       caption = CAP, x = "", y = "")
saveRDS(p12_saldo_migratorio_totale, here::here("data", "plots", "p12_saldo_migratorio_totale.rds"))

# p13: saldo_migratorio_interno ----
saldo_migratorio_interno <- readRDS(fs::path(dir_rds, "saldo_migratorio_interno.rds")) |> prep_data()
p13_saldo_migratorio_interno <- saldo_migratorio_interno |>
  dplyr::filter(indicatore == "saldo_migratorio_interno") |>
  ggplot(aes(x = anno, y = valore, color = territorio, group = territorio)) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(breaks = seq(min(saldo_migratorio_interno$anno), max(saldo_migratorio_interno$anno), by = 1),
                     limits = c(min(saldo_migratorio_interno$anno), max(saldo_migratorio_interno$anno)),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_manual(values = c("Parma" = burg_md, "Emilia-Romagna" = grn_md, "ITALIA" = blu_md)) +
  theme_demografico() +
  labs(title = "Trend demografici: Saldo migratorio interno",
       subtitle = "Indicatore espresso in: differenza tra num. iscritti e num. cancellati per trasferimento di residenza da/verso altro Comune",
       caption = CAP, x = "", y = "")
saveRDS(p13_saldo_migratorio_interno, here::here("data", "plots", "p13_saldo_migratorio_interno.rds"))

# p14: saldo_migratorio_con_l_estero ----
saldo_migratorio_con_l_estero <- readRDS(fs::path(dir_rds, "saldo_migratorio_con_l_estero.rds")) |> prep_data()
p14_saldo_migratorio_con_l_estero <- saldo_migratorio_con_l_estero |>
  dplyr::filter(indicatore == "saldo_migratorio_con_l_estero") |>
  ggplot(aes(x = anno, y = valore, color = territorio, group = territorio)) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(breaks = seq(min(saldo_migratorio_con_l_estero$anno), max(saldo_migratorio_con_l_estero$anno), by = 1),
                     limits = c(min(saldo_migratorio_con_l_estero$anno), max(saldo_migratorio_con_l_estero$anno)),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_manual(values = c("Parma" = burg_md, "Emilia-Romagna" = grn_md, "ITALIA" = blu_md)) +
  theme_demografico() +
  labs(title = "Trend demografici: Saldo migratorio con l'estero",
       subtitle = "Indicatore espresso in: rapporto tra il saldo migratorio con l'estero dell'anno e l'ammontare medio della popolazione residente per 1.000",
       caption = CAP, x = "", y = "")
saveRDS(p14_saldo_migratorio_con_l_estero, here::here("data", "plots", "p14_saldo_migratorio_con_l_estero.rds"))

# p15: speranza_di_vita_0 ----
speranza_di_vita_0 <- readRDS(fs::path(dir_rds, "speranza_di_vita_0.rds")) |>
  dplyr::filter(sotto_categoria == "Maschi e femmine") |> prep_data()
p15_speranza_di_vita_0 <- speranza_di_vita_0 |>
  dplyr::filter(indicatore == "speranza_di_vita_0") |>
  ggplot(aes(x = anno, y = valore, color = territorio, group = territorio)) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(breaks = seq(min(speranza_di_vita_0$anno), max(speranza_di_vita_0$anno), by = 1),
                     limits = c(min(speranza_di_vita_0$anno), max(speranza_di_vita_0$anno)),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_manual(values = c("Parma" = burg_md, "Emilia-Romagna" = grn_md, "ITALIA" = blu_md)) +
  theme_demografico() +
  labs(title = "Trend demografici: Speranza di vita a 0 anni",
       subtitle = "Indicatore espresso in: numero medio di anni che restano da vivere a un neonato",
       caption = CAP, x = "", y = "")
saveRDS(p15_speranza_di_vita_0, here::here("data", "plots", "p15_speranza_di_vita_0.rds"))

# p16: speranza_di_vita_65 ----
speranza_di_vita_65 <- readRDS(fs::path(dir_rds, "speranza_di_vita_65.rds")) |>
  dplyr::filter(sotto_categoria == "Maschi e femmine") |> prep_data()
p16_speranza_di_vita_65 <- speranza_di_vita_65 |>
  dplyr::filter(indicatore == "speranza_di_vita_65") |>
  ggplot(aes(x = anno, y = valore, color = territorio, group = territorio)) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(breaks = seq(min(speranza_di_vita_65$anno), max(speranza_di_vita_65$anno), by = 1),
                     limits = c(min(speranza_di_vita_65$anno), max(speranza_di_vita_65$anno)),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_manual(values = c("Parma" = burg_md, "Emilia-Romagna" = grn_md, "ITALIA" = blu_md)) +
  theme_demografico() +
  labs(title = "Trend demografici: Speranza di vita a 65 anni",
       subtitle = "Indicatore espresso in: numero medio di anni che restano da vivere a 65 anni",
       caption = CAP, x = "", y = "")
saveRDS(p16_speranza_di_vita_65, here::here("data", "plots", "p16_speranza_di_vita_65.rds"))

# p17: tasso_di_crescita_totale ----
tasso_di_crescita_totale <- readRDS(fs::path(dir_rds, "tasso_di_crescita_totale.rds")) |> prep_data()
p17_tasso_di_crescita_totale <- tasso_di_crescita_totale |>
  dplyr::filter(indicatore == "tasso_di_crescita_totale") |>
  ggplot(aes(x = anno, y = valore, color = territorio, group = territorio)) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(breaks = seq(min(tasso_di_crescita_totale$anno), max(tasso_di_crescita_totale$anno), by = 1),
                     limits = c(min(tasso_di_crescita_totale$anno), max(tasso_di_crescita_totale$anno)),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_manual(values = c("Parma" = burg_md, "Emilia-Romagna" = grn_md, "ITALIA" = blu_md)) +
  theme_demografico() +
  labs(title = "Trend demografici: Tasso di crescita totale",
       subtitle = "Indicatore espresso in: somma tasso di crescita naturale + tasso migratorio totale",
       caption = CAP, x = "", y = "")
saveRDS(p17_tasso_di_crescita_totale, here::here("data", "plots", "p17_tasso_di_crescita_totale.rds"))

# p18: tasso_di_fecondità_totale ----
tasso_di_fecondità_totale <- readRDS(fs::path(dir_rds, "tasso_di_fecondità_totale.rds")) |> prep_data()
p18_tasso_di_fecondità_totale <- tasso_di_fecondità_totale |>
  dplyr::filter(indicatore == "tasso_di_fecondità_totale") |>
  ggplot(aes(x = anno, y = valore, color = territorio, group = territorio)) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(breaks = seq(min(tasso_di_fecondità_totale$anno), max(tasso_di_fecondità_totale$anno), by = 1),
                     limits = c(min(tasso_di_fecondità_totale$anno), max(tasso_di_fecondità_totale$anno)),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_manual(values = c("Parma" = burg_md, "Emilia-Romagna" = grn_md, "ITALIA" = blu_md)) +
  theme_demografico() +
  labs(title = "Trend demografici: Tasso di fecondità totale",
       subtitle = "Indicatore espresso in: Numero medio di figli per donna in età fertile (15-49 anni)",
       caption = CAP, x = "", y = "")
saveRDS(p18_tasso_di_fecondità_totale, here::here("data", "plots", "p18_tasso_di_fecondità_totale.rds"))

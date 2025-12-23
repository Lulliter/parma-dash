# ---------- TREND DEMOGRAFICI ISTAT (2002-2024) - NO FUNCTIONS VERSION ------------
# Each plot is self-contained and can be used independently in Quarto code chunks

# Packages ----
library(here)
library(fs)
library(dplyr)
library(tidyr)
library(readxl)
library(janitor)
library(forcats)
library(glue)
library(stringr)
library(ggplot2)
library(patchwork)

# Directories ----
dir_rds <- here::here("data", "data_out", "istat_demo_2002_2024")
dir_plots <- here::here("data", "plots")

# Colors ----
grey_extra_sc <- "#4F4F4F"
grey_sc <- "#808080"
grey_md1 <- "#A9A9A9"
grey_md2 <- "#D3D3D3"
grey_extrlight <- "#FDFBF7"
burg_sc <- "#5C2129"
burg_md <- "#873C4A"
burg_lg <- "#B85E6A"
blu_sc <- "#033c55"
blu_md <- "#005d82"
blu_lg <- "#5582a7"
grn_sc <- "#246864"
grn_md <- "#539d90"
grn_lg <- "#8eb9b1"

# Common caption
CAP <- "Fonte: Istat, Demografia in cifre, Nov. 2025 | Rielaborazione: Fondazione Cariparma"

# ==============================================================================
# DATASET 1: indicatori_di_struttura
# ==============================================================================

# Load dataset
indicatori_di_struttura <- readRDS(fs::path(
  dir_rds,
  "indicatori_di_struttura.rds"
))

# Prepare data
data <- indicatori_di_struttura |>
  dplyr::filter(territorio %in% c("Parma", "Emilia-Romagna", "ITALIA")) |>
  dplyr::mutate(
    territorio = factor(territorio, levels = c("Parma", "Emilia-Romagna", "ITALIA"))
  )

# Plot: p01_e_m [Età Media] ----
p01_e_m <- data |>
  dplyr::filter(indicatore == "Età media") |>
  ggplot(aes(
    x = anno,
    y = valore,
    color = territorio,
    group = territorio
  )) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(
    breaks = seq(min(data$anno), max(data$anno), by = 1),
    limits = c(min(data$anno), max(data$anno)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Parma" = burg_md,
      "Emilia-Romagna" = grn_md,
      "ITALIA" = blu_md
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = rel(0.3)),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
    axis.text.y = element_text(size = rel(0.85)),
    axis.title = element_text(size = rel(1), face = "bold"),
    plot.title = element_text(
      size = rel(1.3),
      face = "bold",
      margin = margin(b = 10)
    ),
    strip.text = element_text(size = rel(1.1), face = "bold"),
    legend.text = element_text(size = rel(0.9)),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Trend demografici: Età media",
    subtitle = "Indicatore espresso in: anni e decimi di anno",
    caption = CAP,
    x = "",
    y = ""
  )

saveRDS(object = p01_e_m, file = here::here("data", "plots", "p01_e_m.rds"))

# Plot: p02_i_v [Indice di vecchiaia] ----
p02_i_v <- data |>
  dplyr::filter(indicatore == "Indice di vecchiaia") |>
  ggplot(aes(
    x = anno,
    y = valore,
    color = territorio,
    group = territorio
  )) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(
    breaks = seq(min(data$anno), max(data$anno), by = 1),
    limits = c(min(data$anno), max(data$anno)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Parma" = burg_md,
      "Emilia-Romagna" = grn_md,
      "ITALIA" = blu_md
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = rel(0.3)),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
    axis.text.y = element_text(size = rel(0.85)),
    axis.title = element_text(size = rel(1), face = "bold"),
    plot.title = element_text(
      size = rel(1.3),
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      lineheight = 1.2,
      margin = margin(b = 10)
    ),
    strip.text = element_text(size = rel(1.1), face = "bold"),
    legend.text = element_text(size = rel(0.9)),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Trend demografici: Indice di vecchiaia",
    subtitle = "Indicatore espresso in: % tra popolazione di 65+ anni e in età 0-14.",
    caption = CAP,
    x = "",
    y = ""
  )

saveRDS(object = p02_i_v, file = here::here("data", "plots", "p02_i_v.rds"))

# Plot: p03_i_d_s [Indice di dipendenza strutturale] ----
p03_i_d_s <- data |>
  dplyr::filter(indicatore == "Indice di dipendenza strutturale") |>
  ggplot(aes(
    x = anno,
    y = valore,
    color = territorio,
    group = territorio
  )) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(
    breaks = seq(min(data$anno), max(data$anno), by = 1),
    limits = c(min(data$anno), max(data$anno)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Parma" = burg_md,
      "Emilia-Romagna" = grn_md,
      "ITALIA" = blu_md
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = rel(0.3)),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
    axis.text.y = element_text(size = rel(0.85)),
    axis.title = element_text(size = rel(1), face = "bold"),
    plot.title = element_text(
      size = rel(1.3),
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      lineheight = 1.2,
      margin = margin(b = 10)
    ),
    strip.text = element_text(size = rel(1.1), face = "bold"),
    legend.text = element_text(size = rel(0.9)),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Trend demografici: Indice di dipendenza strutturale",
    subtitle = "Indicatore espresso in: % tra popolazione in età non attiva (0-14 e 65+ anni) e in età attiva (15-64 anni)",
    caption = CAP,
    x = "",
    y = ""
  )

saveRDS(object = p03_i_d_s, file = here::here("data", "plots", "p03_i_d_s.rds"))

# Plot: p04_i_d_a [Indice di dipendenza anziani] ----
p04_i_d_a <- data |>
  dplyr::filter(indicatore == "Indice di dipendenza anziani") |>
  ggplot(aes(
    x = anno,
    y = valore,
    color = territorio,
    group = territorio
  )) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(
    breaks = seq(min(data$anno), max(data$anno), by = 1),
    limits = c(min(data$anno), max(data$anno)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Parma" = burg_md,
      "Emilia-Romagna" = grn_md,
      "ITALIA" = blu_md
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = rel(0.3)),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
    axis.text.y = element_text(size = rel(0.85)),
    axis.title = element_text(size = rel(1), face = "bold"),
    plot.title = element_text(
      size = rel(1.3),
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      lineheight = 1.2,
      margin = margin(b = 10)
    ),
    strip.text = element_text(size = rel(1.1), face = "bold"),
    legend.text = element_text(size = rel(0.9)),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Trend demografici: Indice di dipendenza anziani",
    subtitle = "Indicatore espresso in: % tra popolazione di 65+ e in età attiva (15-64 anni)",
    caption = CAP,
    x = "",
    y = ""
  )

saveRDS(object = p04_i_d_a, file = here::here("data", "plots", "p04_i_d_a.rds"))

# ==============================================================================
# DATASET 2: indicatori_struttura_popolazione
# ==============================================================================

# Load dataset
indicatori_struttura_pop <- readRDS(fs::path(
  dir_rds,
  "indicatori_struttura_popolazione.rds"
))

# Prepare data
data2 <- indicatori_struttura_pop |>
  dplyr::filter(territorio %in% c("Parma", "Emilia-Romagna", "ITALIA")) |>
  dplyr::mutate(
    territorio = factor(territorio, levels = c("Parma", "Emilia-Romagna", "ITALIA"))
  )

# Plot: p06_0_14_anni [% classe di età 0-14 anni] ----
p06_0_14_anni <- data2 |>
  dplyr::filter(indicatore == "0-14 anni") |>
  ggplot(aes(
    x = anno,
    y = valore,
    color = territorio,
    group = territorio
  )) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(
    breaks = seq(min(data2$anno), max(data2$anno), by = 1),
    limits = c(min(data2$anno), max(data2$anno)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Parma" = burg_md,
      "Emilia-Romagna" = grn_md,
      "ITALIA" = blu_md
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = rel(0.3)),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
    axis.text.y = element_text(size = rel(0.85)),
    axis.title = element_text(size = rel(1), face = "bold"),
    plot.title = element_text(
      size = rel(1.3),
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      lineheight = 1.2,
      margin = margin(b = 10)
    ),
    strip.text = element_text(size = rel(1.1), face = "bold"),
    legend.text = element_text(size = rel(0.9)),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Trend demografici: 0-14 anni",
    subtitle = "Indicatore espresso in: % della popolazione per classe di età",
    caption = CAP,
    x = "",
    y = ""
  )

saveRDS(object = p06_0_14_anni, file = here::here("data", "plots", "p06_0_14_anni.rds"))

# Plot: p06_15_64_anni [% classe di età 15-64 anni] ----
p06_15_64_anni <- data2 |>
  dplyr::filter(indicatore == "15-64 anni") |>
  ggplot(aes(
    x = anno,
    y = valore,
    color = territorio,
    group = territorio
  )) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(
    breaks = seq(min(data2$anno), max(data2$anno), by = 1),
    limits = c(min(data2$anno), max(data2$anno)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Parma" = burg_md,
      "Emilia-Romagna" = grn_md,
      "ITALIA" = blu_md
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = rel(0.3)),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
    axis.text.y = element_text(size = rel(0.85)),
    axis.title = element_text(size = rel(1), face = "bold"),
    plot.title = element_text(
      size = rel(1.3),
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      lineheight = 1.2,
      margin = margin(b = 10)
    ),
    strip.text = element_text(size = rel(1.1), face = "bold"),
    legend.text = element_text(size = rel(0.9)),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Trend demografici: 15-64 anni",
    subtitle = "Indicatore espresso in: % della popolazione per classe di età",
    caption = CAP,
    x = "",
    y = ""
  )

saveRDS(object = p06_15_64_anni, file = here::here("data", "plots", "p06_15_64_anni.rds"))

# Plot: p06_65piu_anni [% classe di età 65 anni e oltre] ----
p06_65piu_anni <- data2 |>
  dplyr::filter(indicatore == "65 anni e oltre") |>
  ggplot(aes(
    x = anno,
    y = valore,
    color = territorio,
    group = territorio
  )) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(
    breaks = seq(min(data2$anno), max(data2$anno), by = 1),
    limits = c(min(data2$anno), max(data2$anno)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Parma" = burg_md,
      "Emilia-Romagna" = grn_md,
      "ITALIA" = blu_md
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = rel(0.3)),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
    axis.text.y = element_text(size = rel(0.85)),
    axis.title = element_text(size = rel(1), face = "bold"),
    plot.title = element_text(
      size = rel(1.3),
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      lineheight = 1.2,
      margin = margin(b = 10)
    ),
    strip.text = element_text(size = rel(1.1), face = "bold"),
    legend.text = element_text(size = rel(0.9)),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Trend demografici: 65 anni e oltre",
    subtitle = "Indicatore espresso in: % della popolazione per classe di età",
    caption = CAP,
    x = "",
    y = ""
  )

saveRDS(object = p06_65piu_anni, file = here::here("data", "plots", "p06_65piu_anni.rds"))

# ==============================================================================
# INDIVIDUAL INDICATOR FILES
# ==============================================================================

# Plot: p07_crescita_naturale ----
# Load & prep data
crescita_naturale <- readRDS(fs::path(dir_rds, "crescita_naturale.rds"))

crescita_naturale_rdx <- crescita_naturale |>
  dplyr::filter(territorio %in% c("Parma", "Emilia-Romagna", "ITALIA")) |>
  dplyr::mutate(
    territorio = factor(territorio, levels = c("Parma", "Emilia-Romagna", "ITALIA"))
  )

# Plot
p07_crescita_naturale <- crescita_naturale_rdx |>
  dplyr::filter(indicatore == "crescita_naturale") |>
  ggplot(aes(
    x = anno,
    y = valore,
    color = territorio,
    group = territorio
  )) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(
    breaks = seq(min(crescita_naturale_rdx$anno), max(crescita_naturale_rdx$anno), by = 1),
    limits = c(min(crescita_naturale_rdx$anno), max(crescita_naturale_rdx$anno)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Parma" = burg_md,
      "Emilia-Romagna" = grn_md,
      "ITALIA" = blu_md
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = rel(0.3)),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
    axis.text.y = element_text(size = rel(0.85)),
    axis.title = element_text(size = rel(1), face = "bold"),
    plot.title = element_text(
      size = rel(1.3),
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      lineheight = 1.2,
      margin = margin(b = 10)
    ),
    strip.text = element_text(size = rel(1.1), face = "bold"),
    legend.text = element_text(size = rel(0.9)),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Trend demografici: crescita_naturale",
    subtitle = "Indicatore espresso in: differenza tra il tasso di natalità e il tasso di mortalità",
    caption = CAP,
    x = "",
    y = ""
  )

saveRDS(object = p07_crescita_naturale, file = here::here("data", "plots", "p07_crescita_naturale.rds"))

# Plot: p08_età_media_al_parto ----
# Load & prep data
età_media_al_parto <- readRDS(fs::path(dir_rds, "età_media_al_parto.rds"))

età_media_al_parto_rdx <- età_media_al_parto |>
  dplyr::filter(territorio %in% c("Parma", "Emilia-Romagna", "ITALIA")) |>
  dplyr::mutate(
    territorio = factor(territorio, levels = c("Parma", "Emilia-Romagna", "ITALIA"))
  )

# Plot
p08_età_media_al_parto <- età_media_al_parto_rdx |>
  dplyr::filter(indicatore == "età_media_al_parto") |>
  ggplot(aes(
    x = anno,
    y = valore,
    color = territorio,
    group = territorio
  )) +
  geom_line(linewidth = rel(1.2)) +
  scale_x_continuous(
    breaks = seq(min(età_media_al_parto_rdx$anno), max(età_media_al_parto_rdx$anno), by = 1),
    limits = c(min(età_media_al_parto_rdx$anno), max(età_media_al_parto_rdx$anno)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Parma" = burg_md,
      "Emilia-Romagna" = grn_md,
      "ITALIA" = blu_md
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = rel(0.3)),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
    axis.text.y = element_text(size = rel(0.85)),
    axis.title = element_text(size = rel(1), face = "bold"),
    plot.title = element_text(
      size = rel(1.3),
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      lineheight = 1.2,
      margin = margin(b = 10)
    ),
    strip.text = element_text(size = rel(1.1), face = "bold"),
    legend.text = element_text(size = rel(0.9)),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Trend demografici: età_media_al_parto",
    subtitle = "Indicatore espresso in: anni e decimi di anno",
    caption = CAP,
    x = "",
    y = ""
  )

saveRDS(object = p08_età_media_al_parto, file = here::here("data", "plots", "p08_età_media_al_parto.rds"))

# Plot: p09_quoziente_di_mortalità ----
# Load & prep data
quoziente_di_mortalità <- readRDS(fs::path(dir_rds, "quoziente_di_mortalità.rds"))

quoziente_di_mortalità_rdx <- quoziente_di_mortalità |>
  dplyr::filter(
    territorio %in%
      c(
        "Bologna", "Piacenza", "Parma", "Reggio nell'Emilia",
        "Modena", "Ferrara", "Ravenna", "Forli'", "Rimini",
        "Emilia-Romagna", "NORD-EST", "ITALIA"
      )
  ) |>
  dplyr::mutate(
  )

# Plot
p09_quoziente_di_mortalità <- quoziente_di_mortalità_rdx |>
  dplyr::filter(indicatore == "quoziente_di_mortalità") |>
  filter(territorio != "NORD-EST", territorio != "Altro") |>
  ggplot(aes(
    x = anno,
    y = valore,
    color = territorio,
    
    group = territorio
  )) +
  geom_line(
    aes(
      
      
    ),
    linewidth = rel(0.8)
  ) +
  geom_line(
    data = \(df) df |> filter(territorio %in% c("Parma", "Emilia-Romagna")),
    aes(
      
      
    ),
    linewidth = rel(1.5)
  ) +
  scale_x_continuous(
    breaks = seq(min(quoziente_di_mortalità_rdx$anno), max(quoziente_di_mortalità_rdx$anno), by = 1),
    limits = c(min(quoziente_di_mortalità_rdx$anno), max(quoziente_di_mortalità_rdx$anno)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Parma" = burg_md,
      "Emilia-Romagna" = grn_md,
      
      "ITALIA" = blu_md,
      
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = rel(0.3)),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
    axis.text.y = element_text(size = rel(0.85)),
    axis.title = element_text(size = rel(1), face = "bold"),
    plot.title = element_text(
      size = rel(1.3),
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      lineheight = 1.2,
      margin = margin(b = 10)
    ),
    strip.text = element_text(size = rel(1.1), face = "bold"),
    legend.text = element_text(size = rel(0.9)),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Trend demografici: quoziente_di_mortalità",
    subtitle = "Indicatore espresso in: rapporto num. decessi nell'anno e num. medio popolazione residente per 1.000",
    caption = CAP,
    x = "",
    y = ""
  )

saveRDS(object = p09_quoziente_di_mortalità, file = here::here("data", "plots", "p09_quoziente_di_mortalità.rds"))

# Plot: p10_quoziente_di_natalità ----
# Load & prep data
quoziente_di_natalità <- readRDS(fs::path(dir_rds, "quoziente_di_natalità.rds"))

quoziente_di_natalità_rdx <- quoziente_di_natalità |>
  dplyr::filter(
    territorio %in%
      c(
        "Bologna", "Piacenza", "Parma", "Reggio nell'Emilia",
        "Modena", "Ferrara", "Ravenna", "Forli'", "Rimini",
        "Emilia-Romagna", "NORD-EST", "ITALIA"
      )
  ) |>
  dplyr::mutate(
  )

# Plot
p10_quoziente_di_natalità <- quoziente_di_natalità_rdx |>
  dplyr::filter(indicatore == "quoziente_di_natalità") |>
  filter(territorio != "NORD-EST", territorio != "Altro") |>
  ggplot(aes(
    x = anno,
    y = valore,
    color = territorio,
    
    group = territorio
  )) +
  geom_line(
    aes(
      
      
    ),
    linewidth = rel(0.8)
  ) +
  geom_line(
    data = \(df) df |> filter(territorio %in% c("Parma", "Emilia-Romagna")),
    aes(
      
      
    ),
    linewidth = rel(1.5)
  ) +
  scale_x_continuous(
    breaks = seq(min(quoziente_di_natalità_rdx$anno), max(quoziente_di_natalità_rdx$anno), by = 1),
    limits = c(min(quoziente_di_natalità_rdx$anno), max(quoziente_di_natalità_rdx$anno)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Parma" = burg_md,
      "Emilia-Romagna" = grn_md,
      
      "ITALIA" = blu_md,
      
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = rel(0.3)),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
    axis.text.y = element_text(size = rel(0.85)),
    axis.title = element_text(size = rel(1), face = "bold"),
    plot.title = element_text(
      size = rel(1.3),
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      lineheight = 1.2,
      margin = margin(b = 10)
    ),
    strip.text = element_text(size = rel(1.1), face = "bold"),
    legend.text = element_text(size = rel(0.9)),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Trend demografici: quoziente_di_natalità",
    subtitle = "Indicatore espresso in: rapporto num. nati vivi nell'anno e num. medio popolazione residente per 1.000",
    caption = CAP,
    x = "",
    y = ""
  )

saveRDS(object = p10_quoziente_di_natalità, file = here::here("data", "plots", "p10_quoziente_di_natalità.rds"))

# Plot: p11_quoziente_di_nuzialità ----
# Load & prep data
quoziente_di_nuzialità <- readRDS(fs::path(dir_rds, "quoziente_di_nuzialità.rds"))

quoziente_di_nuzialità_rdx <- quoziente_di_nuzialità |>
  dplyr::filter(
    territorio %in%
      c(
        "Bologna", "Piacenza", "Parma", "Reggio nell'Emilia",
        "Modena", "Ferrara", "Ravenna", "Forli'", "Rimini",
        "Emilia-Romagna", "NORD-EST", "ITALIA"
      )
  ) |>
  dplyr::mutate(
  )

# Plot
p11_quoziente_di_nuzialità <- quoziente_di_nuzialità_rdx |>
  dplyr::filter(indicatore == "quoziente_di_nuzialità") |>
  filter(territorio != "NORD-EST", territorio != "Altro") |>
  ggplot(aes(
    x = anno,
    y = valore,
    color = territorio,
    
    group = territorio
  )) +
  geom_line(
    aes(
      
      
    ),
    linewidth = rel(0.8)
  ) +
  geom_line(
    data = \(df) df |> filter(territorio %in% c("Parma", "Emilia-Romagna")),
    aes(
      
      
    ),
    linewidth = rel(1.5)
  ) +
  scale_x_continuous(
    breaks = seq(min(quoziente_di_nuzialità_rdx$anno), max(quoziente_di_nuzialità_rdx$anno), by = 1),
    limits = c(min(quoziente_di_nuzialità_rdx$anno), max(quoziente_di_nuzialità_rdx$anno)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Parma" = burg_md,
      "Emilia-Romagna" = grn_md,
      
      "ITALIA" = blu_md,
      
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = rel(0.3)),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
    axis.text.y = element_text(size = rel(0.85)),
    axis.title = element_text(size = rel(1), face = "bold"),
    plot.title = element_text(
      size = rel(1.3),
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      lineheight = 1.2,
      margin = margin(b = 10)
    ),
    strip.text = element_text(size = rel(1.1), face = "bold"),
    legend.text = element_text(size = rel(0.9)),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Trend demografici: quoziente_di_nuzialità",
    subtitle = "Indicatore espresso in: rapporto num. matrimoni celebrati nell'anno e num. medio popolazione residente per 1.000",
    caption = CAP,
    x = "",
    y = ""
  )

saveRDS(object = p11_quoziente_di_nuzialità, file = here::here("data", "plots", "p11_quoziente_di_nuzialità.rds"))

# Plot: p12_saldo_migratorio_totale ----
# Load & prep data
saldo_migratorio_totale <- readRDS(fs::path(dir_rds, "saldo_migratorio_totale.rds"))

saldo_migratorio_totale_rdx <- saldo_migratorio_totale |>
  dplyr::filter(
    territorio %in%
      c(
        "Bologna", "Piacenza", "Parma", "Reggio nell'Emilia",
        "Modena", "Ferrara", "Ravenna", "Forli'", "Rimini",
        "Emilia-Romagna", "NORD-EST", "ITALIA"
      )
  ) |>
  dplyr::mutate(
  )

# Plot
p12_saldo_migratorio_totale <- saldo_migratorio_totale_rdx |>
  dplyr::filter(indicatore == "saldo_migratorio_totale") |>
  filter(territorio != "NORD-EST", territorio != "Altro") |>
  ggplot(aes(
    x = anno,
    y = valore,
    color = territorio,
    
    group = territorio
  )) +
  geom_line(
    aes(
      
      
    ),
    linewidth = rel(0.8)
  ) +
  geom_line(
    data = \(df) df |> filter(territorio %in% c("Parma", "Emilia-Romagna")),
    aes(
      
      
    ),
    linewidth = rel(1.5)
  ) +
  scale_x_continuous(
    breaks = seq(min(saldo_migratorio_totale_rdx$anno), max(saldo_migratorio_totale_rdx$anno), by = 1),
    limits = c(min(saldo_migratorio_totale_rdx$anno), max(saldo_migratorio_totale_rdx$anno)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Parma" = burg_md,
      "Emilia-Romagna" = grn_md,
      
      "ITALIA" = blu_md,
      
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = rel(0.3)),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
    axis.text.y = element_text(size = rel(0.85)),
    axis.title = element_text(size = rel(1), face = "bold"),
    plot.title = element_text(
      size = rel(1.3),
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      lineheight = 1.2,
      margin = margin(b = 10)
    ),
    strip.text = element_text(size = rel(1.1), face = "bold"),
    legend.text = element_text(size = rel(0.9)),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Trend demografici: saldo_migratorio_totale",
    subtitle = "Indicatore espresso in: differenza tra num. iscritti ed num. cancellati dai registri anagrafici per trasferimento di residenza",
    caption = CAP,
    x = "",
    y = ""
  )

saveRDS(object = p12_saldo_migratorio_totale, file = here::here("data", "plots", "p12_saldo_migratorio_totale.rds"))

# Plot: p13_saldo_migratorio_interno ----
# Load & prep data
saldo_migratorio_interno <- readRDS(fs::path(dir_rds, "saldo_migratorio_interno.rds"))

saldo_migratorio_interno_rdx <- saldo_migratorio_interno |>
  dplyr::filter(
    territorio %in%
      c(
        "Bologna", "Piacenza", "Parma", "Reggio nell'Emilia",
        "Modena", "Ferrara", "Ravenna", "Forli'", "Rimini",
        "Emilia-Romagna", "NORD-EST", "ITALIA"
      )
  ) |>
  dplyr::mutate(
  )

# Plot
p13_saldo_migratorio_interno <- saldo_migratorio_interno_rdx |>
  dplyr::filter(indicatore == "saldo_migratorio_interno") |>
  filter(territorio != "NORD-EST", territorio != "Altro") |>
  ggplot(aes(
    x = anno,
    y = valore,
    color = territorio,
    
    group = territorio
  )) +
  geom_line(
    aes(
      
      
    ),
    linewidth = rel(0.8)
  ) +
  geom_line(
    data = \(df) df |> filter(territorio %in% c("Parma", "Emilia-Romagna")),
    aes(
      
      
    ),
    linewidth = rel(1.5)
  ) +
  scale_x_continuous(
    breaks = seq(min(saldo_migratorio_interno_rdx$anno), max(saldo_migratorio_interno_rdx$anno), by = 1),
    limits = c(min(saldo_migratorio_interno_rdx$anno), max(saldo_migratorio_interno_rdx$anno)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Parma" = burg_md,
      "Emilia-Romagna" = grn_md,
      
      "ITALIA" = blu_md,
      
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = rel(0.3)),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
    axis.text.y = element_text(size = rel(0.85)),
    axis.title = element_text(size = rel(1), face = "bold"),
    plot.title = element_text(
      size = rel(1.3),
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      lineheight = 1.2,
      margin = margin(b = 10)
    ),
    strip.text = element_text(size = rel(1.1), face = "bold"),
    legend.text = element_text(size = rel(0.9)),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Trend demografici: saldo_migratorio_interno",
    subtitle = "Indicatore espresso in: differenza tra num. iscritti e num. cancellati per trasferimento di residenza da/verso altro Comune",
    caption = CAP,
    x = "",
    y = ""
  )

saveRDS(object = p13_saldo_migratorio_interno, file = here::here("data", "plots", "p13_saldo_migratorio_interno.rds"))

# Plot: p14_saldo_migratorio_con_l_estero ----
# Load & prep data
saldo_migratorio_con_l_estero <- readRDS(fs::path(dir_rds, "saldo_migratorio_con_l_estero.rds"))

saldo_migratorio_con_l_estero_rdx <- saldo_migratorio_con_l_estero |>
  dplyr::filter(
    territorio %in%
      c(
        "Bologna", "Piacenza", "Parma", "Reggio nell'Emilia",
        "Modena", "Ferrara", "Ravenna", "Forli'", "Rimini",
        "Emilia-Romagna", "NORD-EST", "ITALIA"
      )
  ) |>
  dplyr::mutate(
  )

# Plot
p14_saldo_migratorio_con_l_estero <- saldo_migratorio_con_l_estero_rdx |>
  dplyr::filter(indicatore == "saldo_migratorio_con_l_estero") |>
  filter(territorio != "NORD-EST", territorio != "Altro") |>
  ggplot(aes(
    x = anno,
    y = valore,
    color = territorio,
    
    group = territorio
  )) +
  geom_line(
    aes(
      
      
    ),
    linewidth = rel(0.8)
  ) +
  geom_line(
    data = \(df) df |> filter(territorio %in% c("Parma", "Emilia-Romagna")),
    aes(
      
      
    ),
    linewidth = rel(1.5)
  ) +
  scale_x_continuous(
    breaks = seq(min(saldo_migratorio_con_l_estero_rdx$anno), max(saldo_migratorio_con_l_estero_rdx$anno), by = 1),
    limits = c(min(saldo_migratorio_con_l_estero_rdx$anno), max(saldo_migratorio_con_l_estero_rdx$anno)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Parma" = burg_md,
      "Emilia-Romagna" = grn_md,
      
      "ITALIA" = blu_md,
      
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = rel(0.3)),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
    axis.text.y = element_text(size = rel(0.85)),
    axis.title = element_text(size = rel(1), face = "bold"),
    plot.title = element_text(
      size = rel(1.3),
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      lineheight = 1.2,
      margin = margin(b = 10)
    ),
    strip.text = element_text(size = rel(1.1), face = "bold"),
    legend.text = element_text(size = rel(0.9)),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Trend demografici: saldo_migratorio_con_l_estero",
    subtitle = stringr::str_wrap("Indicatore espresso in: rapporto tra il saldo migratorio con l'estero dell'anno e l'ammontare medio della popolazione residente per 1.000.", width = 120),
    caption = CAP,
    x = "",
    y = ""
  )

saveRDS(object = p14_saldo_migratorio_con_l_estero, file = here::here("data", "plots", "p14_saldo_migratorio_con_l_estero.rds"))

# Plot: p15_speranza_di_vita_0 ----
# Load & prep data
speranza_di_vita_0 <- readRDS(fs::path(dir_rds, "speranza_di_vita_0.rds"))

speranza_di_vita_0_rdx <- speranza_di_vita_0 |>
  dplyr::filter(sotto_categoria == "Maschi e femmine") |>  # Filter first
  dplyr::filter(
    territorio %in%
      c(
        "Bologna", "Piacenza", "Parma", "Reggio nell'Emilia",
        "Modena", "Ferrara", "Ravenna", "Forli'", "Rimini",
        "Emilia-Romagna", "NORD-EST", "ITALIA"
      )
  ) |>
  dplyr::mutate(
  )

# Plot
p15_speranza_di_vita_0 <- speranza_di_vita_0_rdx |>
  dplyr::filter(indicatore == "speranza_di_vita_0") |>
  filter(territorio != "NORD-EST", territorio != "Altro") |>
  ggplot(aes(
    x = anno,
    y = valore,
    color = territorio,
    
    group = territorio
  )) +
  geom_line(
    aes(
      
      
    ),
    linewidth = rel(0.8)
  ) +
  geom_line(
    data = \(df) df |> filter(territorio %in% c("Parma", "Emilia-Romagna")),
    aes(
      
      
    ),
    linewidth = rel(1.5)
  ) +
  scale_x_continuous(
    breaks = seq(min(speranza_di_vita_0_rdx$anno), max(speranza_di_vita_0_rdx$anno), by = 1),
    limits = c(min(speranza_di_vita_0_rdx$anno), max(speranza_di_vita_0_rdx$anno)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Parma" = burg_md,
      "Emilia-Romagna" = grn_md,
      
      "ITALIA" = blu_md,
      
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = rel(0.3)),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
    axis.text.y = element_text(size = rel(0.85)),
    axis.title = element_text(size = rel(1), face = "bold"),
    plot.title = element_text(
      size = rel(1.3),
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      lineheight = 1.2,
      margin = margin(b = 10)
    ),
    strip.text = element_text(size = rel(1.1), face = "bold"),
    legend.text = element_text(size = rel(0.9)),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Trend demografici: speranza_di_vita_0",
    subtitle = "Indicatore espresso in: numero medio di anni che restano da vivere a un neonato",
    caption = CAP,
    x = "",
    y = ""
  )

saveRDS(object = p15_speranza_di_vita_0, file = here::here("data", "plots", "p15_speranza_di_vita_0.rds"))

# Plot: p16_speranza_di_vita_65 ----
# Load & prep data
speranza_di_vita_65 <- readRDS(fs::path(dir_rds, "speranza_di_vita_65.rds"))

speranza_di_vita_65_rdx <- speranza_di_vita_65 |>
  dplyr::filter(sotto_categoria == "Maschi e femmine") |>  # Filter first
  dplyr::filter(
    territorio %in%
      c(
        "Bologna", "Piacenza", "Parma", "Reggio nell'Emilia",
        "Modena", "Ferrara", "Ravenna", "Forli'", "Rimini",
        "Emilia-Romagna", "NORD-EST", "ITALIA"
      )
  ) |>
  dplyr::mutate(
  )

# Plot
p16_speranza_di_vita_65 <- speranza_di_vita_65_rdx |>
  dplyr::filter(indicatore == "speranza_di_vita_65") |>
  filter(territorio != "NORD-EST", territorio != "Altro") |>
  ggplot(aes(
    x = anno,
    y = valore,
    color = territorio,
    
    group = territorio
  )) +
  geom_line(
    aes(
      
      
    ),
    linewidth = rel(0.8)
  ) +
  geom_line(
    data = \(df) df |> filter(territorio %in% c("Parma", "Emilia-Romagna")),
    aes(
      
      
    ),
    linewidth = rel(1.5)
  ) +
  scale_x_continuous(
    breaks = seq(min(speranza_di_vita_65_rdx$anno), max(speranza_di_vita_65_rdx$anno), by = 1),
    limits = c(min(speranza_di_vita_65_rdx$anno), max(speranza_di_vita_65_rdx$anno)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Parma" = burg_md,
      "Emilia-Romagna" = grn_md,
      
      "ITALIA" = blu_md,
      
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = rel(0.3)),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
    axis.text.y = element_text(size = rel(0.85)),
    axis.title = element_text(size = rel(1), face = "bold"),
    plot.title = element_text(
      size = rel(1.3),
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      lineheight = 1.2,
      margin = margin(b = 10)
    ),
    strip.text = element_text(size = rel(1.1), face = "bold"),
    legend.text = element_text(size = rel(0.9)),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Trend demografici: speranza_di_vita_65",
    subtitle = "Indicatore espresso in: numero medio di anni che restano da vivere a 65 anni",
    caption = CAP,
    x = "",
    y = ""
  )

saveRDS(object = p16_speranza_di_vita_65, file = here::here("data", "plots", "p16_speranza_di_vita_65.rds"))

# Plot: p17_tasso_di_crescita_totale ----
# Load & prep data
tasso_di_crescita_totale <- readRDS(fs::path(dir_rds, "tasso_di_crescita_totale.rds"))

tasso_di_crescita_totale_rdx <- tasso_di_crescita_totale |>
  dplyr::filter(
    territorio %in%
      c(
        "Bologna", "Piacenza", "Parma", "Reggio nell'Emilia",
        "Modena", "Ferrara", "Ravenna", "Forli'", "Rimini",
        "Emilia-Romagna", "NORD-EST", "ITALIA"
      )
  ) |>
  dplyr::mutate(
  )

# Plot
p17_tasso_di_crescita_totale <- tasso_di_crescita_totale_rdx |>
  dplyr::filter(indicatore == "tasso_di_crescita_totale") |>
  filter(territorio != "NORD-EST", territorio != "Altro") |>
  ggplot(aes(
    x = anno,
    y = valore,
    color = territorio,
    
    group = territorio
  )) +
  geom_line(
    aes(
      
      
    ),
    linewidth = rel(0.8)
  ) +
  geom_line(
    data = \(df) df |> filter(territorio %in% c("Parma", "Emilia-Romagna")),
    aes(
      
      
    ),
    linewidth = rel(1.5)
  ) +
  scale_x_continuous(
    breaks = seq(min(tasso_di_crescita_totale_rdx$anno), max(tasso_di_crescita_totale_rdx$anno), by = 1),
    limits = c(min(tasso_di_crescita_totale_rdx$anno), max(tasso_di_crescita_totale_rdx$anno)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Parma" = burg_md,
      "Emilia-Romagna" = grn_md,
      
      "ITALIA" = blu_md,
      
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = rel(0.3)),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
    axis.text.y = element_text(size = rel(0.85)),
    axis.title = element_text(size = rel(1), face = "bold"),
    plot.title = element_text(
      size = rel(1.3),
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      lineheight = 1.2,
      margin = margin(b = 10)
    ),
    strip.text = element_text(size = rel(1.1), face = "bold"),
    legend.text = element_text(size = rel(0.9)),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Trend demografici: tasso_di_crescita_totale",
    subtitle = "Indicatore espresso in: somma tasso di crescita naturale + tasso migratorio totale",
    caption = CAP,
    x = "",
    y = ""
  )

saveRDS(object = p17_tasso_di_crescita_totale, file = here::here("data", "plots", "p17_tasso_di_crescita_totale.rds"))

# Plot: p18_tasso_di_fecondità_totale ----
# Load & prep data
tasso_di_fecondità_totale <- readRDS(fs::path(dir_rds, "tasso_di_fecondità_totale.rds"))

tasso_di_fecondità_totale_rdx <- tasso_di_fecondità_totale |>
  dplyr::filter(
    territorio %in%
      c(
        "Bologna", "Piacenza", "Parma", "Reggio nell'Emilia",
        "Modena", "Ferrara", "Ravenna", "Forli'", "Rimini",
        "Emilia-Romagna", "NORD-EST", "ITALIA"
      )
  ) |>
  dplyr::mutate(
  )

# Plot
p18_tasso_di_fecondità_totale <- tasso_di_fecondità_totale_rdx |>
  dplyr::filter(indicatore == "tasso_di_fecondità_totale") |>
  filter(territorio != "NORD-EST", territorio != "Altro") |>
  ggplot(aes(
    x = anno,
    y = valore,
    color = territorio,
    
    group = territorio
  )) +
  geom_line(
    aes(
      
      
    ),
    linewidth = rel(0.8)
  ) +
  geom_line(
    data = \(df) df |> filter(territorio %in% c("Parma", "Emilia-Romagna")),
    aes(
      
      
    ),
    linewidth = rel(1.5)
  ) +
  scale_x_continuous(
    breaks = seq(min(tasso_di_fecondità_totale_rdx$anno), max(tasso_di_fecondità_totale_rdx$anno), by = 1),
    limits = c(min(tasso_di_fecondità_totale_rdx$anno), max(tasso_di_fecondità_totale_rdx$anno)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Parma" = burg_md,
      "Emilia-Romagna" = grn_md,
      
      "ITALIA" = blu_md,
      
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = rel(0.3)),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
    axis.text.y = element_text(size = rel(0.85)),
    axis.title = element_text(size = rel(1), face = "bold"),
    plot.title = element_text(
      size = rel(1.3),
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(0.95),
      lineheight = 1.2,
      margin = margin(b = 10)
    ),
    strip.text = element_text(size = rel(1.1), face = "bold"),
    legend.text = element_text(size = rel(0.9)),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Trend demografici: tasso_di_fecondità_totale",
    subtitle = "Indicatore espresso in: Numero medio di figli per donna in età fertile (15-49 anni)",
    caption = CAP,
    x = "",
    y = ""
  )

saveRDS(object = p18_tasso_di_fecondità_totale, file = here::here("data", "plots", "p18_tasso_di_fecondità_totale.rds"))

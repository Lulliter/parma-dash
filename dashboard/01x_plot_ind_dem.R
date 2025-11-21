# ---------- PULIZIA FILE EXCEL TREND DEMOGRAFICI ISTAT (2002-2024 per prov) ------------
# Plot each indicator over the years comparing the three territories in tidy_data_pr_er_it

# Packgs and functions ----
library(here)
library(fs)
library(dplyr)
library(tidyr)
library(readxl)
library(janitor)
library(skimr)
library(forcats)
library(glue)
library(ggplot2)
library(ggiraph)
library(patchwork) # for combining ggplots
# p1 + p2                    # Affiancati
# p1 / p2                    # Uno sopra l'altro
# p1 | p2                    # Affiancati (equivalente a +)
# (p1 + p2) / p3             # Layout complesso
# p1 + p2 + plot_layout(ncol = 2)  # Specificare colonne

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

# --- Load dataset 1) ----
indicatori_di_struttura <- readRDS(fs::path(
  dir_rds,
  "indicatori_di_struttura.rds"
))

tabyl(indicatori_di_struttura$indicatore)

# ---- Prepare data 1) for plotting (indicatori_di_struttura) ----

data <- indicatori_di_struttura |>
  dplyr::filter(
    territorio %in%
      c(
        "Bologna",
        "Piacenza",
        "Parma",
        "Reggio nell'Emilia",
        "Modena",
        "Ferrara",
        "Ravenna",
        "Forli'",
        "Rimini",
        "Emilia-Romagna",
        "NORD-EST",
        "ITALIA"
      )
  ) |>
  dplyr::mutate(
    highlight = territorio %in%
      c("Parma", "Emilia-Romagna", "NORD-EST", "ITALIA"),
    territorio_display = ifelse(highlight, territorio, "Altro")
  ) |>
  #maintain
  dplyr::mutate(
    territorio_display = factor(
      territorio_display,
      levels = c("Parma", "Emilia-Romagna", "NORD-EST", "ITALIA", "Altro")
    ),
    highlight = factor(highlight)
  )

sample_n(data, 10)

# 🟩🟪 SENZA Funzione Wrapper ------

# Arguments for plot
INDICATORE <- "Età media"
UDM <- "anni e decimi di anno"
TITLE <- glue("Trend demografici: {INDICATORE}")
SUBTIT <- glue("Indicatore espresso in: {UDM}")
CAP <- "Fonte: Istat, Demografia in cifre, Nov. 2025 | Rielaborazione: Fondazione Cariparma"

# ---- Make plot ----
# Plot: p01_e_m [= Età Media]  ----
p01_e_m <- data |>
  dplyr::filter(indicatore == INDICATORE) |>
  filter(territorio != "NORD-EST", territorio != "Altro") |>
  # plot
  ggplot(aes(
    x = anno,
    y = valore,
    color = territorio_display,
    alpha = highlight,
    group = territorio
  )) +
  # (FUNZIONA CON ggiraph)
  geom_line_interactive(
    aes(
      tooltip = glue(
        "Territorio: {gsub(\"'\", \"&apos;\", territorio)}\n{indicatore}: {round(valore,2)}"
      ),
      data_id = gsub("'", "", territorio)
    ),
    linewidth = rel(0.8)
  ) +
  # (FUNZIONA CON ggiraph) Linea più spessa per Parma ed Emilia-Romagna
  geom_line_interactive(
    data = \(df) df |> filter(territorio %in% c("Parma", "Emilia-Romagna")),
    aes(
      tooltip = glue(
        "Territorio: {gsub(\"'\", \"&apos;\", territorio)}\n{indicatore}: {round(valore,2)}"
      ),
      data_id = gsub("'", "", territorio)
    ),
    linewidth = rel(1.5)
  ) +
  # x axis
  scale_x_continuous(
    breaks = seq(min(data$anno), max(data$anno), by = 1),
    limits = c(min(data$anno), max(data$anno)),
    expand = expansion(mult = c(0.02, 0.02)) # Riduce lo spazio ai margini
  ) +
  # ????
  scale_alpha_manual(values = c(0.3, 1), guide = "none") +
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
    title = TITLE,
    subtitle = SUBTIT,
    caption = CAP,
    x = "",
    y = ""
  )

p01_e_m

# Save plot as .rds ----
saveRDS(object = p01_e_m, file = here::here("data", "plots", "p01_e_m.rds"))
# ggsave(
#   filename = fs::path(dir_plots, glue("p01_e_m",".png")),
#   plot = p01_e_m,
#   width = 8,
#   height = 7,
#   units = "in",
#   dpi = 300
#   )

# [POI] read back into env
# p01_e_m <-  readRDS(p01_e_m, here::here("dir_plots", "p01_e_m.rds"))

# 🟨🟩 CON Funzione Wrapper (Dataset 1) ------
# Presuppone un dataframe già pulito

# FUNZIONE che CREA + SALVARE plot con altri indicatori ---------
plot_indicatore_demografico <- function(
  indicatore,
  udm,
  dataset = NULL, # il df di partenza
  title = NULL,
  subtitle = NULL,
  caption = "Fonte: Istat, Demografia in cifre, Nov. 2025 | Rielaborazione: Fondazione Cariparma",
  save_plot = FALSE,
  file_name = NULL
) {
  # Se dataset non è fornito, usa 'data' dall'ambiente globale
  if (is.null(dataset)) {
    dataset <- data
  }

  # Genera title e subtitle se non forniti
  if (is.null(title)) {
    title <- glue("Trend demografici: {indicatore}")
  }
  if (is.null(subtitle)) {
    subtitle <- glue("Indicatore espresso in: {udm}")
  }

  # Crea il plot
  p <- dataset |>
    dplyr::filter(indicatore == !!indicatore) |>
    filter(territorio != "NORD-EST", territorio != "Altro") |>
    # plot
    ggplot(aes(
      x = anno,
      y = valore,
      color = territorio_display,
      alpha = highlight,
      group = territorio
    )) +
    geom_line_interactive(
      aes(
        tooltip = glue(
          "Territorio: {gsub(\"'\", \"&apos;\", territorio)}\n{indicatore}: {round(valore,2)}"
        ),
        data_id = gsub("'", "", territorio)
      ),
      linewidth = rel(0.8)
    ) +
    # Linea più spessa per Parma ed Emilia-Romagna
    geom_line_interactive(
      data = \(df) df |> filter(territorio %in% c("Parma", "Emilia-Romagna")),
      aes(
        tooltip = glue(
          "Territorio: {gsub(\"'\", \"&apos;\", territorio)}\n{indicatore}: {round(valore,2)}"
        ),
        data_id = gsub("'", "", territorio)
      ),
      linewidth = rel(1.5)
    ) +
    # x axis
    scale_x_continuous(
      breaks = seq(min(dataset$anno), max(dataset$anno), by = 1),
      limits = c(min(dataset$anno), max(dataset$anno)),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_alpha_manual(values = c(0.3, 1), guide = "none") +
    scale_color_manual(
      values = c(
        "Parma" = burg_md,
        "Emilia-Romagna" = grn_md,
        "NORD-EST" = blu_lg,
        "ITALIA" = blu_md,
        "Altro" = "grey50"
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
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = "",
      y = ""
    )

  # Salva il plot se richiesto
  if (save_plot) {
    if (is.null(file_name)) {
      file_name <- glue("plot_{janitor::make_clean_names(indicatore)}.rds")
    }
    saveRDS(object = p, file = here::here("data", "plots", file_name))
    message(glue("Plot salvato in: data/plots/{file_name}"))
  }

  return(p)
}


# ==============================================================================
# USO di FUNZIONE ----
# ==============================================================================
# # Plot:p01_e_m [ = Età Media] ----
# p01_e_m <- plot_indicatore_demografico(
#   indicatore = "Età Media",
#   udm = "Anni e decimi di anno.",
#   save_plot = TRUE,
#   file_name = "p01_e_m.rds"
# )
# p01_e_m

# Plot: p02_i_v [ = Indice di vecchiaia] ----
p02_i_v <- plot_indicatore_demografico(
  dataset = data,
  indicatore = "Indice di vecchiaia",
  udm = "% tra popolazione di 65+ anni e in età 0-14.",
  save_plot = TRUE,
  file_name = "p02_i_v.rds"
)
p02_i_v

# Plot: p04_i_d_a  [= Indice di dipendenza anziani] ----
p04_i_d_a <- plot_indicatore_demografico(
  dataset = data,
  indicatore = "Indice di dipendenza anziani",
  udm = "% tra popolazione di 65+ e in età attiva (15-64 anni)",
  save_plot = TRUE,
  file_name = "p04_i_d_a.rds"
)
p04_i_d_a

# Plot: p03_i_d_s [ = Indice di dipendenza strutturale] ----
p03_i_d_s <- plot_indicatore_demografico(
  dataset = data,
  indicatore = "Indice di dipendenza strutturale",
  udm = "% tra popolazione in età non attiva (0-14 e 65+ anni) e in età attiva (15-64 anni)",
  save_plot = TRUE,
  file_name = "p03_i_d_s.rds"
)
p03_i_d_s

# 🟨🟩 CON Funzione Wrapper (Dataset 2)------
# --- Load dataset 2) ----
indicatori_struttura_pop <- readRDS(fs::path(
  dir_rds,
  "indicatori_struttura_popolazione.rds"
))

# ---- Prepare data 2) for plotting (indicatori_di_struttura) ----
data2 <- indicatori_struttura_pop |>
  dplyr::filter(
    territorio %in%
      c(
        "Bologna",
        "Piacenza",
        "Parma",
        "Reggio nell'Emilia",
        "Modena",
        "Ferrara",
        "Ravenna",
        "Forli'",
        "Rimini",
        "Emilia-Romagna",
        "NORD-EST",
        "ITALIA"
      )
  ) |>
  dplyr::mutate(
    highlight = territorio %in%
      c("Parma", "Emilia-Romagna", "NORD-EST", "ITALIA"),
    territorio_display = ifelse(highlight, territorio, "Altro")
  ) |>
  #maintain
  dplyr::mutate(
    territorio_display = factor(
      territorio_display,
      levels = c("Parma", "Emilia-Romagna", "NORD-EST", "ITALIA", "Altro")
    ),
    highlight = factor(highlight)
  )

sample_n(data2, 10)


# 🟨🟩 CON Funzione Wrapper ------
# Arguments for next 3 plots ----
tabyl(indicatori_struttura_pop$indicatore)

CAP <- "Fonte: Istat, Demografia in cifre, Nov. 2025 | Rielaborazione: Fondazione Cariparma"
# OTUPUT_DIR_DEFAULT = "data", "plots", file_name

# Plot: p06_0_14_anni [% classe di età 0-14 anni] ----
p06_0_14_anni <- plot_indicatore_demografico(
  dataset = data2,
  indicatore = "0-14 anni",
  udm = "% della popolazione per classe di età",
  title = NULL,
  subtitle = NULL,
  caption = CAP,
  save_plot = TRUE,
  file_name = "p06_0_14_anni.rds"
)

p06_0_14_anni

# Plot: p06_15_64_anni [% classe di età 15-64 anni] ----
p06_15_64_anni <- plot_indicatore_demografico(
  dataset = data2,
  indicatore = "15-64 anni",
  udm = "% della popolazione per classe di età",
  title = NULL,
  subtitle = NULL,
  caption = CAP,
  save_plot = TRUE,
  file_name = "p06_15_64_anni.rds"
)

p06_15_64_anni

# Plot: p06_65piu_anni [% classe di età 65 anni e oltre] ----
p06_65piu_anni <- plot_indicatore_demografico(
  dataset = data2,
  indicatore = "65 anni e oltre",
  udm = "% della popolazione per classe di età",
  title = NULL,
  subtitle = NULL,
  caption = CAP,
  save_plot = TRUE,
  file_name = "p06_65piu_anni.rds"
)

p06_65piu_anni

# 🟨🟩 CON Funzione Wrapper (Dataset 3 ecc )------
# Arguments for next n  plots ----
CAP <- "Fonte: Istat, Demografia in cifre, Nov. 2025 | Rielaborazione: Fondazione Cariparma"
# OTUPUT_DIR_DEFAULT = "data", "plots", file_name
# udm = da specificare ....

# ---- Load dataset 3 ecc ) ----
crescita_naturale <- readRDS(fs::path(
  dir_rds,
  "crescita_naturale.rds"
))

glimpse(crescita_naturale)
crescita_naturale$indicatore

# Preparazione dati per il plotting ----
data3 <- crescita_naturale |>
  dplyr::filter(
    territorio %in%
      c(
        "Bologna",
        "Piacenza",
        "Parma",
        "Reggio nell'Emilia",
        "Modena",
        "Ferrara",
        "Ravenna",
        "Forli'",
        "Rimini",
        "Emilia-Romagna",
        "NORD-EST",
        "ITALIA"
      )
  ) |>
  dplyr::mutate(
    highlight = territorio %in%
      c("Parma", "Emilia-Romagna", "NORD-EST", "ITALIA"),
    territorio_display = ifelse(highlight, territorio, "Altro")
  ) |>
  #maintain
  dplyr::mutate(
    territorio_display = factor(
      territorio_display,
      levels = c("Parma", "Emilia-Romagna", "NORD-EST", "ITALIA", "Altro")
    ),
    highlight = factor(highlight)
  )

# Plot
p07_crescita_naturale <- plot_indicatore_demografico(
  dataset = data3,
  indicatore = "crescita_naturale", # nome nel df
  udm = "differenza tra il tasso di natalità e il tasso di mortalità",
  title = NULL, # comune
  subtitle = NULL,
  caption = CAP, # comune
  save_plot = TRUE,
  file_name = "p07_crescita_naturale.rds"
)

p07_crescita_naturale

# Visualizzazioni incidenza disabilità (GALI) per anziani ----
# Questo script contiene le funzioni per creare grafici e mappe
# da includere nella dashboard Quarto
library(here)
library(ggplot2)
library(dplyr)
library(sf)
library(scales)

# TEMA PERSONALIZZATO ----
theme_gali <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "gray30"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11, face = "bold"),
      legend.position = "top",
      legend.title = element_text(size = 10, face = "bold"),
      panel.grid.minor = element_blank()
    )
}

# PALETTE COLORI ----
# Palette per solo anziani (2 classi)
colori_eta <- c(
  "65-74 anni" = "#4575b4", # Blu
  "75+ anni" = "#d73027" # Rosso
)

# Palette per tutte le età (4 classi) - gradiente intensità
colori_eta_completo <- c(
  "0-44 anni" = "#1a9850", # Verde (bassa incidenza)
  "45-64 anni" = "#fee08b", # Giallo
  "65-74 anni" = "#f46d43", # Arancio
  "75+ anni" = "#d73027" # Rosso (alta incidenza)
)

# LIST OF AVAILABLE CLEAN DATA SETS
gali_completo_geo <- readRDS(here(
  "data/data_out/istat_GALI_2023/gali_completo_geo.rds"
))
gali_anziani_geo <- readRDS(here(
  "data/data_out/istat_GALI_2023/gali_anziani_geo.rds"
))
gali_ripartizioni_completo <- readRDS(here(
  "data/data_out/istat_GALI_2023/gali_ripartizioni_completo.rds"
))
gali_ripartizioni <- readRDS(here(
  "data/data_out/istat_GALI_2023/gali_ripartizioni.rds"
))
gali_long_regioni_completo <- readRDS(here(
  "data/data_out/istat_GALI_2023/gali_long_regioni_completo.rds"
))
gali_long_regioni <- readRDS(here(
  "data/data_out/istat_GALI_2023/gali_long_regioni.rds"
))
gali_long_ripartizioni_completo <- readRDS(here(
  "data/data_out/istat_GALI_2023/gali_long_ripartizioni_completo.rds"
))
gali_long_ripartizioni <- readRDS(here(
  "data/data_out/istat_GALI_2023/gali_long_ripartizioni.rds"
))


# VISUALIZZAZIONE 1: OVERVIEW RIPARTIZIONI (Bar plot comparativo) ----
# Mostra confronto tra classi d'età per ripartizione
plot_ripartizioni_compare <- function(data) {
  # Scegli automaticamente palette in base al numero di classi età
  n_classi <- length(unique(data$classe_eta))
  palette <- if (n_classi <= 2) colori_eta else colori_eta_completo

  ggplot(
    data,
    aes(
      x = reorder(ripartizione, percentuale_disabilita),
      y = percentuale_disabilita,
      fill = classe_eta
    )
  ) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(values = palette) +
    scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
    coord_flip() +
    labs(
      title = "Incidenza disabilità nelle ripartizioni italiane",
      subtitle = "Percentuale popolazione con limitazioni gravi per classe d'età",
      x = NULL,
      y = "% popolazione con disabilità",
      fill = "Classe d'età",
      caption = "Fonte: ISTAT - Indagine Aspetti della vita quotidiana (2023)"
    ) +
    theme_gali()
}
# Esempio:
plot_ripartizioni_compare(gali_long_ripartizioni) # Solo anziani
plot_ripartizioni_compare(gali_long_ripartizioni_completo) # Tutte le età

# VISUALIZZAZIONE 2: REGIONI - Bar plot orizzontale ----
# Mostra tutte le regioni ordinate per incidenza, con facet per classe età
plot_regioni_facet <- function(data) {
  # Scegli automaticamente palette
  n_classi <- length(unique(data$classe_eta))
  palette <- if (n_classi <= 2) colori_eta else colori_eta_completo

  ggplot(
    data,
    aes(
      x = reorder(territorio, percentuale_disabilita),
      y = percentuale_disabilita,
      fill = classe_eta
    )
  ) +
    geom_col(show.legend = FALSE) +
    geom_text(
      aes(label = sprintf("%.1f%%", percentuale_disabilita)),
      hjust = -0.1,
      size = 3
    ) +
    scale_fill_manual(values = palette) +
    scale_y_continuous(
      labels = label_percent(scale = 1, accuracy = 1),
      limits = c(0, max(data$percentuale_disabilita) * 1.15)
    ) +
    coord_flip() +
    facet_wrap(~classe_eta, ncol = 2) +
    labs(
      title = "Incidenza disabilità per regione",
      subtitle = "Percentuale popolazione con limitazioni gravi (GALI)",
      x = NULL,
      y = "% popolazione con disabilità",
      caption = "Fonte: ISTAT - Indagine Aspetti della vita quotidiana (2023)"
    ) +
    theme_gali() +
    theme(
      strip.text = element_text(face = "bold", size = 11),
      strip.background = element_rect(fill = "gray90", color = NA)
    )
}
# Esempio:
plot_regioni_facet(gali_long_regioni) # Solo anziani
plot_regioni_facet(gali_long_regioni_completo) # Tutte le età

# VISUALIZZAZIONE 3: REGIONI - Confronto diretto 65-74 vs 75+ ----
# Lollipop chart che evidenzia il gap tra le due classi
# Richiede: data in formato wide con colonne eta_65_74 e eta_75_plus
plot_regioni_gap <- function(data) {
  # Calcoliamo il gap
  data_gap <- data %>%
    mutate(gap = eta_75_plus - eta_65_74) %>%
    arrange(desc(eta_75_plus))

  ggplot(data_gap, aes(y = reorder(territorio, eta_75_plus))) +
    # Segmento che collega i due valori
    geom_segment(
      aes(x = eta_65_74, xend = eta_75_plus, yend = territorio),
      color = "gray60",
      linewidth = 1
    ) +
    # Punti per 65-74
    geom_point(aes(x = eta_65_74), color = colori_eta["65-74 anni"], size = 3) +
    # Punti per 75+
    geom_point(aes(x = eta_75_plus), color = colori_eta["75+ anni"], size = 4) +
    # Etichette gap
    geom_text(
      aes(x = eta_75_plus + 1, label = sprintf("+%.1f", gap)),
      size = 3,
      color = "gray30"
    ) +
    scale_x_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
    labs(
      title = "Confronto incidenza disabilità: 65-74 vs 75+ anni",
      subtitle = "Il gap evidenzia l'aumento di disabilità con l'età avanzata",
      x = "% popolazione con disabilità (limitazioni gravi)",
      y = NULL,
      caption = "Fonte: ISTAT (2023). Linea collega le due classi d'età per ogni regione"
    ) +
    theme_gali()
}
# Esempio:
plot_regioni_gap(gali_anziani_geo) # Formato wide con eta_65_74 e eta_75_plus

# VISUALIZZAZIONE 4: TOP/BOTTOM REGIONS ----
# Evidenzia le regioni con maggiore/minore incidenza
plot_top_bottom_regioni <- function(data, top_n = 5, classe = "75+ anni") {
  data_filtered <- data %>%
    filter(classe_eta == classe) %>%
    arrange(desc(percentuale_disabilita)) %>%
    mutate(
      gruppo = case_when(
        row_number() <= top_n ~ "Top incidenza",
        row_number() > (n() - top_n) ~ "Bassa incidenza",
        TRUE ~ "Medio"
      )
    ) %>%
    filter(gruppo != "Medio")

  ggplot(
    data_filtered,
    aes(
      x = reorder(territorio, percentuale_disabilita),
      y = percentuale_disabilita,
      fill = gruppo
    )
  ) +
    geom_col() +
    geom_text(
      aes(label = sprintf("%.1f%%", percentuale_disabilita)),
      hjust = -0.1,
      size = 3.5
    ) +
    scale_fill_manual(
      values = c("Top incidenza" = "#d73027", "Bassa incidenza" = "#4daf4a")
    ) +
    scale_y_continuous(
      labels = label_percent(scale = 1),
      limits = c(0, max(data_filtered$percentuale_disabilita) * 1.2)
    ) +
    coord_flip() +
    labs(
      title = sprintf(
        "Regioni con maggiore e minore incidenza disabilità (%s)",
        classe
      ),
      subtitle = sprintf(
        "Top e bottom %d regioni per %% limitazioni gravi",
        top_n
      ),
      x = NULL,
      y = "% popolazione con disabilità",
      fill = NULL,
      caption = "Fonte: ISTAT (2023)"
    ) +
    theme_gali() +
    theme(legend.position = "top")
}
# Esempio:
plot_top_bottom_regioni(gali_long_regioni, top_n = 5, classe = "75+ anni")
plot_top_bottom_regioni(
  gali_long_regioni_completo,
  top_n = 3,
  classe = "45-64 anni"
)

# VISUALIZZAZIONE 5: MAPPA REGIONALE ----
# Richiede shapefile delle regioni italiane
# Assumiamo che tu abbia già caricato un oggetto sf con le regioni

plot_mappa_regioni <- function(
  data,
  shp_regioni,
  classe_eta_col = "eta_75_plus",
  livello_gravita = "Limitazioni gravi"
) {
  # Filtra per livello gravità se presente la colonna gravita
  if ("gravita" %in% names(data)) {
    data <- data %>% filter(gravita == livello_gravita)
  }

  # Nome per il titolo
  classe_label <- case_when(
    classe_eta_col == "eta_75_plus" ~ "75+ anni",
    classe_eta_col == "eta_65_74" ~ "65-74 anni",
    classe_eta_col == "eta_45_64" ~ "45-64 anni",
    classe_eta_col == "eta_0_44" ~ "0-44 anni",
    TRUE ~ classe_eta_col
  )

  # Join dati GALI con shapefile
  mappa_data <- shp_regioni %>%
    left_join(
      data %>% select(territorio, all_of(classe_eta_col)),
      by = "territorio"
    )

  ggplot(mappa_data) +
    geom_sf(aes(fill = .data[[classe_eta_col]]), color = "white", size = 0.3) +
    scale_fill_gradient2(
      low = "#4575b4",
      mid = "#ffffbf",
      high = "#d73027",
      midpoint = median(data[[classe_eta_col]], na.rm = TRUE),
      labels = label_percent(scale = 1, accuracy = 0.1),
      name = "% disabilità"
    ) +
    labs(
      title = sprintf("Incidenza per regione (%s)", classe_label),
      subtitle = sprintf("%s - Dati ISTAT 2023", livello_gravita),
      caption = "Fonte: ISTAT - Indagine Aspetti della vita quotidiana"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = "gray30", hjust = 0.5),
      legend.position = "right"
    )
}
# Esempio (richiede shapefile regioni_ita):
regioni_ita <- readRDS(here("data/data_out/ITA_shp/regioni_ita_full.rds"))
gali_tutti <- readRDS(here(
  "data/data_out/istat_GALI_2023/gali_tutti_livelli_geo.rds"
))

# Solo limitazioni gravi (default)
plot_mappa_regioni(gali_anziani_geo, regioni_ita, "eta_75_plus")

# Altri livelli di gravità
plot_mappa_regioni(
  gali_tutti,
  regioni_ita,
  "eta_75_plus",
  "Limitazioni non gravi"
)
plot_mappa_regioni(gali_tutti, regioni_ita, "eta_65_74", "Senza limitazioni")
plot_mappa_regioni(gali_tutti, regioni_ita, "eta_45_64", "Non indicato")

# VISUALIZZAZIONE 6: HEATMAP Regioni × Classi età ----
# Griglia che mostra tutte le regioni e fasce d'età contemporaneamente
plot_heatmap_regioni <- function(data) {
  ggplot(
    data,
    aes(
      x = classe_eta,
      y = reorder(territorio, percentuale_disabilita),
      fill = percentuale_disabilita
    )
  ) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(
      aes(label = sprintf("%.1f", percentuale_disabilita)),
      size = 3.5,
      fontface = "bold"
    ) +
    scale_fill_gradient2(
      low = "#4575b4",
      mid = "#ffffbf",
      high = "#d73027",
      midpoint = median(data$percentuale_disabilita, na.rm = TRUE),
      labels = label_percent(scale = 1, accuracy = 0.1),
      name = "% disabilità"
    ) +
    labs(
      title = "Heatmap: Incidenza disabilità per regione e classe d'età",
      subtitle = "Colori più intensi indicano maggiore incidenza",
      x = NULL,
      y = NULL,
      caption = "Fonte: ISTAT (2023)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "gray30"),
      axis.text = element_text(size = 10),
      legend.position = "right",
      panel.grid = element_blank()
    )
}
# Esempio:
plot_heatmap_regioni(gali_long_regioni) # 2 classi età
plot_heatmap_regioni(gali_long_regioni_completo) # 4 classi età

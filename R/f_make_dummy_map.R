library(sf)
library(dplyr)
library(ggplot2)
library(glue)

#' Crea una mappa tematica per una variabile dummy a livello comunale
#'
#' Data una geometria dei comuni e un layer con i confini provinciali,
#' rappresenta una variabile dicotomica come riempimento dei poligoni dei comuni
#' e, se richiesto, evidenzia il confine della provincia di Parma.
#'
#' @param df_comuni Oggetto `sf` con i poligoni dei comuni.
#' @param df_prov Oggetto `sf` con i poligoni delle province; deve contenere
#'   almeno la colonna `COD_PROV` se si usa `highlight_parma = TRUE`.
#' @param dummy_var Stringa con il nome della variabile dummy (0/1) presente
#'   in `df_comuni`.
#' @param title Titolo del grafico (facoltativo). Può contenere segnaposto
#'   interpretati da `glue::glue()`.
#' @param caption Didascalia del grafico (facoltativa). Può contenere segnaposto
#'   interpretati da `glue::glue()`.
#' @param palette Vettore di colori per i livelli della variabile dummy.
#'   Se `NULL`, viene utilizzata una palette di RColorBrewer.
#' @param brewer_palette Nome della palette RColorBrewer da usare quando
#'   `palette` è `NULL`.
#' @param na_col Colore per i valori mancanti.
#' @param prov_color Colore dei confini provinciali.
#' @param comuni_color Colore dei confini comunali.
#' @param fill_alpha Valore tra 0 e 1 che controlla la trasparenza del
#'   riempimento dei poligoni comunali.
#' @param highlight_parma Se `TRUE`, aggiunge un layer che evidenzia il confine
#'   della provincia di Parma (COD_PROV == 34).
#' @param highlight_color Colore usato per evidenziare il confine di Parma.
#' @param highlight_lwd Spessore della linea per il confine evidenziato di Parma.
#' @param legend_width Larghezza (in caratteri) per andare a capo nelle etichette
#'   della legenda. Se `NULL`, viene usato il comportamento predefinito di ggplot2.
#'
#' @return Un oggetto `ggplot` contenente la mappa tematica.
#'
#' @examples
#' \dontrun{
#' # Esempio di palette personalizzata
#' blue_dummy_pal <- c("#d8e4f0", "#4d648d")  # chiaro-scuro blu
#' burg_md       <- "#922b3e"                 # borgogna medio
#'
#' # Mappa dei comuni litoranei in Emilia-Romagna
#' f_make_dummy_map(
#'   df_comuni       = comuni_er,
#'   df_prov         = prov_er,
#'   dummy_var       = "COM_LIT",   # deve esistere in df_comuni e valere 0/1
#'   title           = "Comuni litoranei (= {dummy_var})",
#'   caption         = "Fonte: ISTAT (2025) — Caratteristiche dei comuni, variabile {dummy_var}",
#'   palette         = blue_dummy_pal,  # due colori, uno per 0 e uno per 1
#'   fill_alpha      = 0.75,
#'   highlight_parma = TRUE,
#'   highlight_color = burg_md,
#'   legend_width    = 20
#' )
#' }
f_make_dummy_map <- function(df_comuni,                  # sf con i comuni
                             df_prov,                    # sf con le province (per i confini)
                             dummy_var,                  # nome della variabile dummy (stringa)
                             title         = NULL,
                             caption       = NULL,
                             palette       = NULL,       # vettore di colori (es. c("white", "darkred"))
                             brewer_palette = "Set2",    # nome palette RColorBrewer se palette = NULL
                             na_col  = "#f0f0f0",
                             prov_color = "#525252",     # colore confini province
                             comuni_color = "#737373",   # colore confini comuni
                             fill_alpha = 1,             # trasparenza riempimento dummy `dummy_var`
                             highlight_parma = FALSE,    # evidenzia confine PARMA
                             highlight_color = burg_md,
                             highlight_lwd   = 0.5,
                             legend_width    = NULL      # usare in palette col dummy_var
) {
  # ---- PREP
  # Usa glue solo se <ARG> specificato, altrimenti <ARG> non viene proprio messo
  if (!is.null(title))   title   <- glue(title)
  if (!is.null(caption)) caption <- glue(caption)
  
  df_plot <- df_comuni |>
    mutate(dummy_fac = factor(.data[[dummy_var]],
                              levels = c(0, 1),
                              labels = c("0 = No", "1 = Sì")))
  n_levels <- nlevels(df_plot$dummy_fac)  # dovrebbe essere 2 per dummy
  
  # funzione etichette legenda: se `legend_width = NULL`, lascia default
  label_fun <- if (is.null(legend_width)) {
    ggplot2::waiver()
  } else {
    function(x) stringr::str_wrap(x, width = legend_width)
  }
  
  # ---- PLOT
  p <- ggplot() +
    geom_sf(data = df_plot,
            aes(fill = dummy_fac),
            color = comuni_color,
            linewidth = 0.1,
            alpha = fill_alpha) +
    geom_sf(data = df_prov,
            fill = NA,
            color = prov_color,
            linewidth = 0.2) +
    coord_sf() +
    labs(title = title, caption = caption) +
    theme_minimal() +
    theme(
      axis.text    = element_blank(),
      axis.title   = element_blank(),
      axis.ticks   = element_blank(),
      panel.grid   = element_blank(),
      plot.caption = element_text(hjust = 0)
    )
  
  # Evidenzia il confine della Provincia di Parma (COD_PROV == 34) se richiesto
  if (isTRUE(highlight_parma)) {
    df_parma <- df_prov |>
      dplyr::filter(COD_PROV == 34)      # ER: 33 = Piacenza, 34 = Parma
    
    p <- p +
      geom_sf(data = df_parma,
              fill = NA,
              color = highlight_color,
              linewidth = highlight_lwd)
  }
  
  # Scala dei fill
  if (is.null(palette)) {
    p + scale_fill_brewer(
      palette  = brewer_palette,
      na.value = na_col,
      name     = NULL,
      labels   = label_fun
    )
  } else {
    if (length(palette) < n_levels) {
      stop("La palette personalizzata deve avere almeno ", n_levels, " colori.")
    }
    p + scale_fill_manual(
      values   = palette,
      na.value = na_col,
      name     = NULL,
      labels   = label_fun
    )
  }
}

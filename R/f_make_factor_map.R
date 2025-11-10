library(sf)
library(dplyr)
library(ggplot2)
library(glue)

#' Crea una mappa tematica per una variabile categorica a livello comunale
#'
#' Data una geometria dei comuni e un layer con i confini provinciali,
#' rappresenta una variabile categorica (factor) come riempimento dei poligoni
#' dei comuni e, se richiesto, evidenzia il confine della provincia di Parma.
#'
#' @param df_comuni Oggetto `sf` con i poligoni dei comuni.
#' @param df_prov Oggetto `sf` con i poligoni delle province; deve contenere
#'   almeno la colonna `COD_PROV` se si usa `highlight_parma = TRUE`.
#' @param factor_var Stringa con il nome della variabile categorica presente
#'   in `df_comuni`. Verrà convertita internamente in `factor`.
#' @param title Titolo del grafico (facoltativo). Se `NULL`, viene usato il nome
#'   della variabile `factor_var`. Può contenere segnaposto interpretati da
#'   `glue::glue()`.
#' @param caption Didascalia del grafico (facoltativa). Può contenere segnaposto
#'   interpretati da `glue::glue()`.
#' @param palette Vettore di colori per i livelli della variabile. Se `NULL`,
#'   viene utilizzata una palette di RColorBrewer.
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
#' # Esempio: mappa per una variabile a 3 livelli (es. classi di urbanizzazione)
#'
#' # Palette personalizzata per 3 categorie
#' urb_pal <- c("#d8e4f0", "#4d648d", "#c94c4c")
#'
#' # Assumendo che:
#' # - comuni_er sia un sf con i comuni
#' # - prov_er sia un sf con le province (coerente con comuni_er)
#' # - la variabile "CL_URB" codifichi la classe di urbanizzazione
#'
#' p_factor <- f_make_factor_map(
#'   df_comuni       = comuni_er,
#'   df_prov         = prov_er,
#'   factor_var             = "CL_URB",
#'   title           = "Classi di urbanizzazione (= {factor_var})",
#'   caption         = "Fonte: ISTAT (2025) — Caratteristiche dei comuni, variabile {factor_var}",
#'   palette         = urb_pal,   # un colore per ciascun livello del factor
#'   fill_alpha      = 0.9,
#'   highlight_parma = TRUE,
#'   highlight_color = "#922b3e",
#'   legend_width    = 25
#' )
#'
#' p_factor
#' }
f_make_factor_map <- function(df_comuni,
                              df_prov,
                              factor_var,
                              title          = NULL,
                              caption        = NULL,
                              palette        = NULL,     # vettore di colori c("","")
                              brewer_palette = "YlOrRd", # palette RColorBrewer se palette = NULL
                              na_col         = "#f0f0f0",
                              prov_color     = "#525252",
                              comuni_color   = "#bdbdbd",
                              fill_alpha     = 1,
                              highlight_parma = FALSE,
                              highlight_color = "#922b3e",
                              highlight_lwd   = 0.6,
                              legend_width    = NULL     # usare in palette col factor_var
) {
  if (is.null(title)) title <- factor_var
  if (!is.null(title))   title   <- glue(title)
  if (!is.null(caption)) caption <- glue(caption)
  
  # ---- PREP (la variabile sia trattata come factor)
  df_plot <- df_comuni |>
    mutate(var_fac = as.factor(.data[[factor_var]]))
  n_levels <- nlevels(df_plot$var_fac)
  
  # funzione etichette legenda: IF `legend_width = NULL`, lascia default
  label_fun <- if (is.null(legend_width)) {
    ggplot2::waiver()
  } else {
    function(x) stringr::str_wrap(x, width = legend_width)
  }
  
  # ---- PLOT
  p <- ggplot() +
    # Layer comuni
    geom_sf(
      data     = df_plot,
      aes(fill = var_fac),
      color    = comuni_color,
      linewidth = 0.1,
      alpha    = fill_alpha
    ) +
    # Layer province
    geom_sf(
      data     = df_prov,
      fill     = NA,
      color    = prov_color,
      linewidth = 0.2
    )
  
  # Layer HIGHLIGHT (IF `highlight_parma = TRUE`)
  if (isTRUE(highlight_parma)) {
    df_parma <- df_prov |>
      dplyr::filter(COD_PROV == 34)
    
    p <- p +
      geom_sf(
        data     = df_parma,
        fill     = NA,
        color    = highlight_color,
        linewidth = highlight_lwd
      )
  }
  
  # Tema e labels
  p <- p +
    coord_sf() +
    labs(title = title, caption = caption, fill = NULL) +
    theme_minimal() +
    theme(
      axis.text    = element_blank(),
      axis.title   = element_blank(),
      axis.ticks   = element_blank(),
      panel.grid   = element_blank(),
      plot.caption = element_text(hjust = 0)
    )
  
  # Se NON passi un vettore di colori, usa RColorBrewer
  if (is.null(palette)) {
    p + scale_fill_brewer(
      palette   = brewer_palette,
      na.value  = na_col,
      name      = NULL,
      direction = -1,        # inverti ordine
      labels    = label_fun  # wrap etichette legenda
    )
  } else {
    # palette = vettore di colori
    if (length(palette) < n_levels) {
      stop("La palette personalizzata deve avere almeno ", n_levels,
           " colori (livelli del fattore).")
    }
    p + scale_fill_manual(
      values   = palette,
      na.value = na_col,
      name     = NULL,
      labels   = label_fun   # wrap etichette legenda
    )
  }
}

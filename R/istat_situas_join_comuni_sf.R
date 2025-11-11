#' Join shapefile comuni ISTAT con caratteristiche SITUAS
#'
#' Esegue il join tra lo shapefile nazionale dei comuni (\code{sf})
#' e la tabella SITUAS delle caratteristiche territoriali,
#' utilizzando il codice alfanumerico \code{PRO_COM_T}.
#'
#' Mantiene la geometria \code{sf}, pulisce i nomi delle colonne
#' (.x / .y) e costruisce una versione ridotta, con le sole colonne
#' di codici e attributi principali, più una colonna di denominazione
#' italiana \code{COMUNE_it}.
#'
#' Opzionalmente salva la versione ridotta in un file \code{.rds},
#' in una cartella di output specificata.
#'
#' @param comuni_sf Oggetto \code{sf} con tutti i comuni d'Italia,
#'   deve contenere almeno le colonne \code{PRO_COM_T},
#'   \code{Shape_Leng}, \code{Shape_Area}, \code{geometry}.
#' @param situas_df Data frame/tibble con le caratteristiche SITUAS
#'   dei comuni, che deve contenere una colonna \code{PRO_COM_T}
#'   compatibile con quella dello shapefile.
#' @param out_dir Cartella in cui salvare la versione ridotta in
#'   formato \code{.rds}. Se \code{NULL}, non viene salvato nulla.
#' @param out_basename Nome base del file .rds (senza estensione),
#'   usato solo se \code{out_dir} non è \code{NULL}.
#'   Default: \code{"comuni_ita_info_redux_sf"}.
#' @param keep_cols Vettore di nomi di colonne da mantenere nella
#'   versione ridotta. Se \code{NULL}, viene usato un set di default
#'   (codici territoriali principali, attributi SITUAS essenziali,
#'   area/perimetro, geometria).
#'
#' @return Una lista con:
#'   \itemize{
#'     \item \code{comuni_info_sf}       — join completo (sf)
#'     \item \code{comuni_info_redux_sf} — versione ridotta (sf)
#'   }
#'
#' @examples
#' \dontrun{
#' res <- istat_situas_join_comuni_sf(
#'   comuni_sf      = comuni_ita,
#'   situas_df      = com_caratt,
#'   out_dir        = here::here("data","data_out"),
#'   out_basename   = "comuni_ita_info_redux_sf"
#' )
#'
#' str(res$comuni_info_redux_sf)
#' }
#'
#' @export
istat_situas_join_comuni_sf <- function(comuni_sf,
                                        situas_df,
                                        out_dir      = NULL,
                                        out_basename = "comuni_ita_info_redux_sf",
                                        keep_cols    = NULL) {
  # 1) Join sf (comuni_sf a sinistra per mantenere la geometria)
  comuni_info_sf <- dplyr::left_join(
    comuni_sf,
    situas_df,
    by = "PRO_COM_T"
  ) |>
    dplyr::select(-dplyr::ends_with(".y")) |>
    dplyr::rename_with(
      ~ sub("\\.x$", "", .x),
      dplyr::ends_with(".x")
    ) |>
    dplyr::relocate(c("Shape_Leng", "Shape_Area"), .before = "geometry")
  
  # 2) Colonne da tenere nella versione ridotta
  if (is.null(keep_cols)) {
    keep_cols <- c(
      "COD_RIP", "COD_REG", "COD_UTS",
      "COD_PROV_STORICO", "COD_PROV", "COD_CM", "CC_UTS",
      "PRO_COM_T", "PRO_COM", "COMUNE", "COMUNE_A",
      # "SIGLA_AUTOMOBILISTICA",
      "COM_ISO", "COM_LIT", "ZONA_ALT", "ZONE_COST_2021", "DEGURBA_2021",
      "Shape_Leng", "Shape_Area", "geometry"
    )
  }
  
  # 3) Versione ridotta + COMUNE_it
  comuni_info_redux_sf <- comuni_info_sf |>
    dplyr::select(dplyr::all_of(keep_cols)) |>
    dplyr::mutate(
      COMUNE_it = stringr::str_trim(
        stringr::str_split_fixed(COMUNE, "/", 2)[, 1]
      )
    ) |>
    dplyr::relocate(COMUNE_it, .after = COMUNE)
  
  # 4) Salvataggio opzionale
  if (!is.null(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    out_file <- file.path(out_dir, paste0(out_basename, ".rds"))
    saveRDS(comuni_info_redux_sf, file = out_file)
  }
  
  # 5) Output
  list(
    comuni_info_sf       = comuni_info_sf,
    comuni_info_redux_sf = comuni_info_redux_sf
  )
}


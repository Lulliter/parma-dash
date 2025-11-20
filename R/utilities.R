# Salva tabella di descrizione variabili comuni ISTAT-SITUAS ----
#' Salva tabella di descrizione variabili comuni ISTAT-SITUAS
#'
#' @param df Tibble da salvare
#' @param out_path Percorso completo dell'RDS di output
#' @return Lo stesso tibble (invisibile)
#' @export
salva_vardesc_rds <- function(df, out_path) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(df, out_path)
  invisible(df)
}


# Salva tutti i dataframe del Global Environment come file RDS ----
library(here)
library(purrr)
library(stringr)
#' Salva tutti i dataframe del Global Environment come file RDS
#'
#' @param output_dir Directory di output (obbligatorio)
#' @param pattern Pattern regex per filtrare i nomi dei dataframe (default: NULL = tutti)
#'
#' @return Invisibile: vettore con i nomi dei dataframe salvati
#'
#' @examples
#' save_all_dataframes(here::here("data", "data_out", "istat_demo_2002_2024"))
#' save_all_dataframes("export/dati", pattern = "^dati_")
#'
save_all_dataframes <- function(output_dir, pattern = NULL) {
  
  # Creo la directory se non esiste
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  # Ottengo tutti i dataframe dal Global Environment
  df_list <- ls(.GlobalEnv) |>
    mget(envir = .GlobalEnv, inherits = FALSE, ifnotfound = list(NULL)) |>
    purrr::keep(is.data.frame)
  # Filtro per pattern (se specificato)
  if (!is.null(pattern)) {
    df_list <- df_list[stringr::str_detect(names(df_list), pattern)]
  }
  # Controllo se ci sono dataframe
  if (length(df_list) == 0) {
    message("Nessun dataframe trovato")
    return(invisible(character(0)))
  }
  # Salvo i dataframe
  saved <- character()
  purrr::iwalk(df_list, function(df, df_name) {
    file_path <- file.path(output_dir, paste0(df_name, ".rds"))
    # Salvataggio con gestione errori & warnings (base R)
    tryCatch({
      saveRDS(df, file_path)
      message("✓ ", df_name)
      saved <<- c(saved, df_name)
    }, error = function(e) {
      message("✗ ", df_name, ": ", e$message)
    })
  })
  # Messaggio finale
  message("\nSalvati ", length(saved), " su ", length(df_list), " dataframe")
  
  invisible(saved)
}


# ESEMPIO DI UTILIZZO ----

# save_all_dataframes(here::here("data", "data_out", "istat_demo_2002_2024"))
# save_all_dataframes("export/dati", pattern = "^dati_")
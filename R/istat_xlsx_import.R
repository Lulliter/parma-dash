# Packages ----
library(readxl)
library(tidyverse)

# 1/3) Importa fogli ISTAT (con 2 righe header) --------
#' Importa fogli ISTAT con struttura semplice (2 righe header)
#'
#' Gestisce fogli come: quoziente_di_natalità, quoziente_di_mortalità, ecc.
#' che hanno 2 righe di intestazione (riga 1 = titolo, riga 2 = anni)
#'
#' @param file_path Percorso al file Excel
#' @param sheet_name Nome del foglio da importare
#'
#' @return Tibble in formato tidy con colonne: territorio, tipo, anno, indicatore, valore
#'
#' @examples
#' dati_natalita <- import_istat_simple_sheet(
#'   file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx"),
#'   sheet_name = "quoziente_di_natalità"
#' )
#'
import_istat_simple_sheet <- function(file_path, sheet_name) {
  
  # Leggo il file senza header ----
  raw_data <- readxl::read_excel(
    path = file_path,
    sheet = sheet_name,
    col_names = FALSE
  )
  
  # Creo i nomi delle colonne dalla riga 2 (anni) ----
  col_names <- c(
    "territorio",
    as.character(unlist(raw_data[2, -1]))
  )
  
  # Assegno i nomi alle colonne
  names(raw_data) <- col_names
  
  # Rimuovo le intestazioni ----
  clean_data <- raw_data[-c(1:2), ]
  
  # Rimuovo righe vuote e note ----
  clean_data <- clean_data |>
    dplyr::filter(
      !is.na(territorio),
      territorio != "* Stima",
      territorio != "*Provvisorio"
    )
  
  # Converto colonne numeriche ----
  clean_data <- clean_data |>
    dplyr::mutate(
      dplyr::across(
        .cols = -territorio,
        .fns = as.numeric
      )
    )
  
  # Definisco territori da classificare ----
  regioni <- c(
    "Piemonte", "Valle d'Aosta", "Lombardia", "Trentino-Alto Adige",
    "Veneto", "Friuli-Venezia Giulia", "Liguria", "Emilia-Romagna",
    "Toscana", "Umbria", "Marche", "Lazio", "Abruzzo", "Molise",
    "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia", "Sardegna"
  )
  
  ripartizioni <- c(
    "RIPARTIZIONI", "NORD", "NORD-OVEST", "NORD-EST", 
    "CENTRO", "MEZZOGIORNO", "SUD", "ISOLE", "ITALIA"
  )
  
  # Aggiungo classificazione tipo territorio ----
  clean_data <- clean_data |>
    dplyr::mutate(
      tipo = dplyr::case_when(
        territorio %in% regioni ~ "regione",
        territorio %in% ripartizioni ~ "ripartizione",
        TRUE ~ "provincia"
      )
    )
  
  # Converto in formato long (tidy) ----
  tidy_data <- clean_data |>
    tidyr::pivot_longer(
      cols = -c(territorio, tipo),
      names_to = "anno",
      values_to = "valore"
    )
  
  # Pulisco anno e aggiungo indicatore ----
  tidy_data <- tidy_data |>
    dplyr::mutate(
      anno = stringr::str_remove_all(anno, "[\\*']+"),  # Rimuovo asterischi (singoli/multipli) e apostrofi
      anno = as.integer(anno),
      indicatore = sheet_name
    )
  
  # Rimuovo NA nei valori ----
  tidy_data <- tidy_data |>
    dplyr::filter(!is.na(valore))
  
  return(tidy_data)
}


# 1/3) ESEMPIO DI UTILIZZO ----

# dati_natalita <- import_istat_simple_sheet(
#   file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx"),
#   sheet_name = "quoziente_di_natalità"
# )
# 
# dati_mortalita <- import_istat_simple_sheet(
#   file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx"),
#   sheet_name = "quoziente_di_mortalità"
# )


# 🟧🟩🟦 -----

# 2/3) Importa foglio ISTAT (con 3 righe di header) --------
#' Importa foglio ISTAT "indicatori_di_struttura"
#'
#' Gestisce il foglio speciale "indicatori_di_struttura" che ha 3 righe di header
#' con anni ripetuti ogni 4 colonne e 4 indicatori per anno
#'
#' @param file_path Percorso al file Excel
#' @param sheet_name Nome del foglio (default = "indicatori_di_struttura")
#'
#' @return Tibble in formato tidy con colonne: territorio, tipo, anno, indicatore, valore
#'
#' @examples
#' dati_struttura <- import_istat_complex_sheet(
#'   file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx")
#' )
#'
import_istat_complex_sheet <- function(file_path, sheet_name = "indicatori_di_struttura") {
  
  # Leggo il file senza header ----
  raw_data <- readxl::read_excel(
    path = file_path,
    sheet = sheet_name,
    col_names = FALSE
  )
  
  # Estraggo le righe di intestazione ----
  years_row <- raw_data[2, ]       # Riga 2: anni (ripetuti ogni 4 colonne)
  indicators_row <- raw_data[3, ]  # Riga 3: nomi indicatori
  
  # Riempio gli anni mancanti (forward fill) ----
  years_filled <- as.list(years_row) |>
    purrr::accumulate(function(prev, curr) {
      if (is.na(curr)) prev else curr
    })
  
  # Creo i nomi delle colonne combinando anno_indicatore ----
  col_names <- c(
    "territorio",
    purrr::map2_chr(
      years_filled[-1],
      as.list(indicators_row)[-1],
      ~ paste0(.x, "_", .y)
    )
  )
  
  # Assegno i nomi alle colonne
  names(raw_data) <- col_names
  
  # Rimuovo le intestazioni ----
  clean_data <- raw_data[-c(1:3), ]
  
  # Rimuovo righe vuote e note ----
  clean_data <- clean_data |>
    dplyr::filter(
      !is.na(territorio),
      territorio != "* Stima"
    )
  
  # Converto colonne numeriche ----
  clean_data <- clean_data |>
    dplyr::mutate(
      dplyr::across(
        .cols = -territorio,
        .fns = as.numeric
      )
    )
  
  # Definisco territori da classificare ----
  regioni <- c(
    "Piemonte", "Valle d'Aosta", "Lombardia", "Trentino-Alto Adige",
    "Veneto", "Friuli-Venezia Giulia", "Liguria", "Emilia-Romagna",
    "Toscana", "Umbria", "Marche", "Lazio", "Abruzzo", "Molise",
    "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia", "Sardegna"
  )
  
  ripartizioni <- c(
    "RIPARTIZIONI", "NORD", "NORD-OVEST", "NORD-EST", 
    "CENTRO", "MEZZOGIORNO", "SUD", "ISOLE", "ITALIA"
  )
  
  # Aggiungo classificazione tipo territorio ----
  clean_data <- clean_data |>
    dplyr::mutate(
      tipo = dplyr::case_when(
        territorio %in% regioni ~ "regione",
        territorio %in% ripartizioni ~ "ripartizione",
        TRUE ~ "provincia"
      )
    )
  
  # Converto in formato long (tidy) ----
  tidy_data <- clean_data |>
    tidyr::pivot_longer(
      cols = -c(territorio, tipo),
      names_to = "anno_indicatore",
      values_to = "valore"
    )
  
  # Separo anno e indicatore ----
  tidy_data <- tidy_data |>
    tidyr::separate(
      anno_indicatore,
      into = c("anno", "indicatore"),
      sep = "_",
      extra = "merge"  # Per gestire indicatori con underscore nel nome
    )
  
  # Pulisco anno ----
  tidy_data <- tidy_data |>
    dplyr::mutate(
      anno = stringr::str_remove_all(anno, "[\\*']+"),  # Rimuovo asterischi (singoli/multipli) e apostrofi
      anno = as.integer(anno)
    )
  
  # Rimuovo NA nei valori ----
  tidy_data <- tidy_data |>
    dplyr::filter(!is.na(valore))
  
  return(tidy_data)
}


# 2/3) ESEMPIO DI UTILIZZO ----

# dati_struttura <- import_istat_complex_sheet(
#   file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx")
# )

# 🟧🟩🟦 -----

# 3/3) Importa foglio ISTAT con speranza di vita  --------
library(readxl)
library(tidyverse)
library(here)

#' Importa fogli ISTAT con struttura anno × sotto-categoria
#'
#' Gestisce fogli come speranza_di_vita che hanno 4 righe di header
#' con anni ripetuti e sotto-categorie (es. Maschi/Femmine)
#'
#' @param file_path Percorso al file Excel
#' @param sheet_name Nome del foglio da importare
#'
#' @return Tibble in formato tidy con colonne: territorio, tipo, anno, sotto_categoria, indicatore, valore
#'
#' @examples
#' dati_speranza <- import_istat_speranza(
#'   file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx"),
#'   sheet_name = "speranza_di_vita"
#' )
#'
import_istat_speranza <- function(file_path, sheet_name) {
  
  # Leggo il file senza header ----
  raw_data <- readxl::read_excel(
    path = file_path,
    sheet = sheet_name,
    col_names = FALSE
  )
  
  # Creo i nomi delle colonne combinando anno_categoria ----
  anni_row <- raw_data[2, ]
  categoria_row <- raw_data[3, ]
  
  anni_filled <- as.list(anni_row) |>
    purrr::accumulate(function(prev, curr) {
      if (is.na(curr)) prev else curr
    })
  
  col_names <- c(
    "territorio",
    purrr::map2_chr(
      anni_filled[-1],
      as.list(categoria_row)[-1],
      ~ paste0(.x, "_", .y)
    )
  )
  
  names(raw_data) <- col_names
  
  # Definisco territori da classificare ----
  regioni <- c(
    "Piemonte", "Valle d'Aosta", "Lombardia", "Trentino-Alto Adige",
    "Veneto", "Friuli-Venezia Giulia", "Liguria", "Emilia-Romagna",
    "Toscana", "Umbria", "Marche", "Lazio", "Abruzzo", "Molise",
    "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia", "Sardegna"
  )
  
  ripartizioni <- c(
    "RIPARTIZIONI", "NORD", "NORD-OVEST", "NORD-EST", 
    "CENTRO", "MEZZOGIORNO", "SUD", "ISOLE", "ITALIA"
  )
  
  # Pulizia e trasformazione in tidy format ----
  tidy_data <- raw_data[-c(1:4), ] |>
    dplyr::filter(
      !is.na(territorio),
      territorio != "* Stima",
      territorio != "*Provvisorio"
    ) |>
    dplyr::mutate(
      dplyr::across(.cols = -territorio, .fns = as.numeric)
    ) |>
    dplyr::mutate(
      tipo = dplyr::case_when(
        territorio %in% regioni ~ "regione",
        territorio %in% ripartizioni ~ "ripartizione",
        TRUE ~ "provincia"
      )
    ) |>
    tidyr::pivot_longer(
      cols = -c(territorio, tipo),
      names_to = "anno_categoria",
      values_to = "valore"
    ) |>
    tidyr::separate(
      anno_categoria,
      into = c("anno", "sotto_categoria"),
      sep = "_",
      extra = "merge"
    ) |>
    dplyr::mutate(
      anno = stringr::str_remove_all(anno, "[\\*']+"),
      anno = as.integer(anno),
      indicatore = sheet_name
    ) |>
    dplyr::filter(!is.na(valore))
  
  # Rimuovo oggetti intermedi dall'environment ----
  rm(raw_data, anni_row, categoria_row, anni_filled, col_names, 
     regioni, ripartizioni, envir = environment())
  
  return(tidy_data)
}


# ESEMPIO DI UTILIZZO ----

# dati_speranza <- import_istat_speranza(
#   file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx"),
#   sheet_name = "speranza_di_vita"
# )
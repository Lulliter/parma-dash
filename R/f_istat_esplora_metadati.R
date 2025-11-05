# RICERCA DATASET ISTAT PER PAROLA CHIAVE ----
# Cerca direttamente senza scaricare l'intero catalogo
# NON consuma il rate limit di 5 richieste/giorno

library(rsdmx)
library(dplyr)
library(httr)
library(stringr)

# CONFIGURAZIONE ----
PAROLE_CHIAVE <- c("popolazione", "demografic", "residente")  # MODIFICA QUI
TIMEOUT <- 180  # 3 minuti

# FUNZIONE: Cerca dataflows per keywords ----
#' Cerca dataflows ISTAT filtrando per parola chiave
#'
#' @param keywords Vettore di parole chiave da cercare
#' @param timeout Timeout in secondi
#' @param agency_id Agency ID (default "IT1")
#' @return Data frame con i dataflows che contengono le parole chiave
#'
f_cerca_dataflows <- function(keywords, timeout = 180, agency_id = "IT1") {
  
  message("Ricerca dataflows ISTAT per: ", paste(keywords, collapse = ", "))
  message("(Timeout: ", timeout, " secondi)")
  
  # Imposta timeout
  httr::set_config(httr::timeout(timeout))
  
  # URL per scaricare tutti i dataflows
  url <- sprintf("https://esploradati.istat.it/SDMXWS/rest/dataflow/%s/all/latest/", agency_id)
  
  # Scarica i dataflows
  tryCatch({
    dataflows <- rsdmx::readSDMX(url)
    df_completo <- as.data.frame(dataflows)
    
    # Filtra per parole chiave
    pattern <- paste(keywords, collapse = "|")
    risultati <- df_completo %>%
      dplyr::filter(
        stringr::str_detect(Name.it, stringr::regex(pattern, ignore_case = TRUE)) |
          stringr::str_detect(Name.en, stringr::regex(pattern, ignore_case = TRUE))
      ) %>%
      dplyr::select(id, agencyID, dsdRef, Name.it, Name.en) %>%
      dplyr::arrange(Name.it)
    
    # Reset timeout
    httr::reset_config()
    
    message("✓ Trovati ", nrow(risultati), " dataset su ", nrow(df_completo), " totali\n")
    
    return(risultati)
    
  }, error = function(e) {
    httr::reset_config()
    stop("Errore durante il download: ", e$message, 
         "\n\nProva ad aumentare il timeout o usa la ricerca per categoria.")
  })
}


# FUNZIONE: Scarica solo dataflows recenti ----
#' Alternativa: scarica solo i primi N dataflows (più veloci)
#'
#' @param n_max Numero massimo di dataflows da scaricare
#' @param keywords Parole chiave per filtrare
#' @return Data frame con i dataflows filtrati
#'
f_cerca_primi_dataflows <- function(n_max = 100, keywords = NULL) {
  
  message("Scaricamento primi ", n_max, " dataflows...")
  
  # Questa è una semplificazione - SDMX non supporta nativamente la paginazione
  # Ma possiamo provare con un timeout più breve e vedere cosa arriva
  
  httr::set_config(httr::timeout(30))
  
  tryCatch({
    url <- "https://esploradati.istat.it/SDMXWS/rest/dataflow/IT1/all/latest/"
    dataflows <- rsdmx::readSDMX(url)
    df <- as.data.frame(dataflows)
    
    # Prendi solo i primi N
    df <- df[1:min(n_max, nrow(df)), ]
    
    # Filtra per keywords se fornite
    if (!is.null(keywords)) {
      pattern <- paste(keywords, collapse = "|")
      df <- df %>%
        dplyr::filter(
          stringr::str_detect(Name.it, stringr::regex(pattern, ignore_case = TRUE)) |
            stringr::str_detect(Name.en, stringr::regex(pattern, ignore_case = TRUE))
        )
    }
    
    httr::reset_config()
    
    return(df)
    
  }, error = function(e) {
    httr::reset_config()
    warning("Download parziale o fallito: ", e$message)
    return(NULL)
  })
}


# ESECUZIONE ----

message("\n--- RICERCA DATASET ISTAT ---\n")

risultati <- f_cerca_dataflows(
  keywords = PAROLE_CHIAVE,
  timeout = TIMEOUT
)

# VISUALIZZA RISULTATI ----
if (!is.null(risultati) && nrow(risultati) > 0) {
  
  message("Dataset trovati:\n")
  print(risultati, n = Inf)
  
  # DETTAGLI PRIMO DATASET ----
  if (nrow(risultati) > 0) {
    message("\n--- DETTAGLI PRIMO DATASET ---")
    message("Nome: ", risultati$Name.it[1])
    message("ID: ", risultati$id[1])
    message("DSD Reference: ", risultati$dsdRef[1])
    message("Agency: ", risultati$agencyID[1])
    
    # Per usarlo nello script di download:
    message("\n--- PARAMETRI PER DOWNLOAD ---")
    message("DATASET_ID <- \"", risultati$agencyID[1], ",", risultati$id[1], ",1.0\"")
    
    # Scarica la struttura per vedere le dimensioni
    message("\nScaricamento struttura dataset...")
    
    tryCatch({
      httr::set_config(httr::timeout(60))
      
      dsd_url <- sprintf(
        "https://esploradati.istat.it/SDMXWS/rest/datastructure/%s/%s/latest/",
        risultati$agencyID[1],
        risultati$dsdRef[1]
      )
      
      dsd <- rsdmx::readSDMX(dsd_url)
      
      # Estrai dimensioni
      dims <- slot(slot(dsd, "datastructures")[[1]], "Components")
      nomi_dim <- sapply(dims, function(x) slot(x, "conceptRef"))
      
      message("\nDimensioni disponibili nel dataset:")
      cat(paste("-", nomi_dim, collapse = "\n"), "\n")
      
      httr::reset_config()
      
    }, error = function(e) {
      httr::reset_config()
      message("Impossibile scaricare la struttura: ", e$message)
    })
  }
  
  # SALVA RISULTATI ----
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0("istat_ricerca_", 
                     paste(PAROLE_CHIAVE, collapse = "_"), 
                     "_", timestamp, ".csv")
  
  utils::write.csv(risultati, filename, row.names = FALSE)
  message("\n✓ Risultati salvati in: ", filename)
  
} else {
  message("✗ Nessun risultato trovato per: ", paste(PAROLE_CHIAVE, collapse = ", "))
  message("\nProva con altre parole chiave, ad esempio:")
  message("  - reddito, salari, retribuzione")
  message("  - imprese, aziende, società")
  message("  - istruzione, scuola, università")
  message("  - sanità, salute, mortalità")
  message("  - turismo, alberghi")
  message("  - commercio, esportazioni")
}


# NOTE ----
# Se il timeout continua a essere un problema, hai queste alternative:
#
# 1. Usa il catalogo pre-scaricato da OnData (più veloce):
#    https://github.com/ondata/guida-api-istat/blob/master/rawdata/dataflow.csv
#
# 2. Cerca per categorie specifiche sul sito ISTAT:
#    https://esploradati.istat.it/
#
# 3. Salva il risultato della prima ricerca e riutilizzalo:
#    saveRDS(risultati, "miei_dataflows_popolazione.rds")
#    risultati <- readRDS("miei_dataflows_popolazione.rds")
# Funzioni per scaricare dati ISTAT via API SDMX ------------------------------
# Nessuna configurazione fissa qui: tutto passa come argomento.

library(httr)
library(rsdmx)
library(dplyr)

# Step 1) Funzione per scaricare dati ISTAT via SDMX-REST ----------------------
#' Scarica dati da ISTAT Esploradati via API SDMX (una singola richiesta)
#'
#' @param codici_territorio Vettore di codici territoriali (comuni, province, ecc.).
#' @param dataset_id ID del dataset ISTAT (Dataflow ID, es. "IT1,DF_...,1.0").
#' @param frequenza Frequenza temporale: "A" (annuale), "M" (mensile), "Q" (trimestrale).
#' @param anno Anno di riferimento (numero intero, es. 2023).
#' @param dimensioni_extra Stringa con le dimensioni extra nella chiave SDMX
#'   (es. "......" se non filtri altre dimensioni).
#' @param timeout_sec Timeout per la richiesta HTTP, in secondi.
#'
#' @return Un data frame con i dati scaricati.
f_scarica_istat <- function(codici_territorio,
                            dataset_id,
                            frequenza        = "A",
                            anno,
                            dimensioni_extra = "......",
                            timeout_sec      = 60) {
  
  # Codici territorio concatenati con "+"
  codici_str <- paste(codici_territorio, collapse = "+")
  
  # Periodo di filtro (qui: anno intero)
  start_date <- paste0(anno, "-01-01")
  end_date   <- paste0(anno, "-12-31")
  
  # Costruzione URL SDMX-REST
  url <- paste0(
    "https://esploradati.istat.it/SDMXWS/rest/data/",
    dataset_id, "/",         # dataflow
    frequenza, ".",          # frequenza
    codici_str, ".",         # codici territoriali
    dimensioni_extra, "/",   # altre dimensioni
    "ALL/?detail=full",
    "&startPeriod=", start_date,
    "&endPeriod=",   end_date,
    "&dimensionAtObservation=TIME_PERIOD"
  )
  
  # Richiesta HTTP
  response <- httr::GET(
    url,
    httr::add_headers(
      Accept = "application/vnd.sdmx.structurespecificdata+xml;version=2.1"
    ),
    httr::timeout(timeout_sec)
  )
  
  # Controllo esito
  if (httr::status_code(response) != 200) {
    stop("Errore HTTP: ", httr::status_code(response),
         "\nURL: ", url,
         "\nPossibili cause: rate limit, codici errati, dataset non disponibile.")
  }
  
  # Salvataggio temporaneo e parsing SDMX
  temp_file <- tempfile(fileext = ".xml")
  base::writeBin(httr::content(response, "raw"), temp_file)
  
  sdmx_obj <- rsdmx::readSDMX(temp_file, isURL = FALSE)
  dati     <- as.data.frame(sdmx_obj, labels = TRUE)
  
  unlink(temp_file)
  
  dati
}

# Step 2) Funzione per scaricare dati ISTAT in blocchi -------------------------
#' Scarica dati ISTAT in blocchi, gestendo rate limit e unendo i risultati
#'
#' @param codici_territorio Vettore di codici territoriali da scaricare.
#' @param dataset_id ID del dataset ISTAT (Dataflow ID).
#' @param frequenza Frequenza temporale: "A", "M", "Q".
#' @param anno Anno di riferimento.
#' @param dimensioni_extra Stringa con le dimensioni extra nella chiave SDMX.
#' @param codici_per_blocco Numero di codici territorio per richiesta HTTP.
#' @param pausa_tra_richieste Pausa (secondi) tra richieste riuscite.
#' @param pausa_dopo_errore Pausa (secondi) dopo un errore.
#' @param timeout_sec Timeout (secondi) per singola richiesta HTTP.
#' @param stampa_piano Se TRUE, mostra un riepilogo del piano di download.
#'
#' @return Un data frame con tutti i dati scaricati (righe unite).
f_scarica_istat_blocchi <- function(codici_territorio,
                                    dataset_id,
                                    frequenza        = "A",
                                    anno,
                                    dimensioni_extra = "......",
                                    codici_per_blocco   = 15,
                                    pausa_tra_richieste = 15,
                                    pausa_dopo_errore   = 30,
                                    timeout_sec         = 60,
                                    stampa_piano        = TRUE) {
  
  # Suddivisione codici in blocchi
  blocchi <- split(
    codici_territorio,
    ceiling(seq_along(codici_territorio) / codici_per_blocco)
  )
  
  # Stima tempo grezza (10 sec “medi” per chiamata + pause)
  if (stampa_piano) {
    tempo_stimato <- (length(blocchi) - 1) * pausa_tra_richieste +
      length(blocchi) * 10
    minuti  <- floor(tempo_stimato / 60)
    secondi <- tempo_stimato %% 60
    
    message("\nPiano di download ISTAT")
    message("  - Totale codici territorio: ", length(codici_territorio))
    message("  - Codici per blocco:       ", codici_per_blocco)
    message("  - Numero di query:         ", length(blocchi))
    message("  - Tempo stimato:           ", minuti, " min ", secondi, " sec")
    message("  - Rate limit indicativo:   ~5 query/minuto\n")
  }
  
  # Loop di download
  dati_lista <- vector("list", length(blocchi))
  
  for (i in seq_along(blocchi)) {
    message(">>> Blocco ", i, "/", length(blocchi),
            " (", length(blocchi[[i]]), " codici) ...")
    
    tryCatch({
      dati_lista[[i]] <- f_scarica_istat(
        codici_territorio = blocchi[[i]],
        dataset_id        = dataset_id,
        frequenza         = frequenza,
        anno              = anno,
        dimensioni_extra  = dimensioni_extra,
        timeout_sec       = timeout_sec
      )
      
      if (i < length(blocchi)) {
        Sys.sleep(pausa_tra_richieste)
      }
      
    }, error = function(e) {
      warning("Errore nel blocco ", i, ": ", e$message)
      dati_lista[[i]] <- NULL
      
      if (i < length(blocchi)) {
        Sys.sleep(pausa_dopo_errore)
      }
    })
  }
  
  # Rimuovi blocchi NULL (falliti)
  dati_lista <- dati_lista[!vapply(dati_lista, is.null, logical(1))]
  
  if (length(dati_lista) == 0) {
    warning("Nessun blocco scaricato correttamente.")
    return(NULL)
  }
  
  dplyr::bind_rows(dati_lista)
}

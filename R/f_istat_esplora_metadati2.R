# FUNZIONI PER RICERCA DATASET ISTAT =========================================
# Funzioni riusabili per cercare dataflows ISTAT tramite API SDMX.
#
# ATTENZIONE - RATE LIMIT:
# La piattaforma esploradati.istat.it ha un limite di 5 query al MINUTO per IP.
# Superato questo limite, l'IP viene bloccato per 1-2 GIORNI.
# 
# COSA SIGNIFICA:
# - Puoi fare 5 chiamate API diverse in un minuto senza problemi
# - Se ne fai 6 o più in meno di 60 secondi, rischi il blocco
# - Il blocco è per IP, quindi colpisce tutte le tue applicazioni
# 
# COME EVITARE IL BLOCCO:
# - Non chiamare queste funzioni in loop rapidi (usa Sys.sleep(15) tra chiamate)
# - Salva i risultati e riutilizzali (la funzione lo fa automaticamente)
# - Per esplorazioni iniziali, usa il portale web manuale
# 
# NOTA: Queste funzioni scaricano METADATI (elenco dataset), non dati veri.
# L'elenco completo dei dataflows è uno solo, quindi una singola chiamata
# è sufficiente per esplorare tutti i dataset disponibili.
# 
# RIFERIMENTI:
# - Endpoint attuale: https://esploradati.istat.it/SDMXWS (da ottobre 2022)
# - Vecchio endpoint: http://sdmx.istat.it/SDMXWS (in dismissione)
# - Documentazione: https://www.istat.it/classificazioni-e-strumenti/web-services-sdmx/
# =============================================================================




# FUNZIONE 1: Cerca dataflows ------------------------------------------------
#' Cerca dataflows ISTAT e filtra per keywords
#'
#' Scarica l'elenco completo dei dataflows ISTAT e li filtra in base
#' alle parole chiave fornite (case-insensitive, cerca in Name.it e Name.en).
#'
#' @param keywords Vettore di parole chiave da cercare
#' @param timeout Timeout in secondi (default da configurazione)
#' @param agency_id ID dell'agenzia ISTAT (default "IT1")
#' @param verbose Se TRUE mostra messaggi informativi (default TRUE)
#' 
#' @return Data frame con colonne: id, agencyID, dsdRef, Name.it, Name.en
#' 
#' @examples
#' \dontrun{
#' risultati <- f_cerca_dataflows(c("popolazione", "residente"))
#' }
#' 
f_cerca_dataflows <- function(keywords,
                              timeout = TIMEOUT,
                              agency_id = "IT1",
                              verbose = TRUE) {
  
  # Validazione input
  if (!is.character(keywords) || length(keywords) == 0) {
    stop("keywords deve essere un vettore di caratteri non vuoto")
  }
  
  if (verbose) {
    message("Ricerca dataflows per: ", paste(keywords, collapse = ", "))
  }
  
  # Configurazione timeout HTTP
  httr::set_config(httr::timeout(timeout))
  on.exit(httr::reset_config())
  
  # Costruzione URL API SDMX
  url <- sprintf(
    "https://esploradati.istat.it/SDMXWS/rest/dataflow/%s/all/latest/",
    agency_id
  )
  
  # Download e processing dataflows
  tryCatch({
    # Lettura dataflows tramite SDMX
    if (verbose) message("Download dataflows in corso...")
    dataflows <- rsdmx::readSDMX(url)
    df_completo <- as.data.frame(dataflows)
    
    if (verbose) {
      message(sprintf("Scaricati %d dataflows totali", nrow(df_completo)))
    }
    
    # Filtraggio per keywords
    pattern <- paste(keywords, collapse = "|")
    risultati <- df_completo |>
      dplyr::filter(
        stringr::str_detect(
          Name.it, 
          stringr::regex(pattern, ignore_case = TRUE)
        ) |
          stringr::str_detect(
            Name.en, 
            stringr::regex(pattern, ignore_case = TRUE)
          )
      ) |>
      dplyr::select(id, agencyID, dsdRef, Name.it, Name.en) |>
      dplyr::arrange(Name.it)
    
    # Check risultati
    if (nrow(risultati) == 0) {
      if (verbose) {
        message("Nessun dataset trovato per le parole chiave specificate")
      }
      return(risultati)
    }
    
    if (verbose) {
      message(sprintf("Trovati %d dataset corrispondenti", nrow(risultati)))
    }
    
    return(risultati)
    
  }, error = function(e) {
    stop(
      "Errore durante il download dei dataflows: ", e$message,
      "\nProva ad aumentare il timeout o verifica la connessione."
    )
  })
}


# FUNZIONE 2: Salva risultati -------------------------------------------------
#' Salva risultati ricerca in CSV e RDS
#'
#' Salva il data frame dei risultati in formato CSV (leggibile) e RDS (nativo R).
#' Genera automaticamente un timestamp nel nome file.
#'
#' @param risultati Data frame con i risultati della ricerca
#' @param output_dir Directory dove salvare i file (default da configurazione)
#' @param file_prefix Prefisso per i nomi file (default "istat_ricerca")
#' @param verbose Se TRUE mostra messaggi (default TRUE)
#' 
#' @return Lista con i path dei file salvati (invisibile)
#' 
#' @examples
#' \dontrun{
#' risultati <- f_cerca_dataflows(c("popolazione"))
#' f_salva_risultati(risultati)
#' }
#' 
f_salva_risultati <- function(risultati,
                              output_dir = DIR_OUTPUT,
                              file_prefix = PREFISSO_FILE,
                              verbose = TRUE) {
  
  # Validazione input
  if (!is.data.frame(risultati)) {
    stop("risultati deve essere un data frame")
  }
  
  if (nrow(risultati) == 0) {
    if (verbose) {
      message("Data frame vuoto, nessun file salvato")
    }
    return(invisible(NULL))
  }
  
  # Creazione directory output
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (verbose) message("Creata directory: ", output_dir)
  }
  
  # Generazione nome file base
  if (is.null(file_prefix)) {
    file_prefix <- "istat_ricerca"
  }
  timestamp <- format(Sys.time(), "%Y%m%d")
  base_filename <- sprintf("%s_%s", file_prefix, timestamp)
  
  # Salva CSV
  csv_path <- file.path(output_dir, paste0(base_filename, ".csv"))
  utils::write.csv(risultati, csv_path, row.names = FALSE)
  if (verbose) message("Salvato CSV: ", csv_path)
  
  # Salva RDS
  rds_path <- file.path(output_dir, paste0(base_filename, ".rds"))
  saveRDS(risultati, rds_path)
  if (verbose) message("Salvato RDS: ", rds_path)
  
  # Ritorna path dei file salvati
  invisible(list(csv = csv_path, rds = rds_path))
}

# COME USARE QUESTO SCRIPT ===================================================
#
# 1. Modifica i parametri nella sezione CONFIGURAZIONE PARAMETRI (righe 30-38) ----
# CONFIGURAZIONE PARAMETRI ====================================================
# Modifica questi parametri per personalizzare la ricerca

# Parole chiave per la ricerca (cercate in Name.it e Name.en)
PAROLE_CHIAVE <- c("popolazione", "censim", "residente")

# Timeout per le chiamate API (in secondi)
TIMEOUT <- 180

# Directory per salvare i risultati
DIR_OUTPUT <- "data/data_out"

# Prefisso nome file output (se NULL usa "istat_ricerca")
PREFISSO_FILE <- NULL

# =============================================================================

# 2. CERCA i dataflows ----
#    risultati <- f_cerca_dataflows(keywords = PAROLE_CHIAVE)
#
# 3. SALVA i risultati (opzionale) ----
#    f_salva_risultati(risultati, output_dir = DIR_OUTPUT, file_prefix = PREFISSO_FILE)
#
# 4. VISUALIZZA i risultati ----
#    print(risultati)
#    View(risultati)
#
# 5. (Opzionale) RICARICA risultati salvati senza rifare la query API ----
#    risultati <- readRDS("data/data_out/istat_ricerca_XXXXXX.rds")
#
# =============================================================================


# NOTE E ALTERNATIVE ----------------------------------------------------------
# 
# RATE LIMIT:
# - Limite: 5 query al minuto per IP
# - Penalità: blocco IP per 1-2 giorni se superato
# - Evita di chiamare queste funzioni in loop senza pause
# - I file salvati (RDS/CSV) permettono di riutilizzare i risultati
#
# ALTERNATIVE IN CASO DI PROBLEMI:
#
# 1. Usa i file salvati per evitare nuove query:
#    risultati <- readRDS("data/data_out/istat_ricerca_XXXXXX.rds")
#
# 2. Ricerca manuale sul portale (consigliato per esplorazioni iniziali):
#    https://esploradati.istat.it/
#
# 3. Per problemi persistenti, usa il portale web per esplorare
#    e poi scarica solo i dataset specifici che ti servono
#
# DOCUMENTAZIONE UFFICIALE:
# - Web Services SDMX: https://www.istat.it/classificazioni-e-strumenti/web-services-sdmx/
# - Endpoint: https://esploradati.istat.it/SDMXWS
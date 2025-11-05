# SCRIPT: Download dati ISTAT via API SDMX ----
# Descrizione: Script per scaricare dati dall'API SDMX di ISTAT [**IstatData**](https://esploradati.istat.it/)
# Limitazioni: Max 5 richieste al giorno per IP

# LIBRERIE ----
library(httr)      # Per chiamate HTTP/API
library(rsdmx)     # Per leggere file SDMX (data & metadata) local or remote sources
library(dplyr)     # Per manipolazione dati
library(here)      # Per path relativi al progetto

# CONFIGURAZIONE X RICHIESTA i----

# Parametri del dataset ISTAT ----
# IMPORTANTE: Questi parametri vanno modificati in base al dataset desiderato
# Trovare i codici su: https://esploradati.istat.it/

DATASET_ID <- "IT1,DF_DCSS_POP_DEMCITMIG_TV_2,1.0"  # ID del dataset
FREQUENZA <- "A"                                     # A=Annuale, M=Mensile, Q=Trimestrale
ANNO <- 2023                                         # Anno di riferimento

# Codici territoriali ----
# Per popolazione comunale, i codici sono nel formato "034001" (provincia + comune)
# Lista completa comuni di Parma con codici ISTAT 2023

LISTA_COMUNI_PARMA <- c(
  "034001",  # Albareto            _ 1)
  "034002",  # Bardi               _ 2)
  "034003",  # Bedonia             _ 3)
  "034004",  # Berceto             _ 4)
  "034005",  # Bore                _ 5)
  "034006",  # Borgo Val di Taro   _ 6)
  "034007",  # Busseto             _ 7)
  "034008",  # Calestano           _ 8)
  "034009",  # Collecchio          _ 9)
  "034010",  # Colorno             _ 10)
  "034011",  # Compiano            _ 11)
  "034012",  # Corniglio           _ 12)
  "034013",  # Felino              _ 13)
  "034014",  # Fidenza             _ 14)
  "034015",  # Fontanellato        _ 15)
  "034016",  # Fontevivo           _ 16)
  "034017",  # Fornovo di Taro     _ 17)
  "034018",  # Langhirano          _ 18)
  "034019",  # Lesignano de' Bagni _ 19)
  "034020",  # Medesano            _ 20)
  
  "034022",  # Monchio delle Corti _ 21)
  "034023",  # Montechiarugolo     _ 22)
  "034024",  # Neviano degli Arduini _ 23)
  "034025",  # Noceto              _ 24)
  "034026",  # Palanzano           _ 25)
  "034027",  # Parma (capoluogo)   _ 26)
  "034028",  # Pellegrino Parmense _ 27)
  
  "034030",  # Roccabianca         _ 28)
  "034031",  # Sala Baganza         _ 29)
  "034032",  # Salsomaggiore Terme   _ 30)
  "034033",  # San Secondo Parmense _ 31)
  "034035",  # Solignano            _ 32)
  "034036",  # Soragna              _ 33)
  "034038",  # Terenzo               _ 34)
  "034039",  # Tizzano Val Parma      _ 35)
  "034040",  # Tornolo               _ 36)
  "034041",  # Torrile               _ 37)
  "034042",  # Traversetolo          _ 38)
  "034044",  # Valmozzola           _ 39)
  "034045",  # Varano de' Melegari   _ 40)
  "034046",  # Varsi                _ 41)
  "034049",  # Sissa Trecasali      _ 42)
  "034050",  # Polesine Zibello     _ 43)
  "034051"   # Sorbolo Mezzani      _ 44) 
)

# Parametri di esecuzione ----
# RATE LIMIT ISTAT: Max 5 query al MINUTO per IP
# Se superi → blocco IP per 1-2 giorni
#
# Ogni blocco = 1 query HTTP
# Con 44 comuni di Parma, esempi di configurazione:
# - COMUNI_PER_BLOCCO = 44  → 1 query  (rischio timeout)
# - COMUNI_PER_BLOCCO = 15  → 3 query  (CONSIGLIATO)
# - COMUNI_PER_BLOCCO = 10  → 5 query  (limite esatto)
# - COMUNI_PER_BLOCCO = 5   → 9 query  (SUPERA IL LIMITE!)

COMUNI_PER_BLOCCO <- 15    # Numero di comuni da scaricare per richiesta
PAUSA_TRA_RICHIESTE <- 15  # Secondi di attesa tra richieste (per evitare rate limit)
PAUSA_DOPO_ERRORE <- 30   # Secondi di attesa dopo un errore
TIMEOUT_RICHIESTA <- 60   # Timeout per singola richiesta (secondi)

# Parametri di output ----
NOME_OUTPUT <- "istat_pop_com_PR_2023"
CARTELLA_OUTPUT <- "data","data_out"

# FUNZIONE: Download dati ISTAT ================================================

#' Scarica dati da ISTAT Esploradati via API SDMX
#'
#' @param codici_territorio Vettore di codici territoriali (es. comuni, province)
#' @param dataset_id ID del dataset ISTAT (trovabile su esploradati.istat.it)
#' @param frequenza Frequenza temporale: "A" (annuale), "M" (mensile), "Q" (trimestrale)
#' @param anno Anno di riferimento
#' @param timeout_sec Timeout per la richiesta HTTP (default 60 secondi)
#' @param dimensioni_extra Dimensioni aggiuntive nella query (default "......")
#'
#' @return Data frame con i dati scaricati
#' @details 
#' La funzione costruisce un URL seguendo lo standard SDMX-REST di ISTAT:
#' https://esploradati.istat.it/SDMXWS/rest/data/{DATAFLOW}/{KEY}/{PROVIDER}
#' 
#' Esempio URL risultante:
#' https://esploradati.istat.it/SDMXWS/rest/data/IT1,DF_DCSS_POP_DEMCITMIG_TV_2,1.0/A.034027....../ALL/
#'
f_scarica_istat <- function(codici_territorio, 
                            dataset_id = DATASET_ID,
                            frequenza = "A",
                            anno = ANNO,
                            timeout_sec = TIMEOUT_RICHIESTA,
                            dimensioni_extra = "......") {
  
  # Costruzione della chiave (KEY) per l'API SDMX
  # Formato: FREQ.TERRITORIO.DIMENSIONI_EXTRA./MISURA
  # I codici territorio vengono concatenati con "+" per richieste multiple
  codici_str <- paste(codici_territorio, collapse = "+")
  
  # Date di inizio e fine per filtrare il periodo
  start_date <- paste0(anno, "-01-01")
  end_date <- paste0(anno, "-12-31")
  
  # Costruzione URL completo seguendo lo standard SDMX-REST
  url <- paste0(
    "https://esploradati.istat.it/SDMXWS/rest/data/",  # Base endpoint
    dataset_id, "/",                                    # ID del dataflow
    frequenza, ".",                                     # Frequenza temporale
    codici_str, ".",                                    # Codici territorio (+ separati)
    dimensioni_extra, "/",                              # Altre dimensioni (es. età, sesso)
    "ALL/?detail=full",                                 # Tutte le misure, dettaglio completo
    "&startPeriod=", start_date,                        # Filtro periodo inizio
    "&endPeriod=", end_date,                            # Filtro periodo fine
    "&dimensionAtObservation=TIME_PERIOD"               # Dimensione temporale
  )
  
  # Esecuzione richiesta HTTP GET con header SDMX
  response <- httr::GET(
    url, 
    httr::add_headers(Accept = "application/vnd.sdmx.structurespecificdata+xml;version=2.1"),
    httr::timeout(timeout_sec)
  )
  
  # Controllo status HTTP (200 = OK)
  if (httr::status_code(response) != 200) {
    stop("Errore HTTP: ", httr::status_code(response), 
         "\nURL: ", url,
         "\nPossibili cause: rate limit, codici errati, dataset non disponibile")
  }
  
  # Salvataggio temporaneo del file XML SDMX
  # Il pacchetto rsdmx richiede un file fisico per il parsing
  temp_file <- tempfile(fileext = ".xml")
  base::writeBin(httr::content(response, "raw"), temp_file)
  
  # Parsing del file SDMX in oggetto R
  sdmx_obj <- rsdmx::readSDMX(temp_file, isURL = FALSE)
  
  # Conversione in data frame con etichette descrittive (labels = TRUE)
  dati <- as.data.frame(sdmx_obj, labels = TRUE)
  
  # Pulizia: rimozione file temporaneo
  unlink(temp_file)
  
  return(dati)
}


# ESECUZIONE: Download con gestione rate limit ----

# Suddivisione in blocchi ----
# Per rispettare il limite di 5 richieste/giorno e evitare timeout,
# suddividiamo la lista dei comuni in blocchi più piccoli
#
# Ogni chiamata a f_scarica_istat() = 1 query HTTP
# Quindi: numero di blocchi = numero di query
#
blocchi <- split(
  LISTA_COMUNI_PARMA, 
  ceiling(seq_along(LISTA_COMUNI_PARMA) / COMUNI_PER_BLOCCO)
)

# ATTENZIONE: Se il numero di richieste > 5/m, il download verrà bloccato dal rate limit!
# Calcola tempo stimato di esecuzione
tempo_stimato <- (length(blocchi) - 1) * PAUSA_TRA_RICHIESTE + length(blocchi) * 10
minuti <- floor(tempo_stimato / 60)
secondi <- tempo_stimato %% 60

# Mostra piano di download
message("Piano di download:")
message("  - Totale comuni: ", length(LISTA_COMUNI_PARMA))
message("  - Comuni per blocco: ", COMUNI_PER_BLOCCO)
message("  - Numero di query: ", length(blocchi))
message("  - Tempo stimato: ", minuti, " min ", secondi, " sec")
message("  - Rate limit: 5 query/minuto (pausa ", PAUSA_TRA_RICHIESTE, " sec tra query)\n")


# Loop di download ----
dati_lista <- list()

for (i in seq_along(blocchi)) {
  
  # Tentativo di download con gestione errori
  tryCatch({
    
    # Chiamata alla funzione di download
    dati_lista[[i]] <- f_scarica_istat(
      codici_territorio = blocchi[[i]], 
      anno = ANNO
    )
    
    # Pausa tra richieste per evitare sovraccarico server
    if (i < length(blocchi)) {
      Sys.sleep(PAUSA_TRA_RICHIESTE)
    }
    
  }, error = function(e) {
    
    # Gestione errori (es. rate limit, timeout, codici errati)
    warning("Errore blocco ", i, ": ", e$message)
    dati_lista[[i]] <- NULL
    
    # Pausa più lunga dopo errore
    if (i < length(blocchi)) {
      Sys.sleep(PAUSA_DOPO_ERRORE)
    }
  })
}



# POST-ELABORAZIONE: Unione e salvataggio dati ----

# Rimozione blocchi falliti (NULL)
dati_lista <- dati_lista[!sapply(dati_lista, is.null)]

# Unione di tutti i data frame in un unico dataset
if (length(dati_lista) > 0) {
  
  dati_finali <- dplyr::bind_rows(dati_lista)
  
  # Visualizzazione struttura dati
  dplyr::glimpse(dati_finali)
  
  # Salvataggio su disco ----
  # Creazione cartella output se non esiste
  if (!dir.exists(here::here(CARTELLA_OUTPUT))) {
    dir.create(here::here(CARTELLA_OUTPUT), recursive = TRUE)
  }
  
  # Salvataggio in formato RDS (nativo R, mantiene tipi di dato)
  path_rds <- here::here(CARTELLA_OUTPUT, paste0(NOME_OUTPUT, ".rds"))
  saveRDS(dati_finali, path_rds)
  
  # Salvataggio in formato CSV (universale, leggibile da Excel/Python)
  path_csv <- here::here(CARTELLA_OUTPUT, paste0(NOME_OUTPUT, ".csv"))
  utils::write.csv(dati_finali, path_csv, row.names = FALSE)
  
  # Salva anche l'oggetto nell'environment per uso immediato
  assign(NOME_OUTPUT, dati_finali, envir = .GlobalEnv)
  
  message("Download completato: ", nrow(dati_finali), " record da ", 
          length(unique(dati_finali$REF_AREA)), " comuni")
  
} else {
  
  # Nessun dato scaricato (tutti i blocchi falliti)
  warning("Download fallito. Possibili cause: rate limit, problemi di connessione, codici errati.")
}

# NOTE PER RIUTILIZZO SU ALTRI DATASET ISTAT ----
#
# Per scaricare altri dati da esploradati.istat.it:
#
# 1. Trova il dataset su https://esploradati.istat.it/
#
# 2. Nella pagina del dataset, clicca su "Informazioni" → "Dataflow ID"
#    Esempio: IT1,DF_DCSS_POP_DEMCITMIG_TV_2,1.0
#
# 3. Modifica la sezione CONFIGURAZIONE sopra:
#    - DATASET_ID: il Dataflow ID trovato
#    - FREQUENZA: A (annuale), M (mensile), Q (trimestrale)
#    - ANNO: anno di riferimento
#    - LISTA_COMUNI_PARMA: sostituisci con i tuoi codici territorio
#
# 4. Per trovare i codici territorio corretti:
#    - Vai su esploradati.istat.it
#    - Esplora il dataset desiderato
#    - I codici appaiono nel campo "Territorio" (es. "034027" per Parma)
#
# 5. Regola COMUNI_PER_BLOCCO in base alla complessità del dataset:
#    - Dataset semplici (poche variabili): 10-20 territori per blocco
#    - Dataset complessi (molte variabili): 3-5 territori per blocco
#
# 6. Se il parametro dimensioni_extra nella funzione f_scarica_istat 
#    deve essere modificato, consulta la documentazione SDMX del dataset.

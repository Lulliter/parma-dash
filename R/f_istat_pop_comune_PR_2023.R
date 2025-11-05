
# ___ ----
# Istat - Popolazione residente per classi di età (decennali), sesso e cittadinanza - comuni
# https://esploradati.istat.it/databrowser/#/it/censpop/categories/CPA_POP/DCSS_POP_DEMCITMIG_TV/IT1,DF_DCSS_POP_DEMCITMIG_TV_2,1.0
# METADATI: 

# Packgs ----
library(httr)
library(rsdmx)
library(dplyr)
library(here)

# LISTA TUTTI I COMUNI DI PARMA ----

# [034001] Albareto  
# [034002] Bardi  
# [034003] Bedonia  
# [034004] Berceto  
# [034005] Bore  
# [034006] Borgo Val di Taro  
# [034007] Busseto  
# [034008] Calestano  
# [034009] Collecchio  
# [034010] Colorno  
# [034011] Compiano  
# [034012] Corniglio  
# [034013] Felino  
# [034014] Fidenza  
# [034015] Fontanellato  
# [034016] Fontevivo  
# [034017] Fornovo di Taro  
# [034018] Langhirano  
# [034019] Lesignano de' Bagni  
# [034020] Medesano  
# [034022] Monchio delle Corti  
# [034023] Montechiarugolo  
# [034024] Neviano degli Arduini  
# [034025] Noceto  
# [034026] Palanzano  
# [034027] Parma  
# [034028] Pellegrino Parmense  
# [034050] Polesine Zibello  
# [034030] Roccabianca  
# [034031] Sala Baganza  
# [034032] Salsomaggiore Terme  
# [034033] San Secondo Parmense  
# [034049] Sissa Trecasali  
# [034035] Solignano  
# [034036] Soragna  
# [034051] Sorbolo Mezzani  
# [034038] Terenzo  
# [034039] Tizzano Val Parma  
# [034040] Tornolo  
# [034041] Torrile  
# [034042] Traversetolo  
# [034044] Valmozzola  
# [034045] Varano de' Melegari  
# [034046] Varsi  

# SCARICA TUTTI I COMUNI DI PARMA ----
comuni_parma <- c(
  "034001", "034002", "034003", "034004", "034005", "034006", "034007", 
  "034008", "034009", "034010", "034011", "034012", "034013", "034014", 
  "034015", "034016", "034017", "034018", "034019", "034020", 
  "034021", # che non esiste
  "034022", "034023", "034024", "034025", "034026", "034027", "034028", 
  "034029", # che non esiste
  "034030", "034031", "034032", "034033", "034034", "034035", 
  "034036", "034037", "034038", "034039", "034040", "034041", "034042", 
  "034043", "034044", "034045", "034046", "034048", "034049", "034050", 
  "034051"
)

# WRITE FUNC: Funzione per scaricare dati ISTAT ----
f_scarica_istat <- function(comuni, anno = 2023, timeout_sec = 60) {
  
  comuni_str <- paste(comuni, collapse = "+")
  start_date <- paste0(anno, "-01-01")
  end_date <- paste0(anno, "-12-31")
  
  url <- paste0(
    "https://esploradati.istat.it/SDMXWS/rest/data/",
    "IT1,DF_DCSS_POP_DEMCITMIG_TV_2,1.0/",
    "A.", comuni_str, "......./",
    "ALL/?detail=full",
    "&startPeriod=", start_date,
    "&endPeriod=", end_date,
    "&dimensionAtObservation=TIME_PERIOD"
  )
  
  response <- GET(
    url, 
    add_headers(Accept = "application/vnd.sdmx.structurespecificdata+xml;version=2.1"),
    timeout(timeout_sec)  # Timeout più lungo per evitare errori
  )
  
  if (status_code(response) != 200) {
    stop("Errore HTTP: ", status_code(response))
  }
  
  temp_file <- tempfile(fileext = ".xml")
  writeBin(content(response, "raw"), temp_file)
  sdmx_obj <- readSDMX(temp_file, isURL = FALSE)
  dati <- as.data.frame(sdmx_obj, labels = TRUE)
  unlink(temp_file)
  
  return(dati)
}


# SETUP: Dividi in x blocchi ----
n_per_blocco <- 5 # comuni per blocco   
blocchi <- split(comuni_parma, ceiling(seq_along(comuni_parma) / n_per_blocco))

cat("SCARICAMENTO DATI CENSIMENTO 2023\n")
cat(strrep("=", 50), "\n")
cat("Totale comuni:", length(comuni_parma), "\n")
cat("Numero di blocchi:", length(blocchi), "\n")

# RUN FUNC: Scarica ogni blocco ----
dati_lista <- list()

for (i in seq_along(blocchi)) {
  cat(sprintf("[%2d/%2d] Comuni: %s\n", 
              i, length(blocchi), 
              paste(blocchi[[i]], collapse = ", ")))
  
  tryCatch({
    dati_lista[[i]] <- f_scarica_istat(blocchi[[i]], anno = 2023)
    cat(sprintf("        ✓ Scaricati %d record\n\n", nrow(dati_lista[[i]])))
    
    # Pausa tra richieste
    Sys.sleep(5)
    
  }, error = function(e) {
    cat(sprintf("        ✗ Errore: %s\n\n", e$message))
    dati_lista[[i]] <- NULL
    
    # Pausa dopo errore
    Sys.sleep(10)
  })
}

# Rimuovi elementi NULL ----
dati_lista <- dati_lista[!sapply(dati_lista, is.null)]

# Unisci tutti i dati ----
if (length(dati_lista) > 0) {
  istat_pop_com_PR_2023 <- bind_rows(dati_lista)
  
  cat(strrep("=", 50), "\n")
  cat("✓ COMPLETATO!\n")
  cat("Record scaricati:", nrow(istat_pop_com_PR_2023), "\n")
  cat("Comuni unici:", length(unique(istat_pop_com_PR_2023$REF_AREA)), "\n\n")
  
  # Visualizza struttura ----
  glimpse(istat_pop_com_PR_2023)
  
  # Salva i dati ----
  saveRDS(istat_pop_com_PR_2023, here::here("data","data_out", "istat_pop_com_PR_2023.rds"))
  write.csv(istat_pop_com_PR_2023, here::here("data","data_out", "istat_pop_com_PR_2023.csv"), 
            row.names = FALSE) 
  
  cat("\n✓ Dati salvati!\n")
  
} else {
  cat("✗ Nessun dato scaricato. Aspetta 10 minuti e riprova.\n")
}

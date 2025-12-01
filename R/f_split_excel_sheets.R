# -------------------------------------------------------------------
# Funzione per esportare ogni foglio di un file Excel
# gestendo correttamente accenti, lettere speciali e apostrofi.
# -------------------------------------------------------------------

split_excel_sheets <- function(file_in, prefix = "", output_dir = ".") {
  library(readxl)
  library(writexl)
  library(stringi)  # per normalizzazione Unicode
  
  # Controlla il file
  if (!file.exists(file_in)) {
    stop("Il file indicato non esiste: ", file_in)
  }
  
  # Crea la cartella output se non esiste
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Elenco fogli
  sheets <- readxl::excel_sheets(file_in)
  
  for (s in sheets) {
    
    message("Esporto foglio: ", s)
    
    # ---- Pulizia del nome del file ----
    
    # Normalizza Unicode (risolve accenti composti)
    s_clean <- stringi::stri_trans_general(s, "NFD; NFC")
    
    # Sostituisci apostrofi e backtick con underscore
    s_clean <- gsub("[`']", "_", s_clean)
    
    # Elimina caratteri vietati su macOS/Windows
    s_clean <- gsub("[/:*?\"<>|]", "_", s_clean)
    
    # Sostituisci sequenze di spazi con un singolo "_"
    s_clean <- gsub("[[:space:]]+", "_", s_clean)
    
    # Rimuovi underscore multipli
    s_clean <- gsub("_+", "_", s_clean)
    
    # Rimuovi underscore iniziali/finali
    s_clean <- gsub("^_|_$", "", s_clean)
    
    # -----------------------------------
    
    # Leggi il foglio
    df <- readxl::read_excel(file_in, sheet = s)
    
    # Costruisci nome file
    out_file <- file.path(output_dir, paste0(prefix, s_clean, ".xlsx"))
    
    # Salva
    writexl::write_xlsx(df, out_file)
  }
  
  message("Operazione completata.")
}
# Esempio di utilizzo:
# source("R/f_split_excel_sheets.R")
# split_excel_sheets(
#   file_in = "data/data_in/istat_ehis_2019/3.Limitazioni.xlsx",
#   prefix = "EHIS_limit_",
#   output_dir = "data/data_in/istat_ehis_2019/partials"
# )
#' Prepara e salva shapefile per una regione (e opzionalmente una provincia)
#'
#' A partire dai layer nazionali di Comuni, Province/Città metropolitane
#' e Regioni, filtra una regione tramite \code{COD_REG} e, se richiesto,
#' una provincia tramite \code{COD_PROV}. Salva gli oggetti filtrati
#' in formato \code{.rds} nella cartella di output e restituisce gli
#' stessi oggetti in una lista con nomi coerenti ai file salvati.
#'
#' Se \code{nome_reg} o \code{nome_prov} non sono forniti, vengono
#' generati automaticamente a partire dai codici ISTAT:
#' \itemize{
#'   \item regione: \code{REGxx}, es. \code{REG08} per COD_REG = "8"
#'   \item provincia: \code{PROVxx}, es. \code{PROV34} per COD_PROV = "34"
#' }
#'
#' @param comuni_ita Oggetto sf con tutti i comuni d'Italia.
#' @param province_cm_ita Oggetto sf con tutte le province/città metropolitane.
#' @param regioni_ita Oggetto sf con tutte le regioni.
#' @param cod_reg Codice ISTAT della regione (es. "8" per Emilia-Romagna).
#' @param out_dir Cartella di output per i file .rds.
#' @param nome_reg (opzionale) Sigla regione (es. "ER", "LOMB").
#'   Se NULL, viene generata come \code{REGxx}.
#' @param cod_prov (opzionale) Codice ISTAT della provincia (es. "34" per Parma).
#' @param nome_prov (opzionale) Sigla provincia (es. "PR").
#'   Se NULL e \code{cod_prov} non è NULL, viene generata come \code{PROVxx}.
#'
#' @return Lista con oggetti sf, i cui nomi coincidono con i file salvati.
#' @export
istat_situas_sf_prep <- function(comuni_ita,
                                 province_cm_ita,
                                 regioni_ita,
                                 cod_reg,
                                 out_dir,
                                 nome_reg  = NULL,
                                 cod_prov  = NULL,
                                 nome_prov = NULL) {
  
  # Normalizza codici (sempre carattere)
  cod_reg  <- as.character(cod_reg)
  cod_prov <- if (!is.null(cod_prov)) as.character(cod_prov) else NULL
  
  # Se nome_reg non è fornito, usa prefisso basato su COD_REG
  if (is.null(nome_reg)) {
    # zero-padding a 2 cifre
    cod_reg2 <- sprintf("%02s", cod_reg)
    nome_reg <- paste0("REG", cod_reg2)  # es. "REG08"
  } else {
    nome_reg <- toupper(trimws(nome_reg))
  }
  
  # Se nome_prov non è fornito ma ho cod_prov, costruisco PROVxx
  if (!is.null(cod_prov) && is.null(nome_prov)) {
    cod_prov2 <- sprintf("%02s", cod_prov)
    nome_prov <- paste0("PROV", cod_prov2)  # es. "PROV34"
  } else if (!is.null(nome_prov)) {
    nome_prov <- toupper(trimws(nome_prov))
  }
  
  # 1) Filtra regione
  comuni_reg    <- dplyr::filter(comuni_ita,      COD_REG == cod_reg)
  provincie_reg <- dplyr::filter(province_cm_ita, COD_REG == cod_reg)
  regioni_reg   <- dplyr::filter(regioni_ita,     COD_REG == cod_reg)
  
  # 2) Filtra provincia (se richiesta)
  comuni_prov    <- NULL
  provincie_prov <- NULL
  
  if (!is.null(cod_prov)) {
    comuni_prov    <- dplyr::filter(comuni_ita,      COD_PROV == cod_prov)
    provincie_prov <- dplyr::filter(province_cm_ita, COD_PROV == cod_prov)
  }
  
  # 3) Crea cartella di output se non esiste
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  # 4) Nomi coerenti per file e lista (prefisso regione)
  nm_comuni_reg    <- paste0(nome_reg, "_comuni_sf")
  nm_provincie_reg <- paste0(nome_reg, "_provincie_sf")
  nm_regioni_reg   <- paste0(nome_reg, "_regioni_sf")
  
  # Salva RDS regione
  saveRDS(comuni_reg,
          file = file.path(out_dir, paste0(nm_comuni_reg, ".rds")))
  saveRDS(provincie_reg,
          file = file.path(out_dir, paste0(nm_provincie_reg, ".rds")))
  saveRDS(regioni_reg,
          file = file.path(out_dir, paste0(nm_regioni_reg, ".rds")))
  
  # Inizializza lista risultato
  res <- list()
  res[[nm_comuni_reg]]    <- comuni_reg
  res[[nm_provincie_reg]] <- provincie_reg
  res[[nm_regioni_reg]]   <- regioni_reg
  
  # 5) Se c'è provincia specifica, aggiunge anche quella
  if (!is.null(comuni_prov) && !is.null(nome_prov)) {
    
    nm_comuni_prov    <- paste0(nome_reg, "_", nome_prov, "_comuni_sf")
    nm_provincie_prov <- paste0(nome_reg, "_", nome_prov, "_provincie_sf")
    
    saveRDS(comuni_prov,
            file = file.path(out_dir, paste0(nm_comuni_prov, ".rds")))
    saveRDS(provincie_prov,
            file = file.path(out_dir, paste0(nm_provincie_prov, ".rds")))
    
    res[[nm_comuni_prov]]    <- comuni_prov
    res[[nm_provincie_prov]] <- provincie_prov
  }
  
  res
}

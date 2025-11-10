# FONTE -----
# ISTAT SITUAS - Servizi di pubblicazione dati territoriali
# https://situas.istat.it/web/#/territorio
# AMBITI GEOGRAFICI: Unità amministrative (Comuni)
# REPORT DISPONIBILI: Elenco codici e denominazioni UT (Report ID 61)
#                     Comuni Caratteristiche del territorio (Report ID 73)
#
#
#
# FUNZIONE 1) DATI-----
#' Scarica dati unità territoriali da ISTAT SITUAS
#'
#' @param id_report Numeric. Id del report (es: 61 per elenco comuni)
#' @param data Character o Date. Data di estrazione (formato "GG/MM/AAAA" o Date)
#' @param tipo Character. Tipo di endpoint: "dati", "conteggio", o "metadati"
#'
#' @return Un data frame (tibble) con i dati richiesti
#' @export
#'
#' @examples
#' # Scarica elenco comuni alla data odierna
#' comuni <- f_scarica_situas_dati()
#' 
#' # Scarica per una data specifica
#' comuni_2020 <- f_scarica_situas_dati(data = "01/01/2020")
f_scarica_situas_dati <- function(id_report, 
                           data = Sys.Date(), 
                           tipo = "dati") {
  
  # Converte la data nel formato richiesto (GG/MM/AAAA)
  if (inherits(data, "Date")) {
    data_formattata <- base::format(data, "%d/%m/%Y")
  } else {
    data_formattata <- data
  }
  
  # Costruisce l'URL base in base al tipo richiesto
  base_url <- "https://situas-servizi.istat.it/publish/"
  
  endpoint <- base::switch(tipo,
                           "dati" = "reportspooljson",
                           "conteggio" = "reportspooljsoncount",
                           "metadati" = "anagrafica_report_metadato_web",
                           base::stop("Tipo non valido. Usa 'dati', 'conteggio' o 'metadati'"))
  
  # Costruisce l'URL completo
  url <- base::paste0(base_url, endpoint, 
                      "?pfun=", id_report, 
                      "&pdata=", data_formattata)
  
  # Messaggio informativo
  base::message("Scarico dati da ISTAT SITUAS...")
  base::message("URL: ", url)
  
  # Effettua la richiesta HTTP
  risposta <- httr2::request(url) |>
    httr2::req_perform()
  
  # Estrae i dati JSON
  dati_json <- risposta |>
    httr2::resp_body_json(simplifyVector = TRUE)
  
  # Estrae il resultset (che contiene i dati veri)
  if ("resultset" %in% base::names(dati_json)) {
    risultato <- tibble::as_tibble(dati_json$resultset)
  } else {
    risultato <- tibble::as_tibble(dati_json)
  }
  
  base::message("Download completato: ", base::nrow(risultato), " righe")
  
  return(risultato)
}
# 
# FUNZIONE 2) METADATI-----
#' Scarica metadati delle colonne da ISTAT SITUAS
#'
#' @param id_report Numeric. Id del report (es: 61 per elenco comuni)
#' @param data Character o Date. Data di estrazione (formato "GG/MM/AAAA" o Date)
#'
#' @return Un tibble con i dettagli delle colonne
#' @export
#'
#' @examples
#' # Scarica metadati del report comuni
#' meta <- f_scarica_situas_metadati()

f_scarica_situas_metadati <- function(id_report, 
                                      data = Sys.Date()) {
  
  # Converte la data nel formato richiesto (GG/MM/AAAA)
  if (inherits(data, "Date")) {
    data_formattata <- base::format(data, "%d/%m/%Y")
  } else {
    data_formattata <- data
  }
  
  # Costruisce l'URL completo
  url <- base::paste0("https://situas-servizi.istat.it/publish/anagrafica_report_metadato_web",
                      "?pfun=", id_report, 
                      "&pdata=", data_formattata)
  
  # Messaggio informativo
  base::message("Scarico metadati da ISTAT SITUAS...")
  base::message("URL: ", url)
  
  # Effettua la richiesta HTTP
  risposta <- httr2::request(url) |>
    httr2::req_perform()
  
  # Estrae il testo JSON con encoding UTF-8 esplicito
  testo_json <- risposta |>
    httr2::resp_body_string(encoding = "UTF-8")
  
  # Forza UTF-8 come suggerito da ISTAT
  testo_json <- base::enc2utf8(testo_json)
  
  # Parse JSON con jsonlite
  dati_json <- jsonlite::fromJSON(
    testo_json, 
    simplifyVector = TRUE,
    simplifyDataFrame = TRUE
  )
  
  # Converte in tibble e spacchetta COL_DETAILS
  risultato <- tibble::as_tibble(dati_json) |>
    tidyr::unnest(COL_DETAILS)
  
  base::message("Download completato: ", base::nrow(risultato), " righe")
  
  return(risultato)
}
# 
# ESEMPIO  -----
# 
#ID_REPORT <- 76  # Elenco comuni
# UTILIZZO Funzione 1) -----
# Scarica elenco comuni alla data odierna
#dati_76 <- f_scarica_situas_dati(id_report = ID_REPORT)
#head(dati_76)
# 
# Scarica per una data specifica
# ut_com_dati_2020 <- f_scarica_situas_dati(data = "01/01/2020")
# head(cut_com_dati_2020)
# 
# 
# 
# UTILIZZO Funzione 2) -----
# Scarica metadati del report comuni
#meta_76 <- f_scarica_situas_metadati(id_report = ID_REPORT)
#head(meta_76)

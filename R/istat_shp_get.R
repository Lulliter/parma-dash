#' Legge uno shapefile ISTAT forzando l'encoding UTF-8
#'
#' Wrapper di \code{sf::st_read()} che imposta l'opzione
#' \code{ENCODING = "UTF-8"} e silenzia i messaggi.
#'
#' @param path Percorso completo al file \code{.shp}.
#'
#' @return Un oggetto \code{sf}.
#' @export
#'
#' @examples
#' \dontrun{
#' shp <- read_shp_utf8(
#'   "data/data_in/istat_shp_ITA/Com01012025/Com01012025_WGS84.shp"
#' )
#' }
read_shp_utf8 <- function(path) {
  sf::st_read(path, options = "ENCODING=UTF-8", quiet = TRUE)
}

#' Carica gli shapefile ISTAT Italia 2025 (WGS84)
#'
#' Legge i limiti amministrativi ISTAT (Comuni, Province/Città metropolitane,
#' Regioni, ripartizioni_ita geografiche) alla data 01/01/2025 a partire da
#' una cartella base.
#'
#' @param istat_sh_path Percorso base alla cartella che contiene le
#'   sottocartelle ISTAT, ad esempio:
#'   \code{here::here("data", "data_in", "istat_shp_ITA")}.
#'
#' @return Una lista con quattro elementi:
#'   \itemize{
#'     \item \code{comuni_ita}: oggetto \code{sf} dei Comuni
#'     \item \code{province_cm_ita}: oggetto \code{sf} delle Province/Città metropolitane
#'     \item \code{regioni_ita}: oggetto \code{sf} delle Regioni
#'     \item \code{ripartizioni_ita}: oggetto \code{sf} delle ripartizioni_ita geografiche
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' base_path <- here::here("data", "data_in", "istat_shp_ITA")
#' shp_2025  <- istat_shp_get(base_path)
#'
#' names(shp_2025)
#' # "comuni_ita" "province_cm_ita" "regioni_ita" "ripartizioni_ita"
#' }
istat_shp_get <- function(istat_sh_path) {
  
  comuni_path <- file.path(istat_sh_path, "Com01012025",    "Com01012025_WGS84.shp")
  prov_path   <- file.path(istat_sh_path, "ProvCM01012025", "ProvCM01012025_WGS84.shp")
  reg_path    <- file.path(istat_sh_path, "Reg01012025",    "Reg01012025_WGS84.shp")
  rip_path    <- file.path(istat_sh_path, "RipGeo01012025", "RipGeo01012025_WGS84.shp")
  
  comuni_ita      <- read_shp_utf8(comuni_path)
  province_cm_ita <- read_shp_utf8(prov_path)
  regioni_ita     <- read_shp_utf8(reg_path)
  ripartizioni_ita    <- read_shp_utf8(rip_path)
  
  list(
    comuni_ita      = comuni_ita,
    province_cm_ita = province_cm_ita,
    regioni_ita     = regioni_ita,
    ripartizioni_ita    = ripartizioni_ita
  )
}
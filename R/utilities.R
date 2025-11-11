#' Salva tabella di descrizione variabili comuni ISTAT-SITUAS
#'
#' @param df Tibble da salvare
#' @param out_path Percorso completo dell'RDS di output
#' @return Lo stesso tibble (invisibile)
#' @export
salva_vardesc_rds <- function(df, out_path) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(df, out_path)
  invisible(df)
}

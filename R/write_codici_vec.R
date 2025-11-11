#' Write Unique Codes to R Script as Character Vector
#' 
#' @param x A data frame or tibble containing the data.
#' @param col The column name (as a string) from which to extract unique codes
#' '            (e.g., "COD_COMUNE", "COD_PROV", "COD_REG").
#' @param vec_name The name of the character vector to be created in the output
#' '                 R script (e.g., "codici_comuni", "codici_province").
#' @param out_path The file path where the R script will be saved.
#' @return Invisibly returns the vector of unique codes.
#' @examples
#' \dontrun{
#' # Example data frame
#' df <- data.frame(COD_COMUNE = c("001", "002", "003", "001"))
#' # Write unique codes to R script
#' write_codici_vec(df, "COD_COMUNE", "codici_comuni", "data/codici_comuni.R")
#' }
write_codici_vec <- function(x, col, vec_name, out_path) {
  codici <- sort(unique(as.character(x[[col]])))
  
  txt <- paste0(
    vec_name, " <- c(",
    paste0('"', codici, '"', collapse = ", "),
    ")\n"
  )
  
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(txt, con = out_path)
  cat(txt)
  
  invisible(codici)
}

# SCARICO E PREPARO I DATI SUi TREND DEMOGRAFICI
# DATI DEMO ISTAT TREND 2002-2024 (`data/data_in/indic_demogr_prov_trend.xlsx`)
# e salvo in `data/data_out/indic_demogr_prov_trend/*`

# Load pckgs /  functions ----
library(readxl)
library(tidyverse)
library(here)
library(purrr)
source(here::here("R", "istat_xlsx_import.R"))

# 🟩 Fogli con header semplice (2 riga) ----

# ___ quoziente_di_natalità ----
quoziente_di_natalità <- import_istat_simple_sheet(
  file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx"),
  sheet_name = "quoziente_di_natalità"
)

# ___ quoziente_di_mortalità ----
quoziente_di_mortalità <- import_istat_simple_sheet(
  file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx"),
  sheet_name = "quoziente_di_mortalità"
)


# ___ quoziente_di_nuzialità ----
quoziente_di_nuzialità <- import_istat_simple_sheet(
  file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx"),
  sheet_name = "quoziente_di_nuzialità"
)

# ___ saldo_migratorio_interno ----
saldo_migratorio_interno <- import_istat_simple_sheet(
  file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx"),
  sheet_name = "saldo_migratorio_interno"
)

# ___ saldo_migratorio_con_l_estero ----
saldo_migratorio_con_l_estero <- import_istat_simple_sheet(
  file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx"),
  sheet_name = "saldo_migratorio_con_l_estero"
)

# ___ saldo_migratorio_altro_motivo ----
saldo_migratorio_altro_motivo <- import_istat_simple_sheet(
  file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx"),
  sheet_name = "saldo_migratorio_altro_motivo"
)

# ___ saldo_migratorio_totale ----
saldo_migratorio_totale <- import_istat_simple_sheet(
  file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx"),
  sheet_name = "saldo_migratorio_totale"
)

# ___ crescita_naturale ----
crescita_naturale <- import_istat_simple_sheet(
  file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx"),
  sheet_name = "crescita_naturale"
)

# ___ tasso_di_crescita_totale ----
tasso_di_crescita_totale <- import_istat_simple_sheet(
  file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx"),
  sheet_name = "tasso_di_crescita_totale"
)

# ___ tasso_di_fecondità_totale ----
tasso_di_fecondità_totale <- import_istat_simple_sheet(
  file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx"),
  sheet_name = "tasso_di_fecondità_totale"
)

# ___ età_media_al_parto ----
età_media_al_parto <- import_istat_simple_sheet(
  file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx"),
  sheet_name = "età_media_al_parto"
)


# 🟩 Fogli (strani) speranza di vita -----
## ___ speranza_di_vita_0 ----
speranza_di_vita_0 <- import_istat_speranza(
  file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx"),
  sheet_name = "speranza_di_vita_0"
)
# Verifica
#dplyr::sample_n(speranza_di_vita_0, 5)

## ___ speranza_di_vita_65 ----
speranza_di_vita_65 <- import_istat_speranza(
  file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx"),
  sheet_name = "speranza_di_vita_65"
)
# Verifica
#dplyr::sample_n(speranza_di_vita_65, 5)

# 🟩  Fogli con header Complesso (3 righe) ----
# ___ struttura_popolazione ----
indicatori_struttura_popolazione <- import_istat_complex_sheet(
  file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx"),
  sheet_name = "struttura_popolazione"
)

# ___ indicatori_di_struttura ----
indicatori_di_struttura <- import_istat_complex_sheet(
  file_path = here::here("data", "data_in", "indic_demogr_prov_trend.xlsx"),
  sheet_name = "indicatori_di_struttura"
)
str(indicatori_di_struttura)

# 🟦 SALVO -------
# Use purrr to save all objects in the Glob Env as df to RDS files in data_out/istat_demo_2002_2024/*.rds

# Ottengo tutti gli oggetti nell'environment globale
all_objects <- ls(envir = .GlobalEnv)

# Filtro solo i dataframe
df_names <- all_objects[purrr::map_lgl(
  all_objects,
  ~ is.data.frame(get(.x, envir = .GlobalEnv))
)]

# Creo la directory output se non esiste
output_dir <- here::here("data", "data_out", "istat_demo_2002_2024")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Salvo ogni dataframe come RDS
purrr::walk(df_names, function(df_name) {
  # Ottengo il dataframe dall'environment globale
  df <- get(df_name, envir = .GlobalEnv)
  # Definisco il percorso del file
  file_path <- file.path(output_dir, paste0(df_name, ".rds"))
  saveRDS(df, file_path)
  message("Salvato: ", df_name, ".rds")
})

message("\nTotale dataframe salvati: ", length(df_names))

# 🟦🌫️ Oppure con ----
# source(here::here("R", "utilities.R"))
# save_all_dataframes(here::here("data", "data_out", "istat_demo_2002_2024"))

# Caricamento dati SERVIZI SOCIALI per TUTTE LE PROVINCIE dell'Emilia-Romagna ----
# Fonte: IstatData | Servizi sociali e socio-educativi | Interventi e servizi sociali dei comuni
# Output: un file long con tutte le provincie dell'ER (2011-2022)

library(here)
library(fs)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# 1. LOAD ALL PROVINCE FILES ----

# Directory containing the CSV files
data_dir <- here("data/data_in/ISTAT_SERVSOC/serv_soc_ER_x_prov_serie")

# List all CSV files
csv_files <- dir_ls(data_dir, glob = "*.csv")

# Read and combine all files
servsoc_ER_all <- csv_files |>
  map_df(~ {
    # Extract province name from filename
    provincia <- .x |>
      path_file() |>
      path_ext_remove() |>
      str_remove("servsoc_")

    # Read file
    read_delim(.x, delim = ";", show_col_types = FALSE) |>
      # Add provincia column
      mutate(PROVINCIA = provincia, .before = 1)
  }) |>
  # Drop completely empty columns
  select(where(~ !all(is.na(.)))) |>
  # Convert TIME_PERIOD to integer
  mutate(TIME_PERIOD = as.integer(TIME_PERIOD))

# Check structure
glimpse(servsoc_ER_all)
dim(servsoc_ER_all)

# Check provinces loaded
servsoc_ER_all |> count(PROVINCIA)


# 2. ADD LOOKUP TABLES (Italian labels) ----

# DATA_TYPE lookup
DATA_TYPE_lookup <- tribble(
  ~DATA_TYPE, ~data_type_it,
  "USSSPM", "utenti",
  "USSSP_RP", "utenti richiedenti prestazioni",
  "PERUSTPOP", "utenti sulla popolazione di riferimento (%)",
  "PERMOS", "comuni che offrono il servizio (%)",
  "TOTEXP", "totale spesa (euro)",
  "EXPMUN", "spesa dei comuni (euro)",
  "EXPUSERS", "compartecipazione utenti alla spesa (euro)",
  "EXPSSN", "compartecipazione SSN alla spesa (euro)",
  "EXPTPOP", "spesa dei comuni sulla popolazione (euro)"
)

# CATEG_OF_BENEFICIARIES lookup
CATEG_BENEF_lookup <- tribble(
  ~CATEG_OF_BENEFICIARIES, ~beneficiario,
  "POV", "povertà, disagio adulti e senza dimora",
  "FAM", "famiglia e minori",
  "DIS", "disabili",
  "ELD", "anziani",
  "ADD", "dipendenze",
  "IMMNOM", "immigrati, Rom, Sinti e Caminanti",
  "MULBEN", "multiutenza",
  "TOT", "totale"
)

# CATEGORY_OF_SERVICE lookup (subset - add more as needed)
SERVICE_lookup <- tribble(
  ~CATEGORY_OF_SERVICE, ~servizio,
  "PSW", "servizio sociale professionale",
  "HOMECARE", "assistenza domiciliare socio-assistenziale",
  "HEALTHV", "voucher, assegno di cura, buono socio-sanitario",
  "OTHERHOME", "altre attività di assistenza domiciliare",
  "HOMEHEALTH", "assistenza domiciliare integrata con servizi sanitari",
  "REMASS", "telesoccorso e teleassistenza",
  "PROXIM", "servizi di prossimità (buonvicinato)",
  "DAYC", "centro diurno",
  "ALL", "servizi per tutti"
)

# Add labels to main dataset
servsoc_ER_labeled <- servsoc_ER_all |>
  left_join(DATA_TYPE_lookup, by = "DATA_TYPE") |>
  left_join(CATEG_BENEF_lookup, by = "CATEG_OF_BENEFICIARIES") |>
  left_join(SERVICE_lookup, by = "CATEGORY_OF_SERVICE")


# 3. SAVE PROCESSED DATA ----

output_dir <- here("data/data_out/istat_SERVSOC_2011_2022")
dir_create(output_dir)

# Save full dataset (all services, all data types)
saveRDS(
  servsoc_ER_labeled,
  file.path(output_dir, "servsoc_ER_all_2011_2022.rds")
)

# Save subset focused on home care services for elderly/disabled
servsoc_ER_homecare <- servsoc_ER_labeled |>
  filter(CATEG_OF_BENEFICIARIES %in% c("DIS", "ELD")) |>
  filter(CATEGORY_OF_SERVICE %in% c("HOMECARE", "HEALTHV", "OTHERHOME", "HOMEHEALTH")) |>
  filter(DATA_TYPE %in% c("USSSPM", "USSSP_RP", "PERUSTPOP", "TOTEXP"))

saveRDS(
  servsoc_ER_homecare,
  file.path(output_dir, "servsoc_ER_homecare_2011_2022.rds")
)

# Summary
dim(servsoc_ER_labeled)
dim(servsoc_ER_homecare)

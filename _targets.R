# _targets.R ----

library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("sf", "dplyr", "stringr", "tidyr", "janitor", "here")
)

# --- Sorgenti funzioni (1 file = 1 funzione) ----

# __ 1) map/00_carica_shp_situas.qmd ----
source("R/istat_shp_get.R")           # legge shapefile ISTAT
source("R/istat_situas_get.R")        # costruisce URL SITUAS + API GET
source("R/istat_situas_sf_prep.R")    # filtra/salva sf per regione/provincia
source("R/istat_situas_join_comuni_sf.R")  # join SITUAS + shapefile comuni
source("R/write_codici_vec.R")        # scrive vettori di codici su file .R
source("R/utilities.R")  # funzioni di utilità varie

# --- Definizione pipeline targets ----
list(
  # __ 1) map/00_carica_shp_situas.qmd ----

  # 1.1) Carica dati (shapefile ISTAT + SITUAS) ----
  tar_target(istat_shp_path, here::here("data", "data_in", "istat_shp_ITA")),
  
  tar_target(istat_shp_2025, istat_shp_get(istat_shp_path)),
  
  tar_target(comuni_ita,       istat_shp_2025$comuni_ita),
  tar_target(province_cm_ita,  istat_shp_2025$province_cm_ita),
  tar_target(regioni_ita,      istat_shp_2025$regioni_ita),
  tar_target(ripartizioni_ita, istat_shp_2025$ripartizioni_ita),
  
  tar_target(com_caratt, f_scarica_situas_dati(id_report = 73)),
  
  # 1.2) Prepara sf per regioni/province (caso ER = progetto base) ----
  tar_target(er_out_dir, here::here("data", "data_out", "ER_shp")),
  tar_target(
    er_res,
    istat_situas_sf_prep(
      comuni_ita, province_cm_ita, regioni_ita,
      cod_reg  = "8",   # Emilia-Romagna (ISTAT)
      out_dir  = er_out_dir,
      cod_prov = "34",  # Parma (ISTAT)
      nome_reg = "ER",  # tua sigla
      nome_prov = "PR"  # tua sigla
    )
  ),
  tar_target(ER_comuni_sf,       er_res$ER_comuni_sf),
  tar_target(ER_provincie_sf,    er_res$ER_provincie_sf),
  tar_target(ER_regioni_sf,      er_res$ER_regioni_sf),
  tar_target(ER_PR_comuni_sf,    er_res$ER_PR_comuni_sf),
  tar_target(ER_PR_provincie_sf, er_res$ER_PR_provincie_sf),
  
  tar_target(
    er_codici,
    {
      list(
        prov = write_codici_vec(
          x        = ER_provincie_sf,
          col      = "COD_PROV",
          vec_name = "CODICI_PROV_ER",
          out_path = file.path(er_out_dir, "lista_COD_PROV_er_vec.R")
        ),
        comuni = write_codici_vec(
          x        = ER_comuni_sf,
          col      = "PRO_COM_T",
          vec_name = "CODICI_COMUNI_ER",
          out_path = file.path(er_out_dir, "lista_PRO_COM_T_er_vec.R")
        ),
        comuni_pr = write_codici_vec(
          x        = ER_PR_comuni_sf,
          col      = "PRO_COM_T",
          vec_name = "CODICI_COMUNI_ER_PR",
          out_path = file.path(er_out_dir, "lista_PRO_COM_T_er_pr_vec.R")
        )
      )
    }
  ),
  
  # 1.3) Join SITUAS + shapefile comuni (Italia intera) ----
  tar_target(
    comuni_info,
    istat_situas_join_comuni_sf(
      comuni_sf    = comuni_ita,
      situas_df    = com_caratt,
      out_dir      = here::here("data", "data_out"),
      out_basename = "comuni_ita_info_redux_sf"
    )
  ),
  tar_target(comuni_ita_info_sf,       comuni_info$comuni_info_sf),
  tar_target(comuni_ita_info_redux_sf, comuni_info$comuni_info_redux_sf),
  
  # 1.4) Tbl VARDESC per report in Root (maps.qmd) ----
  tar_target(
    comuni_ita_info_sf_VARDESC,
    salva_vardesc_rds(
      df = comuni_ita_info_sf,
      out_path = here::here("data", "data_out", "comuni_ita_info_sf_VARDESC.rds")
    )
  ),
  
  # 1.5 Render Quarto ----
  tar_quarto(
    report_istat_get_data,
    path = "map/00_carica_shp_situas.qmd",
    # necessario x la bib
    working_directory = here::here()
    
)
)


# ORA PUOI ----

# ESEGUIRE IL PIPELINE DALLA CONSOLE R con:
# targets::tar_make()

# E VISUALIZZARE LO STATO DEL PIPELINE CON:
# targets::tar_visnetwork()

# ESEGUIRE SOLO UN TARGET SPECIFICO (ricostruisce eventuali dipendenze non aggiornate)
# targets::tar_make(report_istat_get_data)
# targets::tar_make(comuni_info)

# ESEGUIRE UN TARGET SPECIFICO nella sessione corrente (utile per il debug):
# targets::tar_make(names = "comuni_ita_info_redux_sf", callr_function = NULL)

# LEGGERE UN TARGET SPECIFICO CON:
# comuni_ita_info_redux_sf <- targets::tar_read(comuni_ita_info_redux_sf)
# targets::tar_load(comuni_ita_info_redux_sf)



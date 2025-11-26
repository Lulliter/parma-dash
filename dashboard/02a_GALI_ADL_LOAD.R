# Caricamento e pulizia dei dati GALI e ADL ----
library(readr)
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(sf)

# Caricamento dei dati GALI da data/data_in/ISTAT_DISAB_CIFRE/GALI_2023_eta_regione.xls ----
# Nota: specifichiamo il foglio "2023_eta_regione" che contiene i dati per età e regione
gali_data <- read_excel(
  here::here("data/data_in/ISTAT_DISAB_CIFRE/GALI_2023_eta_regione.xls"),
  sheet = "2023_eta_regione"
)

# 1. PREPARAZIONE DATI ----

# Pulizia nomi colonne
gali_clean <- gali_data %>%
  rename(
    territorio = Territorio,
    gravita = livello_gravità_limitazioni,
    eta_0_44 = `0-44 anni`,
    eta_45_64 = `45-64 anni`,
    eta_65_74 = `65-74 anni`,
    eta_75_plus = `75 anni e più`,
    media = `media generale`
  )

# Dataset con TUTTI i livelli di gravità (per confronti)
gali_tutti_livelli <- gali_clean %>%
  select(
    territorio,
    gravita,
    eta_0_44,
    eta_45_64,
    eta_65_74,
    eta_75_plus,
    media
  )

# Filtra solo limitazioni gravi (disabilità) - per focus specifico
gali_disabilita <- gali_clean %>%
  filter(gravita == "Limitazioni gravi")

# Dataset completo con TUTTE le classi di età (solo limitazioni gravi)
gali_completo <- gali_disabilita %>%
  select(
    territorio,
    gravita,
    eta_0_44,
    eta_45_64,
    eta_65_74,
    eta_75_plus,
    media
  )

# Dataset focalizzato su classi anziane (solo limitazioni gravi)
gali_anziani <- gali_disabilita %>%
  select(territorio, gravita, eta_65_74, eta_75_plus)

# Aggiungiamo ripartizioni geografiche
ripartizioni_map <- tribble(
  ~territorio             , ~ripartizione ,
  "Piemonte"              , "Nord-ovest"  ,
  "Valle d'Aosta"         , "Nord-ovest"  ,
  "Lombardia"             , "Nord-ovest"  ,
  "Liguria"               , "Nord-ovest"  ,
  "Trentino-Alto Adige"   , "Nord-est"    ,
  "Veneto"                , "Nord-est"    ,
  "Friuli-Venezia Giulia" , "Nord-est"    ,
  "Emilia-Romagna"        , "Nord-est"    ,
  "Toscana"               , "Centro"      ,
  "Umbria"                , "Centro"      ,
  "Marche"                , "Centro"      ,
  "Lazio"                 , "Centro"      ,
  "Abruzzo"               , "Sud"         ,
  "Molise"                , "Sud"         ,
  "Campania"              , "Sud"         ,
  "Puglia"                , "Sud"         ,
  "Basilicata"            , "Sud"         ,
  "Calabria"              , "Sud"         ,
  "Sicilia"               , "Isole"       ,
  "Sardegna"              , "Isole"
)

# Uniamo con ripartizioni - TUTTI I LIVELLI DI GRAVITÀ
gali_tutti_livelli_geo <- gali_tutti_livelli %>%
  left_join(ripartizioni_map, by = "territorio") %>%
  filter(!is.na(ripartizione))

# Uniamo con ripartizioni - DATASET COMPLETO (tutte le età, solo limitazioni gravi)
gali_completo_geo <- gali_completo %>%
  left_join(ripartizioni_map, by = "territorio") %>%
  filter(!is.na(ripartizione))

# Uniamo con ripartizioni - DATASET ANZIANI (solo 65-74 e 75+, solo limitazioni gravi)
gali_anziani_geo <- gali_anziani %>%
  left_join(ripartizioni_map, by = "territorio") %>%
  filter(!is.na(ripartizione))

# 2. AGGREGAZIONE PER RIPARTIZIONI ----

# Ripartizioni - TUTTE LE ETÀ
gali_ripartizioni_completo <- gali_completo_geo %>%
  group_by(ripartizione) %>%
  summarise(
    eta_0_44_media = mean(eta_0_44, na.rm = TRUE),
    eta_45_64_media = mean(eta_45_64, na.rm = TRUE),
    eta_65_74_media = mean(eta_65_74, na.rm = TRUE),
    eta_75_plus_media = mean(eta_75_plus, na.rm = TRUE),
    media_generale = mean(media, na.rm = TRUE),
    n_regioni = n()
  ) %>%
  ungroup()

# Ripartizioni - SOLO ANZIANI (per visualizzazioni specifiche)
gali_ripartizioni <- gali_anziani_geo %>%
  group_by(ripartizione) %>%
  summarise(
    eta_65_74_media = mean(eta_65_74, na.rm = TRUE),
    eta_75_plus_media = mean(eta_75_plus, na.rm = TRUE),
    n_regioni = n()
  ) %>%
  ungroup()

# 3. FORMATO LUNGO PER VISUALIZZAZIONI ----

# Formato lungo COMPLETO - regioni con TUTTE le classi di età
gali_long_regioni_completo <- gali_completo_geo %>%
  pivot_longer(
    cols = starts_with("eta_"),
    names_to = "classe_eta",
    values_to = "percentuale_disabilita",
    names_prefix = "eta_"
  ) %>%
  mutate(
    classe_eta = recode(
      classe_eta,
      "0_44" = "0-44 anni",
      "45_64" = "45-64 anni",
      "65_74" = "65-74 anni",
      "75_plus" = "75+ anni"
    ),
    # Ordine fattoriale per visualizzazioni
    classe_eta = factor(
      classe_eta,
      levels = c("0-44 anni", "45-64 anni", "65-74 anni", "75+ anni")
    )
  )

# Formato lungo ANZIANI - regioni con solo classi 65-74 e 75+
gali_long_regioni <- gali_anziani_geo %>%
  pivot_longer(
    cols = starts_with("eta_"),
    names_to = "classe_eta",
    values_to = "percentuale_disabilita",
    names_prefix = "eta_"
  ) %>%
  mutate(
    classe_eta = recode(
      classe_eta,
      "65_74" = "65-74 anni",
      "75_plus" = "75+ anni"
    )
  )

# Formato lungo COMPLETO - ripartizioni con TUTTE le classi di età
gali_long_ripartizioni_completo <- gali_ripartizioni_completo %>%
  pivot_longer(
    cols = ends_with("_media"),
    names_to = "classe_eta",
    values_to = "percentuale_disabilita"
  ) %>%
  filter(classe_eta != "media_generale") %>% # Escludi media generale
  mutate(
    classe_eta = recode(
      classe_eta,
      "eta_0_44_media" = "0-44 anni",
      "eta_45_64_media" = "45-64 anni",
      "eta_65_74_media" = "65-74 anni",
      "eta_75_plus_media" = "75+ anni"
    ),
    # Ordine fattoriale
    classe_eta = factor(
      classe_eta,
      levels = c("0-44 anni", "45-64 anni", "65-74 anni", "75+ anni")
    )
  )

# Formato lungo ANZIANI - ripartizioni con solo classi 65-74 e 75+
gali_long_ripartizioni <- gali_ripartizioni %>%
  pivot_longer(
    cols = ends_with("_media"),
    names_to = "classe_eta",
    values_to = "percentuale_disabilita"
  ) %>%
  mutate(
    classe_eta = recode(
      classe_eta,
      "eta_65_74_media" = "65-74 anni",
      "eta_75_plus_media" = "75+ anni"
    )
  )

# 3b. SALVA DATASET PROCESSATI ----

output_dir_gali <- here::here("data/data_out/istat_GALI_2023")
if (!dir.exists(output_dir_gali)) {
  dir.create(output_dir_gali, recursive = TRUE)
}

# Salva dataset principali
saveRDS(
  gali_tutti_livelli_geo,
  file.path(output_dir_gali, "gali_tutti_livelli_geo.rds")
)
saveRDS(gali_completo_geo, file.path(output_dir_gali, "gali_completo_geo.rds"))
saveRDS(gali_anziani_geo, file.path(output_dir_gali, "gali_anziani_geo.rds"))
saveRDS(
  gali_ripartizioni_completo,
  file.path(output_dir_gali, "gali_ripartizioni_completo.rds")
)
saveRDS(gali_ripartizioni, file.path(output_dir_gali, "gali_ripartizioni.rds"))
saveRDS(
  gali_long_regioni_completo,
  file.path(output_dir_gali, "gali_long_regioni_completo.rds")
)
saveRDS(gali_long_regioni, file.path(output_dir_gali, "gali_long_regioni.rds"))
saveRDS(
  gali_long_ripartizioni_completo,
  file.path(output_dir_gali, "gali_long_ripartizioni_completo.rds")
)
saveRDS(
  gali_long_ripartizioni,
  file.path(output_dir_gali, "gali_long_ripartizioni.rds")
)

message("✓ Dataset GALI salvati in: ", output_dir_gali)

# 4. CARICA E PREPARA SHAPEFILE REGIONI ITALIANE ----

# Controlla se shapefile già preparato esiste
shp_rds_path <- here::here("data/data_out/ITA_shp/regioni_ita_full.rds")

if (file.exists(shp_rds_path)) {
  # Carica shapefile già preparato
  regioni_ita <- readRDS(shp_rds_path)
} else {
  # Prepara shapefile da zero
  message("Preparazione shapefile regioni italiane...")

  # Carica shapefile
  regioni_ita_raw <- st_read(
    here::here("data/data_in/istat_shp_ITA/Reg01012025/Reg01012025_WGS84.shp"),
    quiet = TRUE
  )

  # Pulizia nomi regioni per match con dati GALI
  regioni_ita_clean <- regioni_ita_raw %>%
    mutate(
      territorio = DEN_REG,
      # Gestione casi speciali
      territorio = case_when(
        territorio == "Valle d'Aosta/Vallée d'Aoste" ~ "Valle d'Aosta",
        territorio == "Trentino-Alto Adige/Südtirol" ~ "Trentino-Alto Adige",
        territorio == "Friuli-Venezia Giulia" ~ "Friuli-Venezia Giulia",
        TRUE ~ territorio
      )
    )

  # Mappa COD_RIP -> nome ripartizione
  ripartizioni_nomi <- tribble(
    ~COD_RIP , ~ripartizione ,
           1 , "Nord-ovest"  ,
           2 , "Nord-est"    ,
           3 , "Centro"      ,
           4 , "Sud"         ,
           5 , "Isole"
  )

  # Aggiungi ripartizioni
  regioni_ita <- regioni_ita_clean %>%
    left_join(ripartizioni_nomi, by = "COD_RIP")

  # Salva per usi futuri
  output_dir <- here::here("data/data_out/ITA_shp")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  saveRDS(regioni_ita, file = shp_rds_path)

  message("✓ Shapefile regioni salvato in: ", output_dir)
}

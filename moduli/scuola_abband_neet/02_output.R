# ------------------------------------------------------------------------
# Modulo: scuola_abband_neet
# Scopo:  giovani a rischio dispersione / NEET nella provincia di Parma:
#         NEET per provincia ER (trend BES); competenze non adeguate in III media
#         per provincia (barre, ultimo anno); ritardo scolastico per anno di corso
#         PR vs ER (l'accumulo lungo il percorso); ritardo per ordine nel tempo
#         PR vs ER; mappa comunale PR del ritardo alle medie
# Input:  output/*.rds (da 01_dati.R); dati/puliti/istat_shp/ (geometrie)
# Output: output/plot_*.rds, mappa_*.rds (ggplot; girafe() nella pagina) + .png
# ------------------------------------------------------------------------

library(here)
library(dplyr)
library(stringr)
library(janitor)
library(purrr)
library(glue)
library(ggplot2)
library(ggiraph)
library(scales)
library(ggtext)
library(sf)

source(here("R", "_parma_colors.R"))
source(here("R", "f_caption_fonte.R"))
source(here("R", "f_theme_scuola.R"))
source(here("R", "f_lab_as.R"))
source(here("R", "f_aggiungi_classe.R"))
source(here("R", "f_disegna_mappa.R"))
source(here("R", "f_salva_mappa.R"))
source(here("R", "f_pal5.R"))

# Parametri ---------------------------------------------------------------
dir_mod <- here("moduli", "scuola_abband_neet", "output")

CAP_BES <- f_caption_fonte("ISTAT, Bes dei territori (ed. 2025)")
CAP_NEET <- f_caption_fonte("ISTAT, Bes dei territori (ed. 2025); NEET = stima campionaria")
# Stime da indagine campionarie RCFL (Rilevazione sulle forze di lavoro)".
CAP_MIM <- f_caption_fonte("MIM, Portale unico dei dati della scuola (statali + paritarie, no infanzia, esclusi serali/CPIA)")

ANNO_PRIMO <- 2015   # primo a.s. serie MIM (2015/16)
ANNO_ULTIMO <- 2024  # a.s. 2024/25 (MIM) e anno 2024 (BES)
ANNO_BES_PRIMO <- 2018

# etichette degli ordini di scuola (nomi MIM → brevi) e dei territori
ORDINI_LBL <- c("SCUOLA PRIMARIA" = "Primaria",
                "SCUOLA SECONDARIA I GRADO" = "Secondaria I grado",
                "SCUOLA SECONDARIA II GRADO" = "Secondaria II grado")
COL_TERRITORI <- c("Parma" = ylw_lg, "Emilia-Romagna" = grn_md, "Italia" = blu_md,
                   "Altre province ER" = grey_sc)

# 1. Carica dati pronti ----------------------------------------------------
ritardo_trend_prov_er <- readRDS(file.path(dir_mod, "ritardo_trend_prov_er.rds"))
ritardo_corso_prov_er <- readRDS(file.path(dir_mod, "ritardo_corso_prov_er.rds"))
ritardo_comuni_pr <- readRDS(file.path(dir_mod, "ritardo_comuni_pr.rds"))
bes_istruzione_prov_er <- readRDS(file.path(dir_mod, "bes_istruzione_prov_er.rds"))

# territorio "display" per colori e legenda (BES: nomi già in forma leggibile)
f_territorio_display <- function(territorio) {
  factor(case_when(
    territorio %in% c("Parma", "PARMA") ~ "Parma",
    territorio %in% c("Emilia-Romagna", "EMILIA-ROMAGNA") ~ "Emilia-Romagna",
    territorio == "Italia" ~ "Italia",
    .default = "Altre province ER"
  ), levels = names(COL_TERRITORI))
}

# 2. Grafici BES -----------------------------------------------------------

# Plot: NEET 15-29 per provincia ER, Parma evidenziata (+ ER e Italia) ----
neet_prov_prep <- bes_istruzione_prov_er |>
  filter(cod_indicatore == "02IST006-N22", sesso == "Totale", territorio != "Nord-est") |>
  mutate(quota = valore / 100,
         territorio_display = f_territorio_display(territorio),
         highlight = territorio_display != "Altre province ER")

neet_prov_prep

plot_neet_prov_er <- neet_prov_prep |>
  ggplot(aes(x = anno, y = quota, color = territorio_display, alpha = highlight, group = territorio)) +
  geom_line_interactive(aes(tooltip = territorio, data_id = territorio), linewidth = rel(0.8)) +
  geom_line_interactive(data = function(df) df |> filter(highlight),
                        aes(tooltip = territorio, data_id = territorio), linewidth = rel(1.5)) +
  geom_point_interactive(data = function(df) df |> filter(highlight),
                         aes(tooltip = glue("{territorio} {anno}: {scales::percent(quota, accuracy = 0.1)}")), size = 1.8) +
  scale_x_continuous(breaks = ANNO_BES_PRIMO:ANNO_ULTIMO) +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1), limits = c(0, NA)) +
  scale_alpha_manual(values = c(0.35, 1), guide = "none") +
  scale_color_manual(values = COL_TERRITORI) +
  f_theme_scuola() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(
    title = str_wrap(glue("Giovani che non studiano e non lavorano (NEET), {ANNO_BES_PRIMO}-{ANNO_ULTIMO}"), 55),
    subtitle = "15-29 anni, in % della popolazione della stessa età; province ER, regione e Italia. Stime campionarie: le differenze tra province vanno lette con cautela",
    caption = CAP_NEET, x = "", y = ""
  )

plot_neet_prov_er

# Plot: competenze non adeguate in III media per provincia, ultimo anno ----
competenze_prep <- bes_istruzione_prov_er |>
  filter(cod_indicatore %in% c("02IST011P", "02IST010P"), sesso == "Totale",
         anno == ANNO_ULTIMO, territorio != "Nord-est") |>
  mutate(materia = if_else(cod_indicatore == "02IST011P", "Italiano (alfabetica)", "Matematica (numerica)"),
         quota = valore / 100,
         territorio_display = f_territorio_display(territorio)) |>
  # ordine delle barre: media delle due materie
  mutate(ordine = mean(quota), .by = territorio) |>
  mutate(territorio = reorder(territorio, ordine))

competenze_prep

plot_competenze_prov_er <- competenze_prep |>
  ggplot(aes(x = quota, y = territorio, fill = territorio_display)) +
  geom_col_interactive(aes(tooltip = glue("{territorio}, {materia}: {scales::percent(quota, accuracy = 0.1)}"),
                           data_id = paste(territorio, materia)), width = 0.75) +
  geom_text(aes(label = scales::percent(quota, accuracy = 1)), hjust = -0.15, size = 3.5) +
  facet_wrap(~ materia) +
  scale_x_continuous(labels = function(x) scales::percent(x, accuracy = 1),
                     limits = c(0, 0.5), expand = expansion(mult = c(0, 0))) +
  scale_fill_manual(values = COL_TERRITORI) +
  f_theme_scuola() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        strip.text = element_text(size = rel(1), face = "bold"),
        panel.spacing.x = unit(2, "lines")) + # altrimenti "50%" e "0%" dei due pannelli si toccano
  labs(
    title = str_wrap(glue("Studenti di III media con competenze non adeguate ({ANNO_ULTIMO})"), 55),
    subtitle = "Prove INVALSI: quota di studenti sotto il livello adeguato, per provincia; dato censuario",
    caption = CAP_BES, x = "", y = ""
  )

plot_competenze_prov_er

# 3. Grafici ritardo scolastico (MIM) --------------------------------------

# Plot: ritardo per anno di corso, dalla 1ª primaria alla 5ª superiore, PR vs ER ----
ritardo_corso_prep <- ritardo_corso_prov_er |>
  filter(provincia %in% c("PARMA", "EMILIA-ROMAGNA"),
         anno_corso <= 5) |> # escluso il 6° anno degli istituti agrari (specializzazione
                             # enotecnico post-diploma: 282 alunni in Italia, over 18 per costruzione)
  mutate(territorio_display = f_territorio_display(provincia),
         ordine_lbl = factor(ORDINI_LBL[ordine_scuola], levels = ORDINI_LBL),
         classe = factor(anno_corso)) # pannelli per ordine, x = anno di corso

ritardo_corso_prep

plot_ritardo_corso_pr_er <- ritardo_corso_prep |>
  ggplot(aes(x = classe, y = quota_ritardo, color = territorio_display, group = territorio_display)) +
  geom_line_interactive(aes(tooltip = territorio_display, data_id = territorio_display), linewidth = rel(1.2)) +
  geom_point_interactive(aes(tooltip = glue("{territorio_display}, {ordine_lbl} {classe}ª: {scales::percent(quota_ritardo, accuracy = 0.1)} ({scales::number(alunni_ritardo, big.mark = '.')} alunni)")), size = 1.8) +
  facet_grid(~ ordine_lbl, scales = "free_x", space = "free_x",
             labeller = label_wrap_gen(14)) + # "Secondaria I grado" su 2 righe
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1), limits = c(0, NA)) +
  scale_color_manual(values = COL_TERRITORI) +
  f_theme_scuola() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        strip.text = element_text(size = rel(1), face = "bold")) +
  labs(
    title = str_wrap(glue("Alunni in ritardo scolastico per anno di corso (a.s. {f_lab_as(ANNO_ULTIMO)})"), 55),
    subtitle = "Età superiore a quella regolare per la classe (ripetenze o inserimenti in classi inferiori), in % degli iscritti della classe. Alle superiori dopo il 3° anno gli iscritti calano di un terzo (fine dell'obbligo a 16 anni, passaggi alla formazione professionale, abbandoni)",
    caption = CAP_MIM, x = "Anno di corso", y = ""
  )

plot_ritardo_corso_pr_er

# Plot: ritardo per ordine nel tempo, PR vs ER ----
ritardo_trend_prep <- ritardo_trend_prov_er |>
  filter(provincia %in% c("PARMA", "EMILIA-ROMAGNA")) |>
  mutate(territorio_display = f_territorio_display(provincia),
         ordine_lbl = factor(ORDINI_LBL[ordine_scuola], levels = ORDINI_LBL),
         etichetta_as = f_lab_as(anno_inizio))

ritardo_trend_prep

plot_ritardo_trend_pr_er <- ritardo_trend_prep |>
  ggplot(aes(x = anno_inizio, y = quota_ritardo, color = territorio_display, group = territorio_display)) +
  geom_line_interactive(aes(tooltip = territorio_display, data_id = paste(territorio_display, ordine_lbl)), linewidth = rel(1.2)) +
  geom_point_interactive(aes(tooltip = glue("{territorio_display} {etichetta_as}: {scales::percent(quota_ritardo, accuracy = 0.1)}")), size = 1.8) +
  facet_wrap(~ ordine_lbl) + # scala y comune: il confronto tra ordini è parte del messaggio
  scale_x_continuous(breaks = ANNO_PRIMO:ANNO_ULTIMO, labels = f_lab_as(ANNO_PRIMO:ANNO_ULTIMO)) +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1), limits = c(0, NA)) +
  scale_color_manual(values = COL_TERRITORI) +
  f_theme_scuola() +
  theme(strip.text = element_text(size = rel(1), face = "bold")) +
  labs(
    title = str_wrap(glue("Alunni in ritardo scolastico per ordine di scuola (trend {f_lab_as(ANNO_PRIMO)}-{f_lab_as(ANNO_ULTIMO)})"), 55),
    subtitle = "In % degli iscritti dell'ordine",
    caption = CAP_MIM, x = "", y = ""
  )

plot_ritardo_trend_pr_er

# 4. Mappa comunale PR: ritardo alle medie (ultimo a.s.) --------------------
# (le superiori stanno in 9 comuni: la mappa ha senso solo per il I grado)
file_dett <- here("dati", "puliti", "istat_shp", "PR_comuni_dettaglio_sf.rds")
if (file.exists(file_dett)) {
  pr_comuni_sf <- readRDS(file_dett) |> select(PRO_COM_T, COMUNE)
} else {
  message("Dettaglio PR non trovato (esegui ingestione/00b): uso il generalizzato")
  pr_comuni_sf <- readRDS(here("dati", "puliti", "istat_shp", "ER_comuni_sf.rds")) |>
    filter(COD_PROV %in% c("34", 34)) |> select(PRO_COM_T, COMUNE)
}
pr_bordo_sf <- pr_comuni_sf |> summarise()

lab_pct <- label_percent(accuracy = 0.1, decimal.mark = ",")

mappa_ritardo_prep <- pr_comuni_sf |>
  left_join(ritardo_comuni_pr |>
              filter(anno_inizio == ANNO_ULTIMO, ordine_scuola == "SCUOLA SECONDARIA I GRADO"),
            by = c("PRO_COM_T" = "pro_com_t")) |>
  # NA = comuni senza scuole medie (grigio in mappa)
  f_aggiungi_classe("quota_ritardo", lab_pct) |> # quintili dei comuni
  # i comuni senza medie: livello esplicito "n.d." (in legenda al posto di "NA")
  mutate(classe_quota_ritardo = forcats::fct_na_value_to_level(classe_quota_ritardo, "n.d.")) |>
  mutate(tooltip_mappa = paste0(
    str_to_title(COMUNE), ": ",
    if_else(is.na(quota_ritardo), "n.d.",
            paste0(lab_pct(quota_ritardo), " (", alunni_ritardo, " su ", alunni, ")"))
  ))

mappa_ritardo_sec1_comuni_pr <- f_disegna_mappa(
  df_comuni    = mappa_ritardo_prep,
  df_prov      = pr_bordo_sf,
  var          = "quota_ritardo",
  titolo       = str_wrap("Alunni in ritardo scolastico alle medie — provincia di Parma", 55),
  palette5     = c(f_pal5(seq_factor_red), grey_m), # 5 classi + "n.d."
  caption      = CAP_MIM,
  sottotitolo  = str_wrap(glue("A.s. {f_lab_as(ANNO_ULTIMO)}, secondaria di I grado, comune della scuola; classi = quintili; in grigio (n.d.) i comuni senza scuole medie"), 80),
  nome_legenda = "% in ritardo\nsu iscritti",
  col_tooltip  = "tooltip_mappa",
  df_evidenzia = NULL
)

mappa_ritardo_sec1_comuni_pr

# 5. Salva (rds per il sito + png; nome file = oggetto) --------------------
lista_plot <- list(
  plot_neet_prov_er = plot_neet_prov_er,
  plot_competenze_prov_er = plot_competenze_prov_er,
  plot_ritardo_corso_pr_er = plot_ritardo_corso_pr_er,
  plot_ritardo_trend_pr_er = plot_ritardo_trend_pr_er
)

purrr::iwalk(lista_plot, function(p, nome) {
  saveRDS(p, file.path(dir_mod, paste0(nome, ".rds")))
  ggsave(file.path(dir_mod, paste0(nome, ".png")), p, width = 9, height = 6, dpi = 300)
  message("Salvato: ", nome, " (.rds + .png)")
})

f_salva_mappa(mappa_ritardo_sec1_comuni_pr, "mappa_ritardo_sec1_comuni_pr", dir_out = dir_mod)

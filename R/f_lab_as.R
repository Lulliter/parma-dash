# f_lab_as ----------------------------------------------------------------
# Etichetta di un anno scolastico dal suo anno di inizio: 2015 → "2015/16"
# (nata in moduli/scuola_iscritti, promossa a R/ il 2026-09-10)
f_lab_as <- function(anno) paste0(anno, "/", (anno + 1) %% 100)

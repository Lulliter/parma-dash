# f_theme_scuola ----------------------------------------------------------
# Tema comune dei grafici dei moduli scuola_* (font grandi: girafe rimpicciolisce)
# (nato in moduli/scuola_iscritti, promosso a R/ il 2026-09-10)
f_theme_scuola <- function() {
  theme_minimal(base_size = 15) + # font grandi: girafe rimpicciolisce
    theme(
      panel.grid.major = element_line(color = "grey90", linewidth = rel(0.3)),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.85)),
      # titolo = element_text NORMALE: il textbox come titolo veniva tagliato
      # in cima nel device svg di ggiraph (dsvg misura male l'altezza);
      # il textbox resta solo sul sottotitolo, che è più lungo e deve andare
      # a capo da solo
      plot.title = element_text(size = rel(1.3), face = "bold", margin = margin(b = 10)),
      plot.subtitle = ggtext::element_textbox_simple(
        size = rel(0.95), lineheight = 1.2, margin = margin(b = 10)
      ),
      legend.title = element_blank(),
      legend.position = "bottom"
    )
}

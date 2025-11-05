# [R STUDIO] Check system fonts ----
# library(systemfonts)
# library(stringr)
#
## *** OKKIO Che becca anche i fonts in utente 
# system_fonts() |> View()                                           # tutti 
# subset(system_fonts(), grepl("Libre", family, ignore.case = TRUE)) # solo Libre...
#
# [INTERNET] vai sul sito di Google Fonts, scarica il font Libre+Franklin 
  # Google Fonts: https://fonts.google.com/specimen/Libre+Franklin
# e fai il dpwnload `get font`
#
# [TERMINAL] Copia cartella in Library/Fonts 
# 
#   ````
#   cp ~/Downloads/Libre_Franklin/*.ttf  /Users/luisamimmi/Library/Fonts  
#   cp ~/Downloads/Roboto/*.ttf  /Users/luisamimmi/Library/Fonts
#   cp ~/Downloads/Roboto/static/*.ttf  /Users/luisamimmi/Library/Fonts
#   ls ~/Library/Fonts/
#   ```
# [R STUDIO] Riavvia RStudio e controlla con
# system_fonts() |> View()


# FONDAMENTALE per Quarto ["Libre Frankin" diventa "librefranklin"] ----
library(showtext)
# font_add("librefranklin", 
#          regular = "~/Library/Fonts/LibreFranklin-Regular.ttf",
#          bold = "~/Library/Fonts/LibreFranklin-Bold.ttf")
font_add_google("Libre Franklin", "librefranklin")
showtext_auto()

# Crea un tema personalizzato con dimensioni relative ---- 
theme_word <- function(base_size = "", base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Plot
      plot.title = element_text(
        size = rel(1.2),  # 20% più grande del base_size
        face = "bold",
        hjust = 0,
        margin = margin(b = base_size * 0.5)
      ),
      plot.subtitle = element_text(
        size = rel(0.95),  # 5% più piccolo
        hjust = 0,
        margin = margin(b = base_size * 0.5)
      ),
      plot.caption = element_text(
        size = rel(0.8),
        hjust = 1,
        margin = margin(t = base_size * 0.5)
      ),
      plot.title.position = "plot",
      plot.margin = margin(10, 90, 10, 10),
      
      # Assi
      axis.title = element_text(size = rel(0.95)),
      axis.title.x = element_text(margin = margin(t = base_size * 0.5)),
      axis.title.y = element_text(
        margin = margin(r = base_size * 0.5),
        angle = 90,      # ← AGGIUNGI QUESTO
        vjust = 0.5      # ← E QUESTO per centrare
      ),
      axis.text = element_text(size = rel(0.85)),
      axis.line = element_line(color = "grey70", linewidth = 0.3),
      
      # Legenda
      legend.position = "bottom",
      legend.title = element_text(size = rel(0.9)),
      legend.text = element_text(size = rel(0.85)),
      legend.key.size = unit(1, "lines"),
      
      # Griglia
      panel.grid = element_blank()
    )
}



# Nel grafico, usa: ----- 
# theme_word(base_size = 12, base_family = "roboto")
# theme_word(base_size = 13, base_family = "librefranklin")


# # Labeling and formatting stuff
# build_ci <- function(lower, upper) {
#   glue::glue("[{fmt_decimal(lower)}, {fmt_decimal(upper)}]")
# }
# 
# # Put these scales things in functions so that they work like regular functions,
# # otherwise {targets} complains that "... may be used in an incorrect context"
# fmt_decimal <- \(x) scales::label_number(accuracy = 0.001, style_negative = "minus")(x)
# fmt_decimal2 <- \(x) scales::label_number(accuracy = 0.01, style_negative = "minus")(x)
# 
# fmt_pct <- \(x) scales::label_percent(accuracy = 0.01, style_negative = "minus")(x)
# 
# fmt_pp_int <- \(x) scales::label_number(accuracy = 0.1, scale = 100, style_negative = "minus")(x)
# 
# fmt_pp2 <- function(x) {
#   scales::label_number(accuracy = 0.1, scale = 100, 
#     suffix = " pp.", style_negative = "minus")(x)
# }
# 
# label_pp <- function(x) {
#   scales::label_number(accuracy = 1, scale = 100, 
#     suffix = " pp.", style_negative = "minus")(x)
# }

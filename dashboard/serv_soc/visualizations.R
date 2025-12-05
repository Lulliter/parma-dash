# Visualizzazioni SERVIZI SOCIALI - Confronto tra PROVINCIE ER ----
# Comparison plots across all provinces of Emilia-Romagna
# Based on ISTAT data (2011-2022) [salvati in data_in/ISTAT_SERVSOC/*]

# IMPLEMENTATION PLAN FOR NEXT SESSION ----
# Priority order for implementing visualization functions:
#
# 1. START WITH: plot_prov_one_combo()
#    - Simplest function - one service, one data type, one beneficiary
#    - Filter data for specific combination
#    - ggplot with PROVINCIA on color aesthetic
#    - geom_line() + geom_point() over TIME_PERIOD
#    - Use colori_provincie palette
#    - Test with: HOMECARE + USSSPM + DIS
#
# 2. THEN: plot_ranking_provincie_latest()
#    - Bar chart showing provinces ranked by value in 2022
#    - Filter for specific combination + latest year
#    - geom_col() with coord_flip()
#    - Highlight Parma with different color/alpha
#    - Easy to see which provinces have higher/lower values
#
# 3. THEN: plot_provincie_facet_beneficiary()
#    - Takes plot_prov_one_combo() and adds facet_wrap(~beneficiario)
#    - Shows DIS, ELD side by side
#    - Useful to compare if patterns differ by beneficiary type
#
library(here)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(scales)
library(ggiraph)
library(ggtext)
library(patchwork)

# 1. LOAD DATASETS ----

# Load combined ER dataset (all provinces)
# servsoc_ER_all <- readRDS(
#   here("data/data_out/istat_SERVSOC_2011_2022/servsoc_ER_all_2011_2022.rds")
# )

# Load subset focused on home care services
servsoc_ER_homecare <- readRDS(
  here(
    "data/data_out/istat_SERVSOC_2011_2022/servsoc_ER_homecare_2011_2022.rds"
  )
)

# 2. THEME & COLORS ----
theme_serv_soc <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "gray30"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(size = 10, face = "bold"),
      panel.grid.minor = element_blank()
    )
}

# Color palette for provinces (9 colors for 9 provinces)
#install.packages("rcartocolor")
#https://carto.com/carto-colors/
# > rcartocolor::carto_pal(15, name = "Prism")
# [1] "#5F4690" "#1D6996" "#38A6A5" "#0F8554" "#73AF48" "#EDAD08" "#E17C05" "#CC503E" "#666666"
# > rcartocolor::carto_pal(9, name = "Safe")
# [1] "#88CCEE" "#CC6677" "#DDCC77" "#117733" "#332288" "#AA4499" "#44AA99" "#999933" "#888888"
# > rcartocolor::carto_pal(9, name = "Antique")
# [1] "#855C75" "#D9AF6B" "#AF6458" "#736F4C" "#526A83" "#625377" "#68855C" "#9C9C5E" "#7C7C7C"

colori_provincie <- c(
  "Bologna" = "#ff7f00",
  "Ferrara" = "#377eb8",
  "ForliCesena" = "#4daf4a",
  "Modena" = "#984ea3",
  "Parma" = "#873C4A",
  "Piacenza" = "#ffff33",
  "Ravenna" = "#a65628",
  "ReggioEmilia" = "#f781bf",
  "Rimini" = "#999999"
)

# Color palette for beneficiary categories
colori_beneficiari <- c(
  "disabili" = "#d75c27",
  "anziani" = "#1d91c0",
  "famiglie" = "#73AF48",
  "totale" = "#7F7F7F"
)

# Light background versions for facet panels (20% opacity)
colori_beneficiari_bg <- c(
  "disabili" = scales::alpha("#d75c27", 0.1),
  "anziani" = scales::alpha("#1d91c0", 0.1),
  "famiglie" = scales::alpha("#73AF48", 0.1),
  "totale" = scales::alpha("#7F7F7F", 0.1)
)

CAPTION <- "Fonte: ISTAT - Interventi e servizi sociali dei comuni (2011-2022)"


# 3. VISUALIZATION FUNCTIONS ----

# Function 1.a: Compare provinces - Interactive with Parma highlighted ----
plot_prov_one_combo <- function(
  data,
  service_code,
  data_type_code,
  beneficiary_code,
  return_ggplot = FALSE # Add parameter to control return type
) {
  # Filter for specific combination
  plot_data <- data |>
    filter(
      CATEGORY_OF_SERVICE == service_code,
      DATA_TYPE == data_type_code,
      CATEG_OF_BENEFICIARIES == beneficiary_code
    ) |>
    filter(!is.na(OBS_VALUE)) |>
    # Highlight Parma, others grey
    mutate(
      highlight = (PROVINCIA == "Parma"),
      provincia_display = ifelse(highlight, PROVINCIA, "Altre provincie")
    )

  # Check if data exists
  if (nrow(plot_data) == 0) {
    message("No data for this combination")
    return(NULL)
  }

  # Get labels for title
  service_name <- unique(plot_data$servizio)
  data_type_name <- unique(plot_data$data_type_it)
  beneficiary_name <- unique(plot_data$beneficiario)

  # Check if indicator is a percentage (starts with "PER")
  is_percentage <- str_starts(data_type_code, "PER")
  percent_suffix <- if (is_percentage) "%" else ""
  number_accuracy <- if (is_percentage) 0.1 else 1

  # Get background color for this beneficiary
  bg_color <- colori_beneficiari_bg[beneficiary_name]
  if (is.na(bg_color)) {
    bg_color <- scales::alpha("#FFFFFF", 0)
  }

  # Create interactive plot
  p <- ggplot(
    plot_data,
    aes(
      x = TIME_PERIOD,
      y = OBS_VALUE,
      color = provincia_display,
      alpha = highlight,
      group = PROVINCIA
    )
  ) +
    # All provinces (faded grey for others)
    geom_line_interactive(
      aes(
        tooltip = paste0(
          PROVINCIA,
          ": ",
          scales::number(
            OBS_VALUE,
            accuracy = number_accuracy,
            big.mark = ".",
            decimal.mark = ","
          ),
          percent_suffix
        ),
        data_id = PROVINCIA
      ),
      linewidth = 0.8
    ) +
    geom_point_interactive(
      aes(
        tooltip = paste0(
          PROVINCIA,
          " (",
          TIME_PERIOD,
          "): ",
          scales::number(
            OBS_VALUE,
            accuracy = number_accuracy,
            big.mark = ".",
            decimal.mark = ","
          ),
          percent_suffix
        ),
        data_id = PROVINCIA
      ),
      size = 2
    ) +
    # Thicker line for Parma
    geom_line_interactive(
      data = \(df) df |> filter(PROVINCIA == "Parma"),
      aes(
        tooltip = paste0(
          PROVINCIA,
          ": ",
          scales::number(
            OBS_VALUE,
            accuracy = number_accuracy,
            big.mark = ".",
            decimal.mark = ","
          ),
          percent_suffix
        ),
        data_id = PROVINCIA
      ),
      linewidth = 1.5
    ) +
    geom_point_interactive(
      data = \(df) df |> filter(PROVINCIA == "Parma"),
      aes(
        tooltip = paste0(
          PROVINCIA,
          " (",
          TIME_PERIOD,
          "): ",
          scales::number(
            OBS_VALUE,
            accuracy = number_accuracy,
            big.mark = ".",
            decimal.mark = ","
          ),
          percent_suffix
        ),
        data_id = PROVINCIA
      ),
      size = 3
    ) +
    scale_alpha_manual(values = c("FALSE" = 0.3, "TRUE" = 1), guide = "none") +
    scale_color_manual(
      values = c("Parma" = "#873C4A", "Altre provincie" = "#808080"),
      name = "Provincia"
    ) +
    scale_x_continuous(breaks = seq(2011, 2022, by = 1)) +
    labs(
      title = paste0("SERVIZIO: ", service_name, " - {", beneficiary_name, "}"),
      subtitle = paste0("TIPO INDICATORE: ", data_type_name),
      x = NULL,
      y = NULL,
      caption = CAPTION
    ) +
    theme_serv_soc() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.background = element_rect(fill = bg_color, color = NA)
    )

  # Return ggplot or interactive girafe based on parameter
  if (return_ggplot) {
    return(p)
  } else {
    girafe(
      ggobj = p,
      width_svg = 9,
      height_svg = 6,
      options = list(
        opts_hover(css = "stroke:black;stroke-width:2;"),
        opts_tooltip(
          css = "background-color:white;padding:5px;border-radius:3px;box-shadow:2px 2px 5px rgba(0,0,0,0.3);"
        )
      )
    )
  }
}


# __ Example 1.a: Compare provinces for home care users (disabled) ----
# ____ Plots USSSPM for 4 categories of service ----
# Keep as interactive girafe objects - arrange in Quarto with layout
p60_homecare_dis <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "HOMECARE",
  data_type_code = "USSSPM",
  beneficiary_code = "DIS"
)
p60_homecare_eld <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "HOMECARE",
  data_type_code = "USSSPM",
  beneficiary_code = "ELD"
)

p60_homehealth_dis <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "HOMEHEALTH",
  data_type_code = "USSSPM",
  beneficiary_code = "DIS"
)
p60_homehealth_eld <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "HOMEHEALTH",
  data_type_code = "USSSPM",
  beneficiary_code = "ELD"
)
# 1,2
p60_otherhome_dis <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "OTHERHOME",
  data_type_code = "USSSPM",
  beneficiary_code = "DIS"
)
p60_otherhome_eld <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "OTHERHOME",
  data_type_code = "USSSPM",
  beneficiary_code = "ELD"
)
# 1,2
p60_healthv_dis <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "HEALTHV",
  data_type_code = "USSSPM",
  beneficiary_code = "DIS"
)
p60_healthv_eld <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "HEALTHV",
  data_type_code = "USSSPM",
  beneficiary_code = "ELD"
)

# ____ Plots PERUSTPOP for 4 categories of service ----
#("HOMECARE", "HOMEHEALTH", "HEALTHV", "OTHERHOME"),
p61_homecare_per_dis <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "HOMECARE",
  data_type_code = "PERUSTPOP",
  beneficiary_code = "DIS"
)
p61_homecare_per_eld <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "HOMECARE",
  data_type_code = "PERUSTPOP",
  beneficiary_code = "ELD"
)
p61_homehealth_per_dis <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "HOMEHEALTH",
  data_type_code = "PERUSTPOP",
  beneficiary_code = "DIS"
)
p61_homehealth_per_eld <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "HOMEHEALTH",
  data_type_code = "PERUSTPOP",
  beneficiary_code = "ELD"
)
p61_otherhome_per_dis <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "OTHERHOME",
  data_type_code = "PERUSTPOP",
  beneficiary_code = "DIS"
)
p61_otherhome_per_eld <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "OTHERHOME",
  data_type_code = "PERUSTPOP",
  beneficiary_code = "ELD"
)
p61_healthv_per_dis <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "HEALTHV",
  data_type_code = "PERUSTPOP",
  beneficiary_code = "DIS"
)
p61_healthv_per_eld <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "HEALTHV",
  data_type_code = "PERUSTPOP",
  beneficiary_code = "ELD"
)
# ____ Plots PERMOS for 4 categories of service ----
p62_homecare_perCOM_dis <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "HOMECARE",
  data_type_code = "PERMOS",
  beneficiary_code = "DIS"
)
p62_homecare_perCOM_eld <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "HOMECARE",
  data_type_code = "PERMOS",
  beneficiary_code = "ELD"
)
p62_homehealth_perCOM_dis <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "HOMEHEALTH",
  data_type_code = "PERMOS",
  beneficiary_code = "DIS"
)
p62_homehealth_perCOM_eld <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "HOMEHEALTH",
  data_type_code = "PERMOS",
  beneficiary_code = "ELD"
)
p62_otherhome_perCOM_dis <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "OTHERHOME",
  data_type_code = "PERMOS",
  beneficiary_code = "DIS"
)
p62_otherhome_perCOM_eld <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "OTHERHOME",
  data_type_code = "PERMOS",
  beneficiary_code = "ELD"
)
p62_healthv_perCOM_dis <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "HEALTHV",
  data_type_code = "PERMOS",
  beneficiary_code = "DIS"
)
p62_healthv_perCOM_eld <- plot_prov_one_combo(
  servsoc_ER_homecare,
  service_code = "HEALTHV",
  data_type_code = "PERMOS",
  beneficiary_code = "ELD"
)

# Function 1.b: Compare provinces for one service/one data_type/ multiple beneficiaries combination ----
# Bar chart for latest year (2022)
# Use fixed_ncol = 3 to maintain consistent plot size regardless of number of beneficiary categories
plot_prov_one_combo_ben <- function(
  data,
  service_code,
  data_type_code,
  beneficiary_code,
  year = 2022,
  fixed_ncol = NULL
) {
  # Filter for specific combination and latest year
  plot_data <- data |>
    filter(
      CATEGORY_OF_SERVICE == service_code,
      DATA_TYPE == data_type_code,
      CATEG_OF_BENEFICIARIES %in% beneficiary_code,
      TIME_PERIOD == year
    ) |>
    # Flag missing values before replacing them
    mutate(
      is_missing = is.na(OBS_VALUE),
      OBS_VALUE = ifelse(is.na(OBS_VALUE), 0, OBS_VALUE),
      # Flag Parma for highlighting
      is_parma = (PROVINCIA == "Parma")
    )

  # Check if data exists
  if (nrow(plot_data) == 0) {
    message("No data for this combination")
    return(NULL)
  }

  # Get labels for title
  service_name <- unique(plot_data$servizio)
  data_type_name <- unique(plot_data$data_type_it)
  beneficiary_names <- paste(unique(plot_data$beneficiario), collapse = ", ")

  # Check if indicator is a percentage (starts with "PER")
  is_percentage <- str_starts(data_type_code, "PER")
  number_accuracy <- if (is_percentage) 0.1 else 1

  # Create bar chart - faceted by beneficiary
  p <- ggplot(
    plot_data,
    aes(x = PROVINCIA, y = OBS_VALUE)
  ) +
    # Add light background for each beneficiary panel
    geom_rect(
      data = plot_data |> distinct(beneficiario),
      aes(fill = beneficiario),
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      alpha = 0.1,
      inherit.aes = FALSE
    ) +
    geom_col(
      aes(
        fill = ifelse(is_missing, "(NA)", beneficiario),
        color = ifelse(is_parma, "#DDCC77", NA),
        linewidth = ifelse(is_parma, 1.5, 0)
      )
    ) +
    scale_linewidth_identity() +
    scale_fill_manual(
      values = c(colori_beneficiari, "(NA)" = "#d73027"),
      breaks = names(colori_beneficiari)
    ) +
    # Add value labels on bars
    geom_text(
      aes(
        label = ifelse(
          is_missing,
          "(NA)",
          paste0(
            scales::number(
              OBS_VALUE,
              accuracy = number_accuracy,
              big.mark = ".",
              decimal.mark = ","
            ),
            if (is_percentage) "%" else ""
          )
        ),
        color = ifelse(is_missing, "red", "black")
      ),
      hjust = -0.1,
      size = 3
    ) +
    scale_color_identity() +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.15)),
      labels = scales::label_number(big.mark = ".", decimal.mark = ",")
    ) +
    coord_flip() +
    facet_wrap(~beneficiario, scales = "free_x", ncol = fixed_ncol) +
    labs(
      title = str_wrap(paste0("SERVIZIO: ", service_name, " (", year, ")"), width = 60),
      subtitle = str_wrap(paste0("TIPO INDICATORE: ", data_type_name), width = 70),
      x = NULL,
      y = NULL,
      fill = NULL,
      caption = CAPTION
    ) +
    theme_serv_soc() +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 12),
      panel.grid = element_blank(),
      axis.line.x = element_line(color = "black", linewidth = 0.5),
      axis.text.x = element_text(face = "bold", size = 9),
      panel.spacing.x = unit(1.5, "lines") # More space between facets
    )

  # Return the plot
  p
}


# __ Example 1.b: Compare provinces for home care users (disabled , elderly) ----
# NOTE: Use fixed_ncol = 3 to keep plot size consistent when comparing 2 vs 3 beneficiary categories
# Example: fixed_ncol = 3 maintains same width whether you use c("DIS", "ELD") or c("DIS", "ELD", "FAM")

# ____ Plots USSSPM for 4 categories of service ----
# Check actual OBS_VALUE (shows if values are NA)
servsoc_ER_homecare |>
  filter(
    CATEGORY_OF_SERVICE %in%
      c("HOMECARE", "HOMEHEALTH", "HEALTHV", "OTHERHOME"),
    DATA_TYPE == "USSSPM",
    TIME_PERIOD == 2022
  ) |>
  select(PROVINCIA, CATEGORY_OF_SERVICE, CATEG_OF_BENEFICIARIES, OBS_VALUE) |>
  filter(is.na(OBS_VALUE))

# Plots USSSPM for 4 categories of service
p63_homecare_bar22_usr <- plot_prov_one_combo_ben(
  servsoc_ER_homecare,
  service_code = "HOMECARE",
  data_type_code = "USSSPM",
  beneficiary_code = c("DIS", "ELD", "FAM"),
  year = 2022
)

p63_homehealth_bar22_usr <- plot_prov_one_combo_ben(
  servsoc_ER_homecare,
  service_code = "HOMEHEALTH",
  data_type_code = "USSSPM",
  beneficiary_code = c("DIS", "ELD", "FAM"),
  year = 2022
)
p63_healthv_bar22_usr <- plot_prov_one_combo_ben(
  servsoc_ER_homecare,
  service_code = "HEALTHV",
  data_type_code = "USSSPM",
  beneficiary_code = c("DIS", "ELD", "FAM"),
  year = 2022
)

p63_otherhome_bar22_usr <- plot_prov_one_combo_ben(
  servsoc_ER_homecare,
  service_code = "OTHERHOME",
  data_type_code = "USSSPM",
  beneficiary_code = c("DIS", "ELD", "FAM"),
  year = 2022
)

# ____ Plots PERUSTPOP for 4 categories of service ----
# Check actual OBS_VALUE (shows if values are NA)
servsoc_ER_homecare |>
  filter(
    CATEGORY_OF_SERVICE %in%
      c("HOMECARE", "HOMEHEALTH", "HEALTHV", "OTHERHOME"),
    DATA_TYPE == "PERUSTPOP",
    TIME_PERIOD == 2022
  ) |>
  select(PROVINCIA, CATEGORY_OF_SERVICE, CATEG_OF_BENEFICIARIES, OBS_VALUE) |>
  filter(is.na(OBS_VALUE))

# Plots PERUSTPOP for 4 categories of service
p64_homecare_per_bar22_perurs <- plot_prov_one_combo_ben(
  servsoc_ER_homecare,
  service_code = "HOMECARE",
  data_type_code = "PERUSTPOP",
  beneficiary_code = c("DIS", "ELD", "FAM"),
  year = 2022
)

p64_homehealth_per_bar22_perusr <- plot_prov_one_combo_ben(
  servsoc_ER_homecare,
  service_code = "HOMEHEALTH",
  data_type_code = "PERUSTPOP",
  beneficiary_code = c("DIS", "ELD", "FAM"),
  year = 2022
)
p64_healthv_per_bar22_perusr <- plot_prov_one_combo_ben(
  servsoc_ER_homecare,
  service_code = "HEALTHV",
  data_type_code = "PERUSTPOP",
  beneficiary_code = c("DIS", "ELD", "FAM"),
  year = 2022
)

p64_otherhome_per_bar22_perusr <- plot_prov_one_combo_ben(
  servsoc_ER_homecare,
  service_code = "OTHERHOME",
  data_type_code = "PERUSTPOP",
  beneficiary_code = c("DIS", "ELD", "FAM"),
  year = 2022
)

# ____ Plots PERMOS for 4 categories of service ----
# Check actual OBS_VALUE (shows if values are NA) for all CATEG_OF_SERVICES
servsoc_ER_homecare |>
  filter(
    CATEGORY_OF_SERVICE %in%
      c("HOMECARE", "HOMEHEALTH", "HEALTHV", "OTHERHOME"),
    DATA_TYPE == "PERMOS",
    TIME_PERIOD == 2022
  ) |>
  select(PROVINCIA, CATEGORY_OF_SERVICE, CATEG_OF_BENEFICIARIES, OBS_VALUE) |>
  filter(is.na(OBS_VALUE))

# Plots PERMOS for 4 categories of service
p64_homecare_perCOM_bar22 <- plot_prov_one_combo_ben(
  servsoc_ER_homecare,
  service_code = "HOMECARE",
  data_type_code = "PERMOS",
  beneficiary_code = c("DIS", "ELD", "FAM"),
  year = 2022
)

p64_homehealth_perCOM_bar22 <- plot_prov_one_combo_ben(
  servsoc_ER_homecare,
  service_code = "HOMEHEALTH",
  data_type_code = "PERMOS",
  beneficiary_code = c("DIS", "ELD", "FAM"),
  year = 2022
)
p64_healthv_perCOM_bar22 <- plot_prov_one_combo_ben(
  servsoc_ER_homecare,
  service_code = "HEALTHV",
  data_type_code = "PERMOS",
  beneficiary_code = c("DIS", "ELD", "FAM"),
  year = 2022
)

p64_otherhome_perCOM_bar22 <- plot_prov_one_combo_ben(
  servsoc_ER_homecare,
  service_code = "OTHERHOME",
  data_type_code = "PERMOS",
  beneficiary_code = c("DIS", "ELD", "FAM"),
  year = 2022
)

# 4. SAVE all the named plots as rds in data/plots/
# list objects starting with p
plot_list <- ls(pattern = "^p6")

# Save each plot as an rds file using saveRDS
lapply(plot_list, function(plot_name) {
  saveRDS(
    object = get(plot_name),
    file = paste0("data/plots/", plot_name, ".rds")
  )
})


# 4. 🟧 TABLES with TERRITORI CONFRONTO --------------------------------------

# Load combined confronto dataset (all territori)
servsoc_cf_all <- readRDS(
  here("data/data_out/istat_SERVSOC_2011_2022/servsoc_territ_2011_2022.rds")
)



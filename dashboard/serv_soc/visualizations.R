# Visualizzazioni SERVIZI SOCIALI - Confronto tra PROVINCIE ER ----
# Comparison plots across all provinces of Emilia-Romagna
# Based on ISTAT data (2011-2022)

# IMPLEMENTATION PLAN FOR NEXT SESSION ----
# Priority order for implementing visualization functions:
#
# 1. START WITH: plot_provincie_one_combo()
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
#    - Takes plot_provincie_one_combo() and adds facet_wrap(~beneficiario)
#    - Shows DIS, ELD side by side
#    - Useful to compare if patterns differ by beneficiary type
#
# 4. THEN: plot_heatmap_provincie_anni()
#    - geom_tile() with PROVINCIA on y-axis, TIME_PERIOD on x-axis
#    - Fill aesthetic = OBS_VALUE
#    - scale_fill_gradient2() for diverging colors
#    - Shows evolution patterns visually
#
# 5. OPTIONAL: plot_beneficiari_facet_provincia()
#    - Reverse of #3: facet by province, show beneficiaries as lines
#    - Only useful if focusing on specific provinces
#
# 6. OPTIONAL: plot_grid_servizi_datatypes()
#    - Most complex - facet_grid(servizio ~ data_type_it)
#    - Overview of ALL combinations at once
#    - May be too crowded, evaluate after testing
#
# KEY CONSIDERATIONS:
# - All functions should handle missing data gracefully (return NULL or message)
# - Use consistent theme_serv_soc() across all plots
# - Add CAPTION to all plots
# - Test each function with servsoc_ER_homecare first (smaller dataset)
# - Save successful plots to data/plots/ for dashboard use
#
# SUGGESTED WORKFLOW:
# 1. Implement function #1
# 2. Test with multiple combinations to verify it works
# 3. Create 2-3 example plots and save them
# 4. Move to next function
# 5. Repeat

library(here)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)
library(ggiraph)


# 1. LOAD DATASETS ----

# Load combined ER dataset (all provinces)
servsoc_ER_all <- readRDS(
  here("data/data_out/istat_SERVSOC_2011_2022/servsoc_ER_all_2011_2022.rds")
)

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
colori_provincie <- c(
  "Bologna" = "#e41a1c",
  "Ferrara" = "#377eb8",
  "ForliCesena" = "#4daf4a",
  "Modena" = "#984ea3",
  "Parma" = "#ff7f00",
  "Piacenza" = "#ffff33",
  "Ravenna" = "#a65628",
  "ReggioEmilia" = "#f781bf",
  "Rimini" = "#999999"
)

# Color palette for beneficiary categories
colori_beneficiari <- c(
  "disabili" = "#d73027",
  "anziani" = "#4575b4",
  "totale" = "#4d4d4d"
)

CAPTION <- "Fonte: ISTAT - Interventi e servizi sociali dei comuni (2011-2022)"


# 3. VISUALIZATION FUNCTIONS ----

# Function 1: Compare provinces for one service/data_type/beneficiary combination
plot_provincie_one_combo <- function(
  data,
  service_code,
  data_type_code,
  beneficiary_code
) {
  # Filter for specific combination
  # Plot with PROVINCIA as different colored lines
  # X-axis: TIME_PERIOD (2011-2022)
  # Y-axis: OBS_VALUE
  # Color: PROVINCIA
  # Title: service name + beneficiary + data type
}

# Function 2: Compare provinces with facets by beneficiary
plot_provincie_facet_beneficiary <- function(
  data,
  service_code,
  data_type_code
) {
  # Filter for specific service and data type
  # Facet by beneficiario (DIS, ELD, TOT)
  # Each facet shows all provinces as lines
  # Useful to see if different beneficiaries have different provincial patterns
}

# Function 3: Compare beneficiaries with facets by province
plot_beneficiari_facet_provincia <- function(
  data,
  service_code,
  data_type_code
) {
  # Filter for specific service and data type
  # Facet by PROVINCIA
  # Each facet shows different beneficiaries as colored lines
  # Useful to see within-province patterns across beneficiary types
}

# Function 4: Heatmap - provinces x years
plot_heatmap_provincie_anni <- function(
  data,
  service_code,
  data_type_code,
  beneficiary_code
) {
  # Filter for specific combination
  # Create heatmap with:
  # X-axis: TIME_PERIOD
  # Y-axis: PROVINCIA
  # Fill: OBS_VALUE
  # Use scale_fill_gradient2 for diverging colors
}

# Function 5: Small multiples - grid of service x data_type
plot_grid_servizi_datatypes <- function(
  data,
  beneficiary_code = "DIS"
) {
  # Create faceted plot with:
  # Facet by servizio (rows) and data_type_it (columns)
  # Each facet shows provinces as lines
  # Useful for overview of all service/indicator combinations
}

# Function 6: Ranking bar plot - latest year comparison
plot_ranking_provincie_latest <- function(
  data,
  service_code,
  data_type_code,
  beneficiary_code,
  year = 2022
) {
  # Filter for latest year (default 2022)
  # Create horizontal bar plot ranking provinces
  # Highlight Parma in different color
  # Useful to see current state across provinces
}


# 4. EXAMPLE USAGE (to be executed) ----

# Example 1: Compare provinces for home care users (disabled)
plot_provincie_one_combo(
  servsoc_ER_homecare,
  service_code = "HOMECARE",
  data_type_code = "USSSPM",
  beneficiary_code = "DIS"
)

# Example 2: Facet by beneficiary
plot_provincie_facet_beneficiary(
  servsoc_ER_homecare,
  service_code = "HOMECARE",
  data_type_code = "USSSPM"
)

# Example 3: Heatmap
plot_heatmap_provincie_anni(
  servsoc_ER_homecare,
  service_code = "HOMECARE",
  data_type_code = "USSSPM",
  beneficiary_code = "DIS"
)

# 5. SAVE PLOTS FOR DASHBOARD ----

# Create and save specific plots for dashboard/serv_soc/index.qmd

# p01_homecare_dis_provincie <- plot_provincie_one_combo(...)
# saveRDS(p01_homecare_dis_provincie, here("data/plots/p01_homecare_dis_provincie.rds"))

# p02_homecare_eld_provincie <- plot_provincie_one_combo(...)
# saveRDS(p02_homecare_eld_provincie, here("data/plots/p02_homecare_eld_provincie.rds"))

# ... etc

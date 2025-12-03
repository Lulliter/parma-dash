# Esplorazione dati SERVIZI SOCIALI ----
# Uses cleaned data from data_load_ALL_ER.R
# Exploratory visualizations to understand the data

library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# 1. LOAD CLEANED DATA ----
# (Created by data_load_ALL_ER.R)

servsoc_ER_all <- readRDS(
  here("data/data_out/istat_SERVSOC_2011_2022/servsoc_ER_all_2011_2022.rds")
)

servsoc_ER_homecare <- readRDS(
  here("data/data_out/istat_SERVSOC_2011_2022/servsoc_ER_homecare_2011_2022.rds")
)

# Explore structure
glimpse(servsoc_ER_all)
dim(servsoc_ER_all)

# Check provinces
servsoc_ER_all |> count(PROVINCIA)

# Check data availability
servsoc_ER_homecare |>
  count(PROVINCIA, CATEG_OF_BENEFICIARIES, CATEGORY_OF_SERVICE, DATA_TYPE)


# 2. FILTER FOR EXPLORATION ----
# Focus on Parma for initial exploration

parma_data <- servsoc_ER_homecare |>
  filter(PROVINCIA == "Parma")

# Check dimensions
dim(parma_data)
parma_data |> count(CATEG_OF_BENEFICIARIES, beneficiario)


# 3. EXPLORATORY PLOTTING FUNCTION ----

plot_one_combination <- function(data, service_code, data_type_code) {
  # Filter for this specific combination
  plot_data <- data |>
    filter(
      CATEGORY_OF_SERVICE == service_code,
      DATA_TYPE == data_type_code
    )

  # Skip if no data
  if (nrow(plot_data) == 0) {
    return(NULL)
  }

  # Get labels for title
  service_name <- unique(plot_data$servizio)
  uoa_name <- unique(plot_data$data_type_it)

  # Create plot
  ggplot(plot_data, aes(x = TIME_PERIOD, y = OBS_VALUE, color = beneficiario)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5) +
    scale_x_continuous(breaks = seq(2011, 2022, by = 1)) +
    labs(
      title = service_name,
      subtitle = uoa_name,
      x = "Anno",
      y = "Valore",
      color = "Beneficiari"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 10, color = "gray40")
    )
}

# Get all unique combinations for Parma
combinations_parma <- parma_data |>
  distinct(CATEGORY_OF_SERVICE, DATA_TYPE, servizio, data_type_it) |>
  arrange(servizio, data_type_it)

# View available combinations
combinations_parma

# Create plots for each combination
for (i in 1:nrow(combinations_parma)) {
  service_code <- combinations_parma$CATEGORY_OF_SERVICE[i]
  dtype_code <- combinations_parma$DATA_TYPE[i]

  p <- plot_one_combination(parma_data, service_code, dtype_code)
  if (!is.null(p)) {
    print(p)
  }
}

# Or create individual plots manually:
# plot_one_combination(parma_data, "HOMECARE", "USSSPM")
# plot_one_combination(parma_data, "HOMECARE", "PERUSTPOP")
# plot_one_combination(parma_data, "HEALTHV", "USSSPM")

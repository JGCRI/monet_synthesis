
library(sf)
library(geodata)
library(terra)
library(tidyr)
library(dplyr)
library(ggplot2)

# Function to extract climate data: temperature and precipitation
process_climate_data <- function(coords_df, ID_column) {
  coords_extracted <- st_coordinates(coords_df)
  coords_data <- tibble(place = coords_df[[ID_column]], lon = coords_extracted[, "X"], lat = coords_extracted[, "Y"])
  coords_data$ID <- seq_len(nrow(coords_data))

  # WorldClim data extraction
  tavg <- worldclim_global("tavg", "10", "worldclim_data/")
  tprec <- worldclim_global("prec", "10", "worldclim_data/")

  tavg_extracted <- terra::extract(tavg, coords_data[, c("lon", "lat")])
  tprec_extracted <- terra::extract(tprec, coords_data[, c("lon", "lat")])

  # Calculate MAT and MAP
  coords_data <- coords_data %>%
    mutate(MAT = rowMeans(tavg_extracted[, -1], na.rm = TRUE),
           MAP = rowMeans(tprec_extracted[, -1], na.rm = TRUE) * 12) # Convert to annual precipitation

  # Reshape monthly temperature data
  monthly_temp <- pivot_longer(tavg_extracted, -ID, names_to = "name", values_to = "temperature")

  # Reshape monthly precipitation data
  monthly_prec <- pivot_longer(tprec_extracted, -ID, names_to = "name", values_to = "precipitation")

  monthly_temp$month <- as.numeric(gsub("wc2.1_10m_tavg_", "", monthly_temp$name))
  monthly_prec$month <- as.numeric(gsub("wc2.1_10m_prec_", "", monthly_prec$name))

  monthly_joined_temp <- monthly_temp %>% mutate(place = coords_data$place[monthly_temp$ID])
  monthly_joined_prec <- monthly_prec %>% mutate(place = coords_data$place[monthly_prec$ID])

  return(list(monthly_temp = monthly_joined_temp, monthly_prec = monthly_joined_prec, annual = coords_data))
}

# Plot function for climatology
plot_climatology <- function(monthly_data, title, y_label, value_column) {
  ggplot(monthly_data, aes(month, !!sym(value_column), color = factor(place))) +
    geom_line() +
    ylab(y_label) +
    ggtitle(title) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Process monet_rs_coords, subset 18 and plot
monet_data <- process_climate_data(monet_rs_coords, "Site_Code")
set.seed(42)
sampled_places_monet <- monet_data$annual %>% distinct(place) %>% sample_n(18)
sampled_temp_monet <- monet_data$monthly_temp %>% filter(place %in% sampled_places_monet$place)
sampled_prec_monet <- monet_data$monthly_prec %>% filter(place %in% sampled_places_monet$place)

p_monet_temp <- plot_climatology(sampled_temp_monet, "Temperature for Sampled Monet RS Coords", "Air temperature", "temperature")
p_monet_prec <- plot_climatology(sampled_prec_monet, "Precipitation for Sampled Monet RS Coords", "Precipitation", "precipitation")
print(p_monet_temp)
print(p_monet_prec)

# Process SRDB_coords, subset 18 and plot
srdb_data <- process_climate_data(SRDB_coords, "Record_number")
set.seed(42)
sampled_places_srdb <- srdb_data$annual %>% distinct(place) %>% sample_n(18)
sampled_temp_srdb <- srdb_data$monthly_temp %>% filter(place %in% sampled_places_srdb$place)
sampled_prec_srdb <- srdb_data$monthly_prec %>% filter(place %in% sampled_places_srdb$place)

p_srdb_temp <- plot_climatology(sampled_temp_srdb, "Temperature for Sampled SRDB Coords", "Air temperature", "temperature")
p_srdb_prec <- plot_climatology(sampled_prec_srdb, "Precipitation for Sampled SRDB Coords", "Precipitation", "precipitation")
print(p_srdb_temp)
print(p_srdb_prec)

# Final annual dataframe
print("Annual data for Monet RS Coords")
print(monet_data$annual)
print("Annual data for SRDB Coords")
print(srdb_data$annual)

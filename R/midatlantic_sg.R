
library(terra)
library(sf)
library(tidyterra)
library(ggplot2)

# state_abbreviations_conus <- c(
#   "AL", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "ID", "IL", "IN", "IA", "KS", "KY",
#   "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
#   "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA",
#   "WV", "WI", "WY"
# )

# conus_valid <- st_read("data/shapefiles/s_18mr25/s_18mr25.shp") %>%
#   st_make_valid() %>%
#   filter(STATE %in% state_abbreviations_conus) %>%
#   st_transform("WGS84")

# climate_zones_sf <- st_read("data/shapefiles/na_climatezones_shapefile/climatezones_shapefile/NA_ClimateZones/data/North_America_Climate_Zones.shp")%>%
#   st_transform(crs(conus_valid))

# Step 1: Load and crop raster
sg_clay_full <- terra::rast("./data/soilgrids/crop_roi_igh_clay_0-5cm.tif") # Load full raster
crop_extent <- terra::ext(-78, -74.5, 38.25, 40.5) # Define crop extent
sg_clay_top_prj <- sg_clay_full %>%
  project(crs(climate_zones_sf))
sg_clay_top_prj_final <- terra::crop(sg_clay_top_prj, crop_extent)  # Crop raster to extent

# Step 2: Reproject to match vector layer CRS
target_crs <- sf::st_crs(conus_valid)$wkt      # Extract CRS from `sf` object
sg_clay_top_prj <- terra::project(sg_clay_top_prj_final, target_crs) # Reproject raster

# Step 3: Apply transformation
sg_clay_top_prj <- sg_clay_top_prj / 10  # Scale raster values as needed

terra::writeRaster(sg_clay_top_prj, "sg_mini_clay_top_prj.tif", overwrite = TRUE)

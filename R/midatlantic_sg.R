
library(terra)
library(sf)
library(tidyterra)
library(ggplot2)

# Step 1: Load and crop raster
sg_clay_full <- terra::rast("./data/soilgrids/crop_roi_igh_clay_0-5cm.tif") # Load full raster
crop_extent <- terra::ext(-78, -74.5, 38.25, 40.5) # Define crop extent
sg_clay_top_prj <- sg_clay_full %>%
  project(crs(climate_zones_sf))
sg_clay_top_prj_final <- terra::crop(sg_clay_top_prj, crop_extent)  # Crop raster to extent

# Step 2: Reproject to match vector layer CRS
target_crs <- sf::st_crs(conus_valid)$wkt                # Extract CRS from `sf` object
sg_clay_top_prj <- terra::project(sg_clay_top_prj_final, target_crs) # Reproject raster

# Step 3: Apply transformation
sg_clay_top_prj <- sg_clay_top_prj / 10  # Scale raster values as needed

# Step 4: Plot with ggplot
ggplot() +
  tidyterra::geom_spatraster(data = sg_clay_top_prj) +
  geom_sf(data = conus_valid, color = "grey", alpha = 0.3) +
  geom_sf(data = top_clay_test, color = "black", size = 5) +
  geom_sf(data = top_clay_test, aes(color = Site_Code), size = 3.5, alpha = 0.8) +
  coord_sf(xlim = c(-78, -74.5), ylim = c(38.25, 40.5)) +
  theme_bw() +
  scale_fill_gradient(low = "white", high = "blue", na.value = NA) +
  labs(title = "MONet data points without Soilgrids data")

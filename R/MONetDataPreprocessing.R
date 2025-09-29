#
# title: "Morris-Wiens_MONet_data_processing"
# author: "Kendalynn A. Morris & Nathan J. Wiens"
#

# Introduction ----------------------------------------------------------------
# This script is designed to download and process the data needed for comparing
# MONet data to SoilGrids and SRDB.

# Load the packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(sf)
library(terra)
library(tidyterra)
library(raster)
library(tiff)
library(geodata)
library(httr)
library(utils)
library(gdalUtilities)
library(ggpmisc)
library(inborutils)

# Check for existing data

if (any(file.exists(c(
  "R_data/processed_clay.RData",
  "R_data/processed_pH.RData",
  "R_data/processed_Rs.RData"
)))) {
  stop("Please remove existing processed data in R_data before running this script")
}

# 1. Download SRDB from github -------------------------------------------------

# Define the URL of the specific release asset
# You can find the asset URL by navigating to the release page on GitHub and
# copying the download link for the file you want
release_url <- "https://github.com/bpbond/srdb/archive/refs/tags/v20250503a.zip"

# Define the destination file path for the downloaded zip
dir <- print(getwd())
destfile <- "/data/srdb-20250503a"

# Use download.file() to download the file
download.file(url = release_url, destfile = paste0(dir, destfile), mode = "wb")

extraction_directory <- "/data/srdb"
dir.create(file.path(dir, extraction_directory))

# Check if the file was downloaded successfully
if (file.exists(paste0(dir, destfile))) {
  # Try unzipping the file and catch any errors
  tryCatch(
    {
      unzip(paste0(dir, destfile), exdir = paste0(dir, extraction_directory))
      cat("Download and extraction of release data complete!\n")
    },
    error = function(e) {
      cat("Error in extraction:", e$message, "\n")
    }
  )
} else {
  cat("Failed to download the release file.\n")
}

# Remove the downloaded zip file
file.remove(paste0(dir, destfile))


# 2. Download SoilGrids clay content and pH data for CONUS ---------------------

# Define geographic coordinates for the bounding box
bbox_geo <- matrix(
  c(
    -125, 24, # Bottom-left corner: xmin, ymin
    -66, 49 # Top-right corner: xmax, ymax
  ),
  nrow = 2, byrow = TRUE
)

# Create a spatial object with these coordinates
bbox_geo_sf <- st_sfc(st_polygon(list(rbind(
  c(bbox_geo[1, 1], bbox_geo[1, 2]), # Bottom-left
  c(bbox_geo[2, 1], bbox_geo[1, 2]), # Bottom-right
  c(bbox_geo[2, 1], bbox_geo[2, 2]), # Top-right
  c(bbox_geo[1, 1], bbox_geo[2, 2]), # Top-left
  c(bbox_geo[1, 1], bbox_geo[1, 2]) # Closing point to form the polygon
))), crs = 4326)

# Goode Homolosine (igh) projection
proj_string <- "+proj=igh +datum=WGS84 +no_defs"

# Transform geographic coordinates to IGH
bbox_igh_sf <- st_transform(bbox_geo_sf, crs = proj_string)

# Get the bounding box of our ROI
# now in the appropriate igh projection
bbox <- st_bbox(bbox_igh_sf)

# Format for gdalUtilities
ulx <- bbox$xmin
uly <- bbox$ymax
lrx <- bbox$xmax
lry <- bbox$ymin
(bb <- c(ulx, uly, lrx, lry))

# SoilGrids url
sg_url <- "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"

# Define coordinate reference system
igh <- "+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs"

# gdal_translate goes to SoilGrid's webpage and
# downloads the .vrt data in a .tif in your workspace
# this is retrieving the mean variable of interest for surface soil
# many other data types are available

# pH 0-5cm
gdal_translate(paste0(sg_url, "phh2o/phh2o_0-5cm_mean.vrt"), # units: ph * 10
  "./data/soilgrids/crop_roi_igh_ph_0-5cm.tif",
  tr = c(250, 250),
  projwin = bb,
  projwin_srs = igh
)

# pH 15-30cm
gdal_translate(paste0(sg_url, "phh2o/phh2o_15-30cm_mean.vrt"), # units: ph * 10
  "./data/soilgrids/crop_roi_igh_ph_15-30cm.tif",
  tr = c(250, 250),
  projwin = bb,
  projwin_srs = igh
)

# clay 0-5cm
gdal_translate(paste0(sg_url, "clay/clay_0-5cm_mean.vrt"), # units: g/kg
  "./data/soilgrids/crop_roi_igh_clay_0-5cm.tif",
  tr = c(250, 250),
  projwin = bb,
  projwin_srs = igh
)

# clay 15-30cm
gdal_translate(paste0(sg_url, "clay/clay_15-30cm_mean.vrt"), # units: g/kg
  "./data/soilgrids/crop_roi_igh_clay_15-30cm.tif",
  tr = c(250, 250),
  projwin = bb,
  projwin_srs = igh
)


# 3. Read in MONet respiration and SRDB data -----------------------------------

# Create color palette for plotting
myColors <- c("#000080", "#4040A0", "#8080C0", "#D6AA80", "#D98040", "#CC5500")
names(myColors) <- c(
  "MONet_resp", "MONet_pH", "MONet_clay",
  "comp_clay", "comp_pH", "comp_resp"
)

# If your data directory is missing any of the files below, download them from
# their respective sources into the "data" folder.

# MONet soil respiration (https://zenodo.org/records/15328215)
# United States shapefile (https://www.weather.gov/gis/USStates)

# read in site coordinates and data
monet_rs <- read.csv("data/MONet/1000S_processed_L2_summary.csv")
rs_coord <- read.csv("data/MONet/1000Soils_Metadata_Site_Mastersheet_v1.csv")
srdb <- read.csv("data/srdb/srdb-20250503a/srdb-data.csv")

# state codes for filtering CONUS
state_abbreviations_conus <- c(
  "AL", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "ID", "IL", "IN", "IA", "KS", "KY",
  "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
  "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA",
  "WV", "WI", "WY"
)

# Read and ensure valid shapes for CONUS
conus_valid <- st_read("data/shapefiles/s_18mr25/s_18mr25.shp") %>%
  st_make_valid() %>%
  filter(STATE %in% state_abbreviations_conus) %>%
  st_transform("EPSG:5070")

# Define the target CRS from the shapefile
target_crs <- st_crs(conus_valid)

# Disable S2 processing to use GEOS for spatial operations
sf_use_s2(FALSE)

# Filter SRDB coordinates within shapefile boundaries
SRDB_coords <- srdb %>%
  dplyr::select(Record_number, Latitude, Longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(target_crs) %>%
  {
    .[rowSums(st_within(., conus_valid, sparse = FALSE)) > 0, ]
  }

# Convert coordinates to sf assuming the column names are 'Long' and 'Lat'
monet_rs_coords <- rs_coord %>%
  filter(Site_Code != "PUUM") %>% # Take out one site in HI
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  st_transform(target_crs)

# Join 1000 soil sample data and location
processed_1000s_loc <- monet_rs %>%
  separate(Sampling_Set, into = c("prj", "Site_Code")) %>%
  left_join(monet_rs_coords, by = c("Site_Code")) %>%
  filter(!st_is_empty(geometry)) %>%
  mutate(
    source = "MONet", sample = paste0(prj, "_", Site_Code),
    label = if_else(!is.na(respiration_ppm_co2_c), "MONet ppm", "MONet rate")
  ) %>%
  # format to MONet structure
  dplyr::select(
    proposal_id = Proposal_ID, sample,
    core_section = Core_Section, pH, Clay_percent, respiration_ppm_co2_c,
    respiration_mg_co2_c_g_soil_day_24hour,
    geometry, label, source
  )

rs_sites <- processed_1000s_loc %>%
  filter(!is.na(respiration_mg_co2_c_g_soil_day_24hour) | !is.na(respiration_ppm_co2_c))

# Plotting both Rs datasets
srdb_MONet_conus_sites <- ggplot() +
  geom_sf(data = conus_valid, fill = NA, color = "darkslategray", linetype = "solid") +
  geom_sf(data = SRDB_coords, aes(color = "SRDB"), size = 1.8, alpha = 0.7) +
  geom_sf(data = rs_sites, aes(geometry = geometry, shape = label, color = "MONet"), size = 2, alpha = 0.7) +
  scale_color_manual(values = c("SRDB" = myColors[[6]], "MONet" = myColors[[1]])) +
  scale_shape_manual(values = c(1, 16, 19)) +
  theme_bw() +
  labs(
    title = "Soil Respiration Field Sites",
    color = "Data Source",
    shape = "Rs unit"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16)
  )

ggsave(plot = srdb_MONet_conus_sites, file = "./figures/srdb_MONet_conus_sites.png", width = 8, height = 5.3)


# 4. Reading pH and clay content data from MONet and 1000 soils database -------

# If your data directory is missing any of the files below, download
# them from their respective sources put into the "data" folder.

# MONet (https://sc-data.emsl.pnnl.gov/monet)
# 1000 soils (https://zenodo.org/records/15328215)

# site coordinates and data
monet_pH_coord <- read.csv("data/MONet/pH/processed_data/Coordinates.csv")
monet_clay_coord <- read.csv("data/MONet/clay/processed_data/Coordinates.csv")

monet_pH_data <- read.csv("data/MONet/pH/processed_data/Soil_BioChemical_properties.csv")
monet_clay_data <- read.csv("data/MONet/clay/processed_data/Soil_BioChemical_properties.csv")

## Join pH and clay site locations to data and combine MONet and 1000 soils-----

# Join MONet pH sample data and location
monet_pH_loc <- monet_pH_data %>%
  # Separate the Sample_Name column into distict columns
  separate(Sample_Name, into = c("proposal_id", "sample", "core_section")) %>%
  mutate(
    proposal_id = as.numeric(proposal_id),
    sample = as.numeric(sample)
  ) %>%
  # Join to the coordinated by proposal ID and sample set
  left_join(monet_pH_coord, by = c("proposal_id", "sample" = "sampling_set")) %>%
  # Resolves longitude mislabeling
  mutate(
    longitude = if_else(longitude > 0, longitude * -1, longitude),
    sample = as.character(sample)
  ) %>%
  dplyr::select(proposal_id, sample, core_section, pH, latitude, longitude) %>%
  mutate(source = "MONet")

# Join MONet clay content sample data and location
monet_clay_loc <- monet_clay_data %>%
  # Separate the Sample_Name column into distict columns
  separate(Sample_Name, into = c("proposal_id", "sample", "core_section")) %>%
  mutate(
    proposal_id = as.numeric(proposal_id),
    sample = as.numeric(sample)
  ) %>%
  left_join(monet_clay_coord, by = c("proposal_id", "sample" = "sampling_set")) %>%
  # Resolves longitude mislabeling
  mutate(
    longitude = if_else(longitude > 0, longitude * -1, longitude),
    sample = as.character(sample)
  ) %>%
  dplyr::select(proposal_id, sample, core_section, Clay_percent, latitude, longitude) %>%
  mutate(source = "MONet")

## Convert MONet data points to `sf` objects and extract Koppen climate zone information ----

# Convert MONet pH data into shapefile object setting CRS to the target, clip points within CONUS
MONet_crs <- "EPSG:4326"

# Convert MONet to sf and drop points outside of CONUS then bind with 1000s pH data
pH_loc_sf <- st_as_sf(monet_pH_loc, coords = c("longitude", "latitude"), crs = MONet_crs) %>%
  st_transform(target_crs) %>%
  st_intersection(st_union(conus_valid)) %>%
  bind_rows(dplyr::select(processed_1000s_loc, proposal_id, sample, core_section, pH, source, geometry)) %>%
  filter(!is.na(pH))

# Filter for top and bottom samples
pH_loc_sf_top <- pH_loc_sf %>% filter(core_section == "TOP")
pH_loc_sf_btm <- pH_loc_sf %>% filter(core_section == "BTM")

# Convert MONet to sf and drop points outside of CONUS then and bind with 1000s clay data
clay_loc_sf <- st_as_sf(monet_clay_loc, coords = c("longitude", "latitude"), crs = MONet_crs) %>%
  st_transform(target_crs) %>%
  st_intersection(st_union(conus_valid)) %>%
  bind_rows(dplyr::select(processed_1000s_loc, proposal_id, sample, core_section, Clay_percent, source, geometry)) %>%
  filter(!is.na(Clay_percent))

# Filter for top and bottom samples
clay_loc_sf_top <- clay_loc_sf %>% filter(core_section == "TOP")
clay_loc_sf_btm <- clay_loc_sf %>% filter(core_section == "BTM")

## Plot Koppen Zones ----
# Load the shapefile as an `sf` object
climate_zones_sf <- st_read("data/shapefiles/na_climatezones_shapefile/climatezones_shapefile/NA_ClimateZones/data/North_America_Climate_Zones.shp") %>%
  st_transform(crs(conus_valid))

# Create a mapping table for the 3 letter climate zone code to the full name
climate_zone_mapping <- climate_zones_sf %>%
  distinct(Code, Climate)

# Find intersection of climate zones and CONUS
climate_zones_conus_sf <- climate_zones_sf %>%
  st_intersection(st_union(conus_valid))

conus_climate_zones_MONet <- ggplot() +
  geom_sf(data = climate_zones_conus_sf, aes(fill = Climate), color = "darkslategrey", alpha = 1) +
  geom_sf(data = conus_valid, color = "black", alpha = 0.3) +
  geom_sf(data = pH_loc_sf, size = 2.5, color = "white") +
  geom_sf(data = pH_loc_sf, size = 2, color = "#000080") +
  theme_bw() +
  labs(title = "CONUS Koppen Climate Zones of North America with MONet Sites") +
  scale_color_discrete(drop = T) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16)
  )

ggsave(plot = conus_climate_zones_MONet, file = "figures/conus_climate_zones_MONet.png", width = 8, height = 5.3)

# Join point data to Koppen shapefile to extract climate zone information for each point
pH_zones <- st_join(pH_loc_sf, climate_zones_conus_sf) %>%
  dplyr::select(proposal_id, sample, core_section, pH, source, Code, Climate, Key_EN, geometry) %>%
  filter(!is.na(Climate))
clay_zones <- st_join(clay_loc_sf, climate_zones_conus_sf) %>%
  dplyr::select(proposal_id, sample, core_section, Clay_percent, source, Code, Climate, Key_EN, geometry) %>%
  filter(!is.na(Climate))

# Read in soilGrids data and clip to CONUS -------------------------------------

# Reads in a raster from the specified file path, projects it to the target CRS,
# and masks by the second input raster.
read_project_mask <- function(raster_path, conus_valid) {
  # Read in raster from path
  raster <- rast(raster_path)

  # create a projected conus object to crop input raster
  conus_crop <- conus_valid %>%
    st_transform(crs(raster))

  # crop raster to conus extent
  raster_crop <- crop(raster, ext(conus_crop))

  # project cropped raster to project CRS
  prj_raster <- raster_crop %>%
    project(crs(conus_valid))

  # mask projected raster by conus
  prj_raster_mask <- raster::mask(prj_raster, conus_valid)

  return(prj_raster_mask)
}
# This chunk accomplishes two functions. First it extracts the SoilGrids data at
# the location of each MONet sample site. The second function of this chunk is
# to extract the SoilGrids data by their appropriate climate zone.

# Mask SoilGrids data to CONUS
sg_clay_conus_top <- read_project_mask("./data/soilgrids/crop_roi_igh_clay_0-5cm.tif", conus_valid)
sg_clay_conus_btm <- read_project_mask("./data/soilgrids/crop_roi_igh_clay_15-30cm.tif", conus_valid)

sg_pH_conus_top <- read_project_mask("./data/soilgrids/crop_roi_igh_ph_0-5cm.tif", conus_valid)
sg_pH_conus_btm <- read_project_mask("./data/soilgrids/crop_roi_igh_ph_15-30cm.tif", conus_valid)

# Extracts SoilGrids clay and pH data from the MONet site locations
# Values are divided by 10

# Clay
sg_clay_monet_top <- terra::extract(sg_clay_conus_top, clay_loc_sf_top, fun = mean, bind = TRUE)
sg_clay_monet_top_values <- values(sg_clay_monet_top) %>%
  mutate(crop_roi_igh_clay_0.5cm = crop_roi_igh_clay_0.5cm / 10)

sg_clay_monet_btm <- terra::extract(sg_clay_conus_btm, clay_loc_sf_btm, fun = mean, bind = TRUE)
sg_clay_monet_btm_values <- values(sg_clay_monet_btm) %>%
  mutate(crop_roi_igh_clay_15.30cm = crop_roi_igh_clay_15.30cm / 10)

# pH
sg_pH_monet_top <- terra::extract(sg_pH_conus_top, pH_loc_sf_top, fun = mean, na.rm = TRUE, bind = TRUE)
sg_pH_monet_top_values <- values(sg_pH_monet_top) %>%
  mutate(crop_roi_igh_ph_0.5cm = crop_roi_igh_ph_0.5cm / 10)

sg_pH_monet_btm <- terra::extract(sg_pH_conus_btm, pH_loc_sf_btm, fun = mean, na.rm = TRUE, bind = TRUE)
sg_pH_monet_btm_values <- values(sg_pH_monet_btm) %>%
  mutate(crop_roi_igh_ph_15.30cm = crop_roi_igh_ph_15.30cm / 10)

# Extracts all SoilGrids values for each climate zone in CONUS
soilgrids_by_zone <- function(soilgrids, climate_zones_conus_sf) {
  # Convert the sf object (climate zones) to a terra SpatVector
  climate_zones_vect <- vect(climate_zones_conus_sf)

  # Mask clay content raster by each climate zone
  var_by_zone <- list() # Create a list to store masked rasters for each zone
  values_by_zone <- list()
  climate_zone_names <- unique(climate_zones_conus_sf$Code)[1:21]

  for (zone_name in climate_zone_names) {
    # Filter the specific climate zone
    single_zone <- climate_zones_vect[climate_zones_conus_sf$Code == zone_name, ]
    # Mask clay content to just this zone
    var_zone <- mask(crop(soilgrids, single_zone), single_zone)

    values_by_zone[[zone_name]] <- unique(values(var_zone, na.rm = TRUE)) # Remove NA values
  }

  var_long <- do.call(rbind, lapply(names(values_by_zone), function(zone_name) {
    tibble(zone = zone_name, value = values_by_zone[[zone_name]])
  }))

  return(var_long)
}

clay_top_long <- soilgrids_by_zone(sg_clay_conus_top, climate_zones_conus_sf)
clay_btm_long <- soilgrids_by_zone(sg_clay_conus_btm, climate_zones_conus_sf)

pH_top_long <- soilgrids_by_zone(sg_pH_conus_top, climate_zones_conus_sf)
pH_btm_long <- soilgrids_by_zone(sg_pH_conus_btm, climate_zones_conus_sf)

# For spatial comparisons ------------------------------------------------------
sg_clay_conus_top_scaled <- sg_clay_conus_top / 10 # Scale raster values as needed

# SoilGrids and MONet Histograms -----------------------------------------------

# Processes and joins MONet and SoilGrids dataframes for histogram plotting
process_soil_data <- function(type, section, MONet_df, sg_df, climate_mapping_df, sg_column_name) {
  # Process MONet data
  MONet_data <- MONet_df %>%
    left_join(climate_mapping_df, by = c("Climate", "Code")) %>%
    filter(core_section == section) %>%
    dplyr::select(Climate, !!type) %>%
    mutate(source = "MONet")

  # Process SoilGrids data
  sg_data <- sg_df %>%
    left_join(climate_mapping_df, by = c("zone" = "Code")) %>%
    filter(Climate %in% unique(MONet_data$Climate)) %>%
    rename(!!type := sg_column_name) %>%
    dplyr::select(Climate, !!type) %>%
    mutate(
      source = "SoilGrids",
      !!type := !!sym(type) / 10
    ) # convert units

  # Combine MONet and SoilGrids data
  combined_data <- bind_rows(MONet_data, sg_data)
  return(combined_data)
}

clay_MONet_sg_top <- process_soil_data(
  type = "Clay_percent",
  section = "TOP",
  MONet_df = clay_zones,
  sg_df = clay_top_long,
  climate_mapping_df = climate_zone_mapping,
  sg_column_name = "value" # "crop_roi_igh_clay_0.5cm"
)

clay_MONet_sg_btm <- process_soil_data(
  type = "Clay_percent",
  section = "BTM",
  MONet_df = clay_zones,
  sg_df = clay_btm_long,
  climate_mapping_df = climate_zone_mapping,
  sg_column_name = "value" # "crop_roi_igh_clay_15.30cm"
)

## Histograms ----
clay_top <- clay_MONet_sg_top %>%
  mutate(Experiment = as.factor(if_else(source == "MONet", "MONet_clay", "comp_clay"))) %>%
  ggplot(aes(x = Clay_percent, fill = Experiment)) +
  geom_histogram(mapping = aes(y = after_stat(density)), position = "identity", alpha = 0.7) +
  theme_bw() +
  scale_fill_manual(name = "Experiment", values = myColors, labels = c("comp_clay" = "SoilGrids", "MONet_clay" = "MONet")) +
  labs(
    title = "SoilGrids and MONet Clay Share Distribution (Top sample)",
    x = "Clay Content (%)"
  )

clay_btm <- clay_MONet_sg_btm %>%
  mutate(Experiment = as.factor(if_else(source == "MONet", "MONet_clay", "comp_clay"))) %>%
  ggplot(aes(x = Clay_percent, fill = Experiment)) +
  geom_histogram(mapping = aes(y = after_stat(density)), position = "identity", alpha = 0.7) +
  theme_bw() +
  scale_fill_manual(name = "Experiment", values = myColors, labels = c("comp_clay" = "SoilGrids", "MONet_clay" = "MONet")) +
  labs(
    title = "SoilGrids and MONet Clay Share Distribution (Bottom sample)",
    x = "Clay Content (%)"
  )

pH_MONet_sg_top <- process_soil_data(
  type = "pH",
  section = "TOP",
  MONet_df = pH_zones,
  sg_df = pH_top_long,
  climate_mapping_df = climate_zone_mapping,
  sg_column_name = "value" # "crop_roi_igh_ph_0.5cm"
)

pH_MONet_sg_btm <- process_soil_data(
  type = "pH",
  section = "BTM",
  MONet_df = pH_zones,
  sg_df = pH_btm_long,
  climate_mapping_df = climate_zone_mapping,
  sg_column_name = "value" # "crop_roi_igh_ph_15.30cm"
)

pH_top <- pH_MONet_sg_top %>%
  mutate(Experiment = as.factor(if_else(source == "MONet", "MONet_pH", "comp_pH"))) %>%
  ggplot(aes(x = pH, fill = Experiment)) +
  geom_histogram(mapping = aes(y = after_stat(density)), position = "identity", alpha = 0.7) +
  theme_bw() +
  scale_fill_manual(name = "Experiment", values = myColors, labels = c("comp_pH" = "SoilGrids", "MONet_pH" = "MONet")) +
  labs(
    title = "SoilGrids and MONet pH Distribution (Top sample)",
    x = "pH"
  )

pH_btm <- pH_MONet_sg_btm %>%
  mutate(Experiment = as.factor(if_else(source == "MONet", "MONet_pH", "comp_pH"))) %>%
  ggplot(aes(x = pH, fill = Experiment)) +
  geom_histogram(mapping = aes(y = after_stat(density)), position = "identity", alpha = 0.7) +
  theme_bw() +
  scale_fill_manual(name = "Experiment", values = myColors, labels = c("comp_pH" = "SoilGrids", "MONet_pH" = "MONet")) +
  labs(
    title = "SoilGrids and MONet pH Distribution (Bottom sample)",
    x = "pH"
  )

clay_hist <- ggarrange(clay_top, clay_btm, nrow = 2, ncol = 1)
ggsave(plot = clay_hist, "./figures/clay_hist.png", width = 6, height = 7)
ggsave(plot = clay_top, "./figures/clay_top.png", width = 6, height = 4)
ggsave(plot = clay_btm, "./figures/clay_btm.png", width = 6, height = 4)

pH_hist <- ggarrange(pH_top, pH_btm, nrow = 2, ncol = 1)
ggsave(plot = pH_hist, "./figures/pH_hist.png", width = 6, height = 7)
ggsave(plot = pH_top, "./figures/pH_top.png", width = 6, height = 4)
ggsave(plot = pH_btm, "./figures/pH_btm.png", width = 6, height = 4)

# 1:1 Comparisons Buffer -------------------------------------------------------

# Creates a buffer of a specified size (in meters) around point data and extracts
# values in that region from the specified raster. For each extracted buffer region,
# the mean, standard deviation, and number of raster points within the buffer are
# recorded.
extract_buffer <- function(raster, point_data, buffer_size_m, output_cols) {
  # Create buffer region around MONet data points
  buffer_points <- st_buffer(sf_transform_xy(point_data,
    source_crs = point_data,
    target_crs = point_data
  ), units::set_units(buffer_size_m, meters))

  # Extract data from buffer region from raster data and calculate mean and sd
  extract_buffer_region <- raster::extract(raster, buffer_points,
    fun = function(x) {
      list(
        mean = mean(x / 10, na.rm = TRUE),
        count = length(x[!is.na(x)]),
        sd = sd(x / 10, na.rm = TRUE)
      )
    },
    bind = TRUE
  ) %>% data.frame()
  # rename data columns
  names(extract_buffer_region)[(ncol(extract_buffer_region) - 2):ncol(extract_buffer_region)] <- output_cols

  extract_buffer_region[output_cols] <- lapply(extract_buffer_region[output_cols], as.numeric)

  return(extract_buffer_region)
}

# Clay buffer
sg_clay_buffer_top <- extract_buffer(sg_clay_conus_top, clay_loc_sf_top, buffer_size_m = 1000, output_cols = c("sg_mean", "sg_count", "sg_sd"))
sg_clay_buffer_btm <- extract_buffer(sg_clay_conus_btm, clay_loc_sf_btm, buffer_size_m = 1000, output_cols = c("sg_mean", "sg_count", "sg_sd"))
# combine top and bottom samples
sg_clay_buffer <- rbind(sg_clay_buffer_top, sg_clay_buffer_btm)

# pH buffer
sg_pH_buffer_top <- extract_buffer(sg_pH_conus_top, pH_loc_sf_top, buffer_size_m = 1000, output_cols = c("sg_mean", "sg_count", "sg_sd"))
sg_pH_buffer_btm <- extract_buffer(sg_pH_conus_btm, pH_loc_sf_btm, buffer_size_m = 1000, output_cols = c("sg_mean", "sg_count", "sg_sd"))
# combine top and bottom samples
sg_pH_buffer <- rbind(sg_pH_buffer_top, sg_pH_buffer_btm)

# Join Rs MONet data with coordinate metadata by site code and core section
combined_monet <- monet_rs %>%
  mutate(
    Site_Code = sapply(strsplit(Sample_Name, "_"), `[`, 3),
    Core_Section = sapply(strsplit(Sample_Name, "_"), `[`, 4)
  ) %>%
  left_join(rs_coord, by = c("Site_Code")) %>%
  dplyr::select(
    Sample_Name, respiration_ppm_co2_c, respiration_mg_co2_c_g_soil_day_24hour, respiration_mg_co2_c_g_soil_day_96hour,
    Site_Code, Core_Section, Lat, Long
  )

# Reads in gridded Rs data, projects to working CRS, and masks to CONUS
soil_Rh_conus <- read_project_mask("./data/SoilResp_HeterotrophicResp_1928/data/soil_Rh_mean.tif", conus_valid)

# Extracts data around MONet points
soil_Rh_location <- extract_buffer(soil_Rh_conus, monet_rs_coords, 1000, output_cols = c("srdb_mean", "srdb_count", "srdb_sd"))

# Joins MONet data to extracted values from gridded Rs product
soil_Rh_buffer <- monet_rs %>%
  mutate(
    Site_Code = sapply(strsplit(Sample_Name, "_"), `[`, 3),
    Core_Section = sapply(strsplit(Sample_Name, "_"), `[`, 4)
  ) %>%
  left_join(soil_Rh_location, by = c("Site_Code")) %>%
  dplyr::select(
    Sample_Name, respiration_ppm_co2_c, respiration_mg_co2_c_g_soil_day_24hour, respiration_mg_co2_c_g_soil_day_96hour,
    Site_Code, Core_Section, srdb_mean, srdb_count, srdb_sd
  )

## 1:1 Plots ----
# Create plots of clay and pH data comparing MONet values to the value of the
# sample location in SoilGrids with a confidence interval of the buffer region
# around each data point in SoilGrids.

# clay
sg_clay_buffer_plot <- sg_clay_buffer %>%
  mutate(
    ymin = sg_mean - sg_sd,
    ymax = sg_mean + sg_sd
  ) %>%
  ggplot(aes(x = Clay_percent, y = sg_mean, color = core_section, fill = core_section)) +
  geom_point() +
  geom_errorbar(aes(x = Clay_percent, ymin = ymin, ymax = ymax)) +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  stat_smooth(formula = y ~ x, method = "lm", se = TRUE, level = 0.95) +
  stat_poly_eq(
    aes(
      label = paste(..eq.label.., ..rr.label.., sep = "~~~"),
      group = core_section,
      color = core_section
    ),
    formula = y ~ x,
    parse = TRUE,
    size = 3
  ) +
  labs(
    title = "SoilGrids vs MONet Clay Percent",
    x = "MONet (% Clay)", y = "SoilGrids (% Clay)"
  ) +
  theme_bw()

ggsave(plot = sg_clay_buffer_plot, file = "./figures/clay_OneToOne_plot.png", width = 6, height = 5)

# pH
sg_pH_buffer_plot <- sg_pH_buffer %>%
  mutate(
    ymin = sg_mean - sg_sd,
    ymax = sg_mean + sg_sd
  ) %>%
  ggplot(aes(x = pH, y = sg_mean, color = core_section, fill = core_section)) +
  geom_point() +
  geom_errorbar(aes(x = pH, ymin = ymin, ymax = ymax)) +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  stat_smooth(formula = y ~ x, method = "lm", se = TRUE, level = 0.95) +
  stat_poly_eq(
    aes(
      label = paste(..eq.label.., ..rr.label.., sep = "~~~"),
      group = core_section,
      color = core_section
    ),
    formula = y ~ x,
    parse = TRUE,
    size = 3
  ) +
  labs(
    title = "SoilGrids vs MONet pH",
    x = "MONet (pH)", y = "SoilGrids (pH)"
  ) +
  theme_bw()

ggsave(plot = sg_pH_buffer_plot, file = "./figures/pH_OneToOne_plot.png", width = 6, height = 5)

# Random sample comparison -----------------------------------------------------

# Creates a random sample of the data by source (MONet or SoilGrids) and climate
# region. Uses a maximum sample size of n = 1000 so for each climate region,
# there are 1000 SoilGrids points compared to the MONet points.
process_plot_data <- function(soil_data, n_samples = 1000) {
  # Sample soil data
  plot_data <- soil_data %>%
    group_by(Climate, source) %>%
    slice_sample(n = n_samples) %>% # takes maximum number of samples available up to specified value
    ungroup()

  # Create table of sample sizes for plot label
  sample_sizes <- plot_data %>%
    as.data.frame() %>%
    dplyr::select(Climate, source) %>%
    group_by(Climate, source) %>%
    summarise(n = n()) %>%
    pivot_wider(names_from = "source", values_from = "n") %>%
    filter(MONet > 1) %>%
    mutate(axis_label = paste0(Climate, "\n", "MONet = ", MONet, ", SoilGrids = ", SoilGrids))

  # Join tables
  plot_data_samples <- left_join(plot_data, sample_sizes, by = "Climate")

  return(plot_data_samples)
}

plot_clay_top <- process_plot_data(clay_MONet_sg_top)
plot_clay_btm <- process_plot_data(clay_MONet_sg_btm)

plot_pH_top <- process_plot_data(pH_MONet_sg_top)
plot_pH_btm <- process_plot_data(pH_MONet_sg_btm)

# Creates a set of randomly sampled SoilGrids data to compare to MONet data where
# n equals the number of MONet samples in each climate region.
sg_random_sample <- function(MONet_sg_top, sample_sizes_sg_top, seeds) {
  MONet_data <- MONet_sg_top %>%
    filter(source == "MONet")

  for (seed in seeds) {
    set.seed(seed)
    for (zone in unique(sample_sizes_sg_top$Climate)) {
      n_samples <- sample_sizes_sg_top$MONet[sample_sizes_sg_top$Climate == zone]

      sampled_sg_zone <- MONet_sg_top %>%
        as.data.frame() %>%
        filter(Climate == zone, source != "MONet") %>%
        slice_sample(n = n_samples) %>%
        mutate(source = paste0("SoilGrids_sample_", which(seed == seeds)))

      MONet_data <- bind_rows(MONet_data, sampled_sg_zone)
    }
  }
  return(MONet_data)
}

# Creates figure label for plot with sample number
sample_sizes_pH_top <- pH_MONet_sg_top %>%
  as.data.frame() %>%
  dplyr::select(Climate, source) %>%
  group_by(Climate, source) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = "source", values_from = "n") %>%
  filter(MONet > 1) %>%
  mutate(soilGrids = MONet) %>%
  mutate(axis_label = paste0(Climate, "\n", "MONet = ", MONet, ", SoilGrids = ", soilGrids))

sample_sizes_clay_top <- clay_MONet_sg_top %>%
  as.data.frame() %>%
  dplyr::select(Climate, source) %>%
  group_by(Climate, source) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = "source", values_from = "n") %>%
  filter(MONet > 1) %>%
  mutate(soilGrids = MONet) %>%
  mutate(axis_label = paste0(Climate, "\n", "MONet = ", MONet, ", SoilGrids = ", soilGrids))

pH_samples <- sg_random_sample(pH_MONet_sg_top, sample_sizes_pH_top, seeds = c(1, 5, 10))
clay_samples <- sg_random_sample(clay_MONet_sg_top, sample_sizes_clay_top, seeds = c(1, 5, 10))

## Random sample plots ----
# Plotting color palette
sample_colors <- c("#8080C0", "#D6AA80", "#D98040", "#CC5500")
names(sample_colors) <- unique(pH_samples$source)

pH_random_sample <- pH_samples %>%
  mutate(Experiment = as.factor(if_else(source == "MONet", "MONet_pH", source))) %>%
  left_join(sample_sizes_pH_top, by = c("Climate")) %>%
  filter(!is.na(axis_label)) %>%
  ggplot(aes(
    x = pH, y = axis_label,
    fill = source, color = source
  )) +
  geom_violin(
    alpha = 0.7,
    position = position_dodge(width = 0.8),
    scale = "width"
  ) + # Equal widths regardless of sample size
  geom_boxplot(
    fill = NA,
    position = position_dodge(width = 0.8),
    outlier.shape = NA
  ) +
  theme_bw() +
  labs(
    title = "MONet and SoilGrids pH by Climate Zone",
    x = "pH",
    y = "Climate Zone"
  ) +
  scale_y_discrete(labels = label_wrap_gen(37)) +
  scale_fill_manual(name = "Date Source", values = sample_colors, labels = c("MONet", "SoilGrids_sample_1", "SoilGrids_sample_2", "SoilGrids_sample_3")) +
  scale_colour_manual(name = "Data Source", values = sample_colors, labels = c("MONet", "SoilGrids_sample_1", "SoilGrids_sample_2", "SoilGrids_sample_3")) +
  guides(
    fill = guide_legend(title = "Data Source"), # Combine legends into one titled 'Data Source'
    color = guide_legend(title = "Data Source") # Ensure the same guide maps to both
  ) +
  theme(plot.title = element_text(size = 16))

names(sample_colors) <- unique(clay_samples$source)

clay_random_sample <- clay_samples %>%
  mutate(Experiment = as.factor(if_else(source == "MONet", "MONet_clay", source))) %>%
  left_join(sample_sizes_clay_top, by = c("Climate")) %>%
  filter(!is.na(axis_label)) %>%
  ggplot(aes(
    x = Clay_percent, y = axis_label,
    fill = source, color = source
  )) +
  geom_violin(
    alpha = 0.7,
    position = position_dodge(width = 0.8),
    scale = "width"
  ) + # Equal widths regardless of sample size
  geom_boxplot(
    fill = NA,
    position = position_dodge(width = 0.8),
    outlier.shape = NA
  ) +
  theme_bw() +
  labs(
    title = "MONet and SoilGrids Clay by Climate Zone",
    x = "% Clay",
    y = "Climate Zone"
  ) +
  scale_y_discrete(labels = label_wrap_gen(37)) +
  scale_fill_manual(name = "Date Source", values = sample_colors) +
  scale_color_manual(name = "Data Source", values = sample_colors) +
  guides(
    fill = guide_legend(title = "Data Source"), # Combine legends into one titled 'Data Source'
    color = guide_legend(title = "Data Source") # Ensure the same guide maps to both
  ) +
  theme(plot.title = element_text(size = 16))

ggsave(plot = pH_random_sample, "./figures/pH_random_sample.png", width = 9, height = 8)
ggsave(plot = clay_random_sample, "./figures/clay_random_sample.png", width = 9, height = 8)

# MONet points missing from SoilGrids ------------------------------------------

# Table of values where MONet is missing SoilGrids counterpart
monet_clay_missing <- rbind(
  sg_clay_monet_top_values %>%
    filter(is.na(crop_roi_igh_clay_0.5cm)) %>%
    dplyr::select(Clay_percent, source, core_section, sample),
  sg_clay_monet_btm_values %>%
    filter(is.na(crop_roi_igh_clay_15.30cm)) %>%
    dplyr::select(Clay_percent, source, core_section, sample)
) %>%
  mutate(missing = TRUE)

missing_clay <- clay_loc_sf_top %>%
  left_join(monet_clay_missing, by = c("core_section", "Clay_percent", "source", "sample")) %>%
  filter(missing == TRUE) %>%
  separate(sample, into = c("prj", "Site_Code")) %>%
  left_join(data.frame(monet_rs_coords), by = c("Site_Code")) %>%
  mutate(location = substr(Site_Code, 1, 2)) %>%
  group_by(location) %>%
  mutate(samples = paste0("n = ", n())) %>%
  ungroup()


crop_extent <- as.polygons(terra::ext(-78, -74.5, 37.8, 44.5)) # creates extent in EPSG:4326
crs(crop_extent) <- "EPSG:4326" # add CRS

crop_extent <- crop_extent %>%
  project("EPSG:5070") # projects to working CRS

sg_clay_top_prj_final <- terra::crop(sg_clay_conus_top_scaled, crop_extent) # Crop raster to extent

MONet_missing_from_sg <- ggplot() +
  tidyterra::geom_spatraster(data = sg_clay_top_prj_final) +
  geom_sf(data = conus_valid, color = "black", alpha = 0.5) +
  geom_sf(data = missing_clay, color = "black", size = 3.5) +
  geom_sf(data = missing_clay, aes(color = Site_Code), size = 3) +
  geom_sf_label(data = missing_clay, aes(label = samples), nudge_y = 15000, nudge_x = -20000) +
  coord_sf(xlim = c(1503569, 1847901), ylim = c(1838176, 2146496)) +
  theme_bw() +
  scale_fill_gradient(low = "white", high = "blue", na.value = NA) +
  labs(
    title = "MONet data points without SoilGrids data"
  ) +
  theme(
    plot.title = element_text(size = 16),
    axis.title.x = element_blank(), # Remove x-axis title
    axis.title.y = element_blank()
  )

ggsave(plot = MONet_missing_from_sg, "./figures/MONet_missing_from_sg.png", width = 6, height = 5.3)

# Spatial Comparison of Clay Percent -------------------------------------------

MONet_sg_clay_map <- ggplot() +
  tidyterra::geom_spatraster(data = sg_clay_conus_top_scaled) +
  geom_sf(data = conus_valid, color = "black", alpha = 0.3) +
  geom_sf(data = clay_loc_sf_top, aes(fill = Clay_percent), colour = "black", size = 3) +
  geom_sf(data = clay_loc_sf_top, aes(color = Clay_percent), size = 2) +
  theme_bw() +
  scale_fill_gradient(low = "white", high = "blue", na.value = NA, limits = c(0, 85)) +
  scale_color_gradient(low = "white", high = "blue", na.value = NA, limits = c(0, 85), guide = "none") +
  labs(
    title = "Spatial Comparison of MONet Clay Content to SoilGrids (Top sample)",
    fill = "% Clay"
  ) +
  theme(
    plot.title = element_text(size = 16),
    margin(0, 0, 0, 0, unit = "mm")
  )

ggsave(plot = MONet_sg_clay_map, "./figures/MONet_sg_clay_map.png", width = 8, height = 5.3)

# Clay comparison GCAM regions -------------------------------------------------

# sets up zenodo download for GCAM basin shapefile
doi <- "10.5281/zenodo.4688451"
local_path <- "./data/shapefiles/"
inborutils::download_zenodo(doi, local_path, quiet = TRUE)
list.files(local_path)
destfile <- "gcam_boundaries_moirai_3p1_0p5arcmin_wgs84.zip"
extraction_directory <- ""

if (file.exists(paste0(local_path, destfile))) {
  # Try unzipping the file and catch any errors
  tryCatch(
    {
      unzip(paste0(local_path, destfile), exdir = paste0(local_path, extraction_directory))
      cat("Download and extraction of GCAM basin data complete!\n")
    },
    error = function(e) {
      cat("Error in extraction:", e$message, "\n")
    }
  )
} else {
  cat("Failed to download the data.\n")
}

global_basins <- read_sf(paste0(local_path, "/gcam_boundaries_moirai_3p1_0p5arcmin_wgs84/main_outputs/glu_boundaries_moirai_landcells_3p1_0p5arcmin.shp"))

# transforms to project CRS
global_basins_prj <- global_basins %>%
  st_transform(crs(conus_valid))

conus_basins <- st_crop(global_basins_prj, conus_valid)

clay_basins <- st_intersection(clay_loc_sf_top, conus_basins)

clay_basin_summary <- clay_basins %>%
  data.frame() %>%
  group_by(core_section, glu_id, glu_nm) %>%
  summarize(
    Clay_percent_mean = mean(Clay_percent), monet_sd = sd(Clay_percent),
    monet_min = min(Clay_percent), monet_max = max(Clay_percent), monet_n = n()
  )

# Clay content values from GCAM
GCAM_clay_content <- read.csv("data/gcam/mapped_clay_KN.csv")

# Calculates summary statistics by region
GCAM_clay_usa <- GCAM_clay_content %>%
  filter(iso == "usa", c_type == "clay content (0-30 cms)") %>%
  group_by(iso, glu_code, GCAM_GLU_name, c_type) %>%
  summarize(gcam_average = mean(weighted_average), gcam_median_value = median(median_value), gcam_min_value = min(min_value), gcam_max_value = max(max_value), gcam_q1_value = mean(q1_value), gcam_q3_value = mean(q3_value))

MONet_GCAM_clay <- clay_basin_summary %>%
  left_join(GCAM_clay_usa, by = c("glu_id" = "glu_code"))

# Joins clay data to basin shapefile
GCAM_clay_map <- left_join(conus_basins, GCAM_clay_usa, by = c("glu_id" = "glu_code"))

## MONet GCAM plot ----
MONet_GCAM_plot <- ggplot() +
  geom_sf(data = GCAM_clay_map, aes(fill = gcam_average), color = "black") +
  geom_sf(data = clay_loc_sf_top, aes(fill = Clay_percent), colour = "black", size = 3) +
  geom_sf(data = clay_loc_sf_top, aes(color = Clay_percent), size = 2) +
  theme_bw() +
  scale_fill_gradient(low = "white", high = "blue", na.value = NA, limits = c(0, 85)) +
  scale_color_gradient(low = "white", high = "blue", na.value = NA, limits = c(0, 85), guide = "none") +
  labs(
    title = "Spatial Comparison of MONet to GCAM Clay Content (Top sample)",
    fill = "% Clay"
  ) +
  theme(plot.title = element_text(size = 18))

ggsave(plot = MONet_GCAM_plot, "./figures/MONet_GCAM_plot.png", width = 8, height = 5.3)

# Write outputs ----------------------------------------------------------------

# clay
save(
  sg_clay_monet_top_values, sg_clay_monet_btm_values,
  clay_MONet_sg_top,
  plot_clay_top, plot_clay_btm,
  MONet_GCAM_clay, clay_loc_sf_top,
  file = "R_data/processed_clay.RData"
)

# pH
save(
  pH_MONet_sg_top,
  plot_pH_top, plot_pH_btm,
  sg_pH_monet_top_values, sg_pH_monet_btm_values,
  file = "R_data/processed_pH.RData"
)

# Rs
save(
  srdb, SRDB_coords, monet_rs, monet_rs_coords,
  plot_pH_top, soil_Rh_buffer,
  file = "R_data/processed_Rs.RData"
)

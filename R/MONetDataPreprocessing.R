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



# 1. Download SRDB from github ----------------------------------------------------

# Define the URL of the specific release asset
# You can find the asset URL by navigating to the release page on GitHub and copying the download link for the file you want
release_url <- "https://github.com/bpbond/srdb/archive/refs/tags/v20250503a.zip"

# Define the destination file path for the downloaded zip
dir <- print(getwd())
destfile <- "/data/srdb-20250503a"

# Use download.file() to download the file
download.file(url = release_url, destfile = paste0(dir, destfile), mode = "wb")

extraction_directory <- "/data/srdb"
dir.create(file.path(dir, extraction_directory))

# Check if the file was downloaded successfully
if (file.exists(paste0(dir,destfile))) {
  # Try unzipping the file and catch any errors
  tryCatch({
    unzip(paste0(dir,destfile), exdir = paste0(dir,extraction_directory))
    cat("Download and extraction of release data complete!\n")
  }, error = function(e) {
    cat("Error in extraction:", e$message, "\n")
  })
} else {
  cat("Failed to download the release file.\n")
}

# Remove the downloaded zip file
file.remove(paste0(dir, destfile))


# 2. Download SoilGrids clay content and pH data for CONUS -----------------------

# Define geographic coordinates for the bounding box
bbox_geo <- matrix(c(-125, 24,  # Bottom-left corner: xmin, ymin
                     -66, 49), # Top-right corner: xmax, ymax
                   nrow = 2, byrow = TRUE)

# Create a spatial object with these coordinates
bbox_geo_sf <- st_sfc(st_polygon(list(rbind(
  c(bbox_geo[1,1], bbox_geo[1,2]),  # Bottom-left
  c(bbox_geo[2,1], bbox_geo[1,2]),  # Bottom-right
  c(bbox_geo[2,1], bbox_geo[2,2]),  # Top-right
  c(bbox_geo[1,1], bbox_geo[2,2]),  # Top-left
  c(bbox_geo[1,1], bbox_geo[1,2])   # Closing point to form the polygon
))), crs = 4326)

# Goode Homolosine (igh) projection
proj_string <- "+proj=igh +datum=WGS84 +no_defs"

# Transform geographic coordinates to IGH
bbox_igh_sf <- st_transform(bbox_geo_sf, crs = proj_string)

# Get the bounding box of our ROI
# now in the appropriate igh projection
bbox <- st_bbox(bbox_igh_sf)

# Format for gdalUtilities
ulx = bbox$xmin
uly = bbox$ymax
lrx= bbox$xmax
lry = bbox$ymin
(bb <- c(ulx, uly, lrx, lry))

# Soil Grids url
sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"

# Define coordinate reference system
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'

# gdal_translate goes to SG's webpage and
# downloads the .vrt data in a .tif in your workspace
# this is retrieving the mean variable of interest for surface soil
# many other data types are available

# pH 0-5cm
gdal_translate(paste0(sg_url,'phh2o/phh2o_0-5cm_mean.vrt'), # ph * 10
               "./data/soilgrids/crop_roi_igh_ph_0-5cm.tif",
               tr=c(250,250),
               projwin=bb,
               projwin_srs =igh)

# pH 15-30cm
gdal_translate(paste0(sg_url,'phh2o/phh2o_15-30cm_mean.vrt'), # ph * 10
               "./data/soilgrids/crop_roi_igh_ph_15-30cm.tif",
               tr=c(250,250),
               projwin=bb,
               projwin_srs =igh)

# clay 0-5cm
gdal_translate(paste0(sg_url,'clay/clay_0-5cm_mean.vrt'), #g/kg
               "./data/soilgrids/crop_roi_igh_clay_0-5cm.tif",
               tr=c(250,250),
               projwin=bb,
               projwin_srs =igh)

# clay 15-30cm
gdal_translate(paste0(sg_url,'clay/clay_15-30cm_mean.vrt'), #g/kg
               "./data/soilgrids/crop_roi_igh_clay_15-30cm.tif",
               tr=c(250,250),
               projwin=bb,
               projwin_srs =igh)


# 3. Read in MONet respiration and SRDB data -----------------------------------

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
  st_transform("WGS84")

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
  {.[rowSums(st_within(., conus_valid, sparse = FALSE)) > 0, ]}

# Load the shapefile as an `sf` object
climate_zones_sf <- st_read("data/shapefiles/na_climatezones_shapefile/climatezones_shapefile/NA_ClimateZones/data/North_America_Climate_Zones.shp")%>%
  st_transform(crs(conus_valid))

# Create a mapping table for the 3 letter climate zone code to the full name
climate_zone_mapping <- climate_zones_sf%>%
  distinct(Code, Climate)

# Find intersection of climate zones and CONUS
climate_zones_conus_sf <- climate_zones_sf%>%
  st_intersection(st_union(conus_valid))

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
monet_pH_loc <- monet_pH_data%>%
  # Separate the Sample_Name column into distict columns
  separate(Sample_Name, into = c("proposal_id", "sample", "core_section"))%>%
  mutate(proposal_id = as.numeric(proposal_id),
         sample = as.numeric(sample))%>%
  # Join to the coordinated by proposal ID and sample set
  left_join(monet_pH_coord, by = c("proposal_id", "sample" = "sampling_set"))%>%
  # Resolves longitude mislabeling
  mutate(longitude = if_else(longitude > 0, longitude * -1, longitude),
         sample = as.character(sample))%>%
  dplyr::select(proposal_id, sample, core_section, pH, latitude, longitude)%>%
  mutate(source = "MONet")

# Join MONet clay content sample data and location
monet_clay_loc <- monet_clay_data%>%
  # Separate the Sample_Name column into distict columns
  separate(Sample_Name, into = c("proposal_id", "sample", "core_section"))%>%
  mutate(proposal_id = as.numeric(proposal_id),
         sample = as.numeric(sample))%>%
  left_join(monet_clay_coord, by = c("proposal_id", "sample" = "sampling_set"))%>%
  # Resolves longitude mislabeling
  mutate(longitude = if_else(longitude > 0, longitude * -1, longitude),
         sample = as.character(sample))%>%
  dplyr::select(proposal_id, sample, core_section, Clay_percent, latitude, longitude)%>%
  mutate(source = "MONet")

# Join 1000 soil sample data and location
processed_1000s_loc <- monet_rs%>%
  separate(Sampling_Set, into = c("prj", "Site_Code"))%>%
  left_join(rs_coord, by = c("Site_Code"))%>%
  filter(!is.na(Lat))%>%
  mutate(source = "1000s", sample = paste0(prj, "_", Site_Code))%>%
  # format to MONet structure
  dplyr::select(proposal_id = Proposal_ID, sample,
                core_section = Core_Section, pH, Clay_percent,
                latitude = Lat, longitude = Long, source)

# Bind MONet data with 1000s clay and pH data
monet_pH <- bind_rows(monet_pH_loc, dplyr::select(processed_1000s_loc, -Clay_percent))
monet_clay <- bind_rows(monet_clay_loc, dplyr::select(processed_1000s_loc, -pH))

## Convert MONet data points to `sf` objects and extract Koppen climate zone information ----

# Convert MONet pH data into shapefile object setting CRS to the target WGS1984, clip points within CONUS
pH_loc_sf <- st_as_sf(monet_pH, coords = c("longitude", "latitude"), crs = target_crs)%>%
  st_intersection(st_union(conus_valid))%>%
  filter(!is.na(pH))

# Filter for top and bottom samples
pH_loc_sf_top <- pH_loc_sf%>%filter(core_section == "TOP")
pH_loc_sf_btm <- pH_loc_sf%>%filter(core_section == "BTM")

# Convert MONet clay content data into shapefile object setting CRS to the target WGS1984, clip points within CONUS
clay_loc_sf <- st_as_sf(monet_clay, coords = c("longitude", "latitude"), crs = target_crs)%>%
  st_intersection(st_union(conus_valid))%>%
  filter(!is.na(Clay_percent))

# Filter for top and bottome samples
clay_loc_sf_top <- clay_loc_sf%>%filter(core_section == "TOP")
clay_loc_sf_btm <- clay_loc_sf%>%filter(core_section == "BTM")

# Join point data to Koppen shapefile to extract climate zone information for each point
pH_zones <- st_join(pH_loc_sf, climate_zones_conus_sf)%>%
  dplyr::select(proposal_id, sample, core_section, pH, source, Code, Climate, Key_EN, geometry)%>%
  filter(!is.na(Climate))
clay_zones <- st_join(clay_loc_sf, climate_zones_conus_sf)%>%
  dplyr::select(proposal_id, sample, core_section, Clay_percent, source, Code, Climate, Key_EN, geometry)%>%
  filter(!is.na(Climate))

# Read in soilGrids data and clip to CONUS -------------------------------------

# This chunk accomplishes two functions. First it extracts the soilgrids data at
# the location of each MONet sample site. The second function of this chunk is
# to extract the soilgrids data by their appropriate climate zone.

# if soilgrid data has already been processed and saved to .RData, load in RData
read_project_mask <- function(raster_path, conus_valid){
  raster <- rast(raster_path)

  prj_raster <- raster%>%
    project(crs(conus_valid))

  prj_raster_mask <- raster::mask(prj_raster, conus_valid)

  return(prj_raster_mask)
}

extract_buffer <- function(raster, point_data, buffer_size_m){
  sf_use_s2(TRUE)

  buffer_points <- st_buffer(sf_transform_xy(point_data,
                                             source_crs = point_data,
                                             target_crs = point_data), units::set_units(1000, meters))

  sf_use_s2(FALSE)

  extract_buffer_region <- raster::extract(raster, buffer_points,
                                           fun = function(x) list(mean = mean(x/10, na.rm = TRUE),
                                                                  count = length(x[!is.na(x)]),
                                                                  sd = sd(x/10, na.rm = TRUE)),
                                           bind = TRUE)%>%data.frame()

  names(extract_buffer_region)[(ncol(extract_buffer_region)-2):ncol(extract_buffer_region)] <- c("sg_mean", "sg_count", "sg_sd")

  extract_buffer_region[c("sg_mean", "sg_count", "sg_sd")] <- lapply(extract_buffer_region[c("sg_mean", "sg_count", "sg_sd")], as.numeric)

  return(extract_buffer_region)
}

if(!"sg_data.RData" %in% list.files()){

  # Read in an projects soil grids clay and pH rasters to WGS 1984 (time intensive) and mask to CONUS
  sg_clay_conus_top <- read_project_mask("./data/soilgrids/crop_roi_igh_clay_0-5cm.tif", conus_valid)
  sg_clay_conus_btm <- read_project_mask("./data/soilgrids/crop_roi_igh_clay_15-30cm.tif", conus_valid)

  sg_pH_conus_top <- read_project_mask("./data/soilgrids/crop_roi_igh_ph_0-5cm.tif", conus_valid)
  sg_pH_conus_btm <- read_project_mask("./data/soilgrids/crop_roi_igh_ph_15-30cm.tif", conus_valid)

  # Creates buffer region around points and extracts sg raster values from within
  sg_clay_buffer_top <- extract_buffer(sg_clay_conus_top, clay_loc_sf_top, buffer_size_m = 1000)
  sg_clay_buffer_btm <- extract_buffer(sg_clay_conus_btm, clay_loc_sf_btm, buffer_size_m = 1000)

  sg_pH_buffer_top <- extract_buffer(sg_pH_conus_top, pH_loc_sf_top, buffer_size_m = 1000)
  sg_pH_buffer_btm <- extract_buffer(sg_pH_conus_btm, pH_loc_sf_btm, buffer_size_m = 1000)

  # Binds top and bottom samples
  sg_clay_buffer <- rbind(sg_clay_buffer_top, sg_clay_buffer_btm)
  sg_pH_buffer <- rbind(sg_pH_buffer_top, sg_pH_buffer_btm)

  # Extracts soil grids clay and pH data from the MONet site locations
  # Values are divided by 10

  # Clay
  sg_clay_monet_top <- terra::extract(sg_clay_conus_top, clay_loc_sf_top, fun=mean, bind=TRUE)
  sg_clay_monet_top_values <- values(sg_clay_monet_top)%>%
    mutate(crop_roi_igh_clay_0.5cm = crop_roi_igh_clay_0.5cm/10)

  sg_clay_monet_btm <- terra::extract(sg_clay_conus_btm, clay_loc_sf_btm, fun=mean, bind=TRUE)
  sg_clay_monet_btm_values <- values(sg_clay_monet_btm)%>%
    mutate(crop_roi_igh_clay_15.30cm = crop_roi_igh_clay_15.30cm/10)

  # pH
  sg_pH_monet_top <- terra::extract(sg_pH_conus_top, pH_loc_sf_top, fun=mean, na.rm=TRUE, bind=TRUE)
  sg_pH_monet_top_values <- values(sg_pH_monet_top)%>%
    mutate(crop_roi_igh_ph_0.5cm = crop_roi_igh_ph_0.5cm/10)

  sg_pH_monet_btm <- terra::extract(sg_pH_conus_btm, pH_loc_sf_btm, fun=mean, na.rm=TRUE, bind=TRUE)
  sg_pH_monet_btm_values <- values(sg_pH_monet_btm)%>%
    mutate(crop_roi_igh_ph_15.30cm = crop_roi_igh_ph_15.30cm/10)

  soilgrids_by_zone <- function(soilgrids, climate_zones_conus_sf){

    # Convert the sf object (climate zones) to a terra SpatVector
    climate_zones_vect <- vect(climate_zones_conus_sf)

    # Mask clay content raster by each climate zone
    var_by_zone <- list()  # Create a list to store masked rasters for each zone

    climate_zone_names <- unique(climate_zones_conus_sf$Code)[1:21]

    for (zone_name in climate_zone_names) {
      # Filter the specific climate zone
      single_zone <- climate_zones_vect[climate_zones_conus_sf$Code == zone_name, ]
      # Mask clay content to just this zone
      var_zone <- mask(crop(soilgrids, single_zone), single_zone)
      # Save the clay raster for this zone
      var_by_zone[[zone_name]] <- var_zone
    }

    # Create a list to store distinct clay content values for each zone
    values_by_zone <- list()

    for (zone_name in climate_zone_names) {
      # Get the clay content raster for this zone
      var_zone <- var_by_zone[[zone_name]]
      # Extract cell values as a vector
      var_values <- unique(values(var_zone, na.rm = TRUE))  # Remove NA values
      values_by_zone[[zone_name]] <- var_values         # Store in list
    }

    #TODO very slow ~few minutes
    var_zone_df <- data.frame(
      zone = names(values_by_zone),
      value = sapply(values_by_zone, function(x) paste(x, collapse = ", "))
    )

    var_long <- do.call(rbind, lapply(names(values_by_zone), function(zone_name) {
      data.frame(zone = zone_name, value = values_by_zone[[zone_name]])
    }))

    return(var_long)
  }

  # 11 min each
  clay_top_long <- soilgrids_by_zone(sg_clay_conus_top, climate_zones_conus_sf)
  clay_btm_long <- soilgrids_by_zone(sg_clay_conus_btm, climate_zones_conus_sf)

  pH_top_long <- soilgrids_by_zone(sg_pH_conus_top, climate_zones_conus_sf)
  pH_btm_long <- soilgrids_by_zone(sg_pH_conus_btm, climate_zones_conus_sf)

  save(sg_clay_conus_top, sg_clay_conus_btm, # for spatial comparison
       sg_clay_buffer, sg_pH_buffer,
       clay_top_long, clay_btm_long, # for entire SG comparison
       pH_top_long, pH_btm_long,
       sg_clay_monet_top_values, sg_clay_monet_btm_values, # 1 to 1 comparison
       sg_pH_monet_top_values,sg_pH_monet_btm_values,
       file = "sg_data.RData")
} else {

  load("sg_data.RData")

}

# Read in soil respiration raster, clip to conus, and extract buffered regions around MONet points
soil_Rh_conus <- read_project_mask("./data/SoilResp_HeterotrophicResp_1928/data/soil_Rh_mean.tif", conus_valid)

monet_rs_combined <- monet_rs%>%
  mutate(
    Site_Code = sapply(strsplit(Sample_Name, "_"), `[`, 3),
    Core_Section = sapply(strsplit(Sample_Name, "_"), `[`, 4)
  )%>%
  left_join(rs_coord, by = c("Site_Code"))%>%
  select(Sample_Name, respiration_ppm_co2_c, respiration_mg_co2_c_g_soil_day_24hour,
         respiration_mg_co2_c_g_soil_day_96hour,
         Site_Code, Core_Section, Lat, Long)%>%
  filter(Site_Code != "PUUM", !is.na(Long), !is.na(Lat)) %>% #Take out one
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  st_transform(target_crs)

soil_Rh_buffer <-  extract_buffer(soil_Rh_conus, monet_rs_combined, 1000)

# Output is broken up into two smaller Rdata files for easier loading
# Save tables for synthesis
save(pH_zones, clay_zones,
     clay_loc_sf_top, clay_loc_sf_btm,
     climate_zone_mapping, soil_Rh_buffer,
     file = "processed_data.Rdata")



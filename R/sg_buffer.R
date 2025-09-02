library(dplyr)
library(tidyr)
library(sf)
library(terra)
library(ggplot2)

# Copied from MONetDataPreprocessing.R ---------
myColors <- c( "#000080","#4040A0","#8080C0", "#D6AA80", "#D98040", "#CC5500")
names(myColors) <- c("MONet_resp", "MONet_pH", "MONet_clay", "comp_clay", "comp_pH", "comp_resp")

# read in site coordinates and data
# site coordinates and data
monet_clay_coord <- read.csv("data/MONet/clay/processed_data/Coordinates.csv")
monet_clay_data <- read.csv("data/MONet/clay/processed_data/Soil_BioChemical_properties.csv")

monet_pH_coord <- read.csv("data/MONet/pH/processed_data/Coordinates.csv")
monet_pH_data <- read.csv("data/MONet/pH/processed_data/Soil_BioChemical_properties.csv")

# 1000soil coordinates and data
monet_rs <- read.csv("data/MONet/1000S_processed_L2_summary.csv")
metadata_1000s <- read.csv("data/MONet/1000Soils_Metadata_Site_Mastersheet_v1.csv")

# Select soil respiration from MONet data
monet_rs %>%
  dplyr::select(Sample_Name, respiration_ppm_co2_c,
                respiration_mg_co2_c_g_soil_day_24hour,
                respiration_mg_co2_c_g_soil_day_96hour) -> monet_slim

# Function to separate and add metadata from Sample_Name
extract_metadata <- function(monet_slim) {
  monet_slim %>%
    mutate(
      Site_Code = sapply(strsplit(Sample_Name, "_"), `[`, 3),
      Core_Section = sapply(strsplit(Sample_Name, "_"), `[`, 4)
    )
}

# Extract metadata
monet_rs_metadata <- extract_metadata(monet_slim)

# Integrate this with the annual climate data (MAT, MAP) using Site_Code as key
combined_monet <- monet_slim%>%
  mutate(
    Site_Code = sapply(strsplit(Sample_Name, "_"), `[`, 3),
    Core_Section = sapply(strsplit(Sample_Name, "_"), `[`, 4)
  )%>%
  left_join(metadata_1000s, by = c("Site_Code"))%>%
  select(Sample_Name, respiration_ppm_co2_c, respiration_mg_co2_c_g_soil_day_24hour, respiration_mg_co2_c_g_soil_day_96hour,
         Site_Code, Core_Section, Lat, Long)

names(combined_monet)

# Convert Rs from Rs annual to mgC/gSoil/day
bulk_density_g_cm2 <- 1.85
effective_soil_depth_cm <- 10
C_molec_weight_ratio_gCo2_C <- 3.67

conversion_factor = 1000 * (C_molec_weight_ratio_gCo2_C/(365 * bulk_density_g_cm2 * 10^5))

combined_data_mg_co2_c_g_soil_day <- combined_data%>%
  mutate(Rs_mg_co2_g_24h = Rs_annual * conversion_factor)
## Join pH and clay site locations to data and combine MONet and 1000 soils-----


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

# Join 1000 soil sample data and location
processed_1000s_loc <- processed_1000s%>%
  separate(Sampling_Set, into = c("prj", "Site_Code"))%>%
  left_join(metadata_1000s, by = c("Site_Code"))%>%
  filter(!is.na(Lat))%>%
  mutate(source = "1000s", sample = paste0(prj, "_", Site_Code))%>%
  # format to MONet structure
  dplyr::select(proposal_id = Proposal_ID, sample,
                core_section = Core_Section, pH, Clay_percent,
                latitude = Lat, longitude = Long, source)

# Bind MONet data with 1000s clay and pH data
monet_clay <- bind_rows(monet_clay_loc, dplyr::select(processed_1000s_loc, -pH))

## Convert MONet data points to `sf` objects and extract Koppen climate zone information ----
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
# Convert MONet clay content data into shapefile object setting CRS to the target WGS1984, clip points within CONUS
clay_loc_sf <- st_as_sf(monet_clay, coords = c("longitude", "latitude"), crs = target_crs)%>%
  st_intersection(st_union(conus_valid))%>%
  filter(!is.na(Clay_percent))

clay_loc_sf_top <- clay_loc_sf%>%filter(core_section == "TOP")
clay_loc_sf_btm <- clay_loc_sf%>%filter(core_section == "BTM")

monet_pH <- bind_rows(monet_pH_loc, dplyr::select(processed_1000s_loc, -Clay_percent))

pH_loc_sf <- st_as_sf(monet_pH, coords = c("longitude", "latitude"), crs = target_crs)%>%
  st_intersection(st_union(conus_valid))%>%
  filter(!is.na(pH))

# Filter for top and bottom samples
pH_loc_sf_top <- pH_loc_sf%>%filter(core_section == "TOP")
pH_loc_sf_btm <- pH_loc_sf%>%filter(core_section == "BTM")

# New processing ------------------------------------------------------------

# functions and plotting -----------------
read_project_mask <- function(raster_path, conus_valid){
  raster <- rast(raster_path)

  prj_raster <- raster%>%
    project(crs(conus_valid))

  prj_raster_mask <- raster::mask(prj_raster, conus_valid)

  return(prj_raster_mask)
}
#+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs"
# buffer --------------------------------------------------------
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

# Clay ------------
#res(rast("./data/soilgrids/crop_roi_igh_clay_0-5cm.tif"))
sg_clay_conus_top <- read_project_mask("./data/soilgrids/crop_roi_igh_clay_0-5cm.tif", conus_valid)
sg_clay_conus_btm <- read_project_mask("./data/soilgrids/crop_roi_igh_clay_15-30cm.tif", conus_valid)

sg_clay_buffer_top <- extract_buffer(sg_clay_conus_top, clay_loc_sf_top, buffer_size_m = 1000)
sg_clay_buffer_btm <- extract_buffer(sg_clay_conus_btm, clay_loc_sf_btm, buffer_size_m = 1000)

sg_clay_buffer <- rbind(sg_clay_buffer_top, sg_clay_buffer_btm)


sg_clay_buffer%>%
  mutate(ymin = sg_mean - sg_sd,
         ymax = sg_mean + sg_sd)%>%
  ggplot(aes(x = Clay_percent, y = sg_mean, color = core_section, fill = core_section))+
  geom_point()+
  #geom_path()
  geom_errorbar(aes(x = Clay_percent, ymin = ymin, ymax = ymax))+
  geom_abline(intercept = 0, slope = 1, color = "black")+
  stat_smooth(formula = y~x,method = "lm", se = TRUE, level = 0.95)+
  labs(title = "Clay Percent Soilgrids vs MONet",
       x = "MONet", y = "Soilgrids")+
  theme_bw()

model_clay <- lm(sg_mean ~ Clay_percent, sg_clay_buffer)
summary(model_clay)

# y = 10.79 + 0.53x

# pH -----------

sg_pH_conus_top <- read_project_mask("./data/soilgrids/crop_roi_igh_pH_0-5cm.tif", conus_valid)
sg_pH_conus_btm <- read_project_mask("./data/soilgrids/crop_roi_igh_pH_15-30cm.tif", conus_valid)

sg_pH_buffer_top <- extract_buffer(sg_pH_conus_top, pH_loc_sf_top, buffer_size_m = 1000)
sg_pH_buffer_btm <- extract_buffer(sg_pH_conus_btm, pH_loc_sf_btm, buffer_size_m = 1000)

sg_pH_buffer <- rbind(sg_pH_buffer_top, sg_pH_buffer_btm)%>%
  mutate(sg_mean = as.numeric(sg_mean))%>%
  filter(!is.nan(sg_mean))

cor(sg_pH_buffer$pH, as.numeric(sg_pH_buffer$sg_mean))

sg_pH_buffer%>%
  mutate(ymin = sg_mean - sg_sd,
         ymax = sg_mean + sg_sd)%>%
  ggplot(aes(x = pH, y = sg_mean, color = core_section, fill = core_section))+
  geom_point()+
  geom_errorbar(aes(x = pH, ymin = ymin, ymax = ymax))+
  geom_abline(intercept = 0, slope = 1, color = "black")+
  stat_smooth(formula = y~x,method = "lm", se = TRUE, level = 0.95)+
  labs(title = "pH Soilgrids vs MONet",
       x = "MONet", y = "Soilgrids")+
  theme_bw()

model_pH <- lm(sg_mean ~ pH, sg_pH_buffer)
summary(model_pH)

# y = 2.62 + 0.54x

# soil Rh -------------

# Taken from MONetSynthesis ----------------------------
# Convert Rs from Rs annual to mgC/gSoil/day
bulk_density_g_cm2 <- 1.85
effective_soil_depth_cm <- 10
C_molec_weight_ratio_gCo2_C <- 3.67

conversion_factor = 1000 * (C_molec_weight_ratio_gCo2_C/(365 * bulk_density_g_cm2 * 10^5))

monet_rs_coords <- combined_monet %>%
  filter(Site_Code != "PUUM", !is.na(Lat)) %>% #Take out one
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  st_transform(target_crs)

# Soil Rh -------------------------

soil_Rh_conus <- read_project_mask("./data/SoilResp_HeterotrophicResp_1928/data/soil_Rh_mean.tif", conus_valid)
soil_Rh_buffer <-  extract_buffer(soil_Rh_conus, monet_rs_coords, 1000)

soil_Rh_buffer%>%
  mutate(ymin = sg_mean - sg_sd,
         ymax = sg_mean + sg_sd)%>%
  ggplot(aes(x = respiration_ppm_co2_c , y = sg_mean * conversion_factor , color = Core_Section, fill = Core_Section))+
  geom_point()+
  geom_errorbar(aes(x = respiration_ppm_co2_c , ymin = ymin * conversion_factor, ymax = ymax * conversion_factor))+
  geom_abline(intercept = 0, slope = 1, color = "black")+
  stat_smooth(formula = y~x,method = "lm", se = TRUE, level = 0.95)+
  labs(title = "Rh Upscaled SRDB vs MONet",
       x = "MONet", y = "Upscaled SRDB")+
  theme_bw()




library(dplyr)
library(ggplot2)
library(sf)

# read in site coordinates
monet_rs <- read.csv("data/MONet/1000S_processed_L2_summary.csv")
rs_coord <- read.csv("data/MONet/1000Soils_Metadata_Site_Mastersheet_v1.csv")
srdb <- read.csv("data/srdb/srdb-20250503a/srdb-data.csv")

# Read and ensure valid shapes for CONUS
shape_valid <- st_read("data/shapefiles/s_18mr25/s_18mr25.shp") %>%
  st_make_valid() %>%
  filter(STATE %in% state_abbreviations_conus)

# Define the target CRS from the shapefile
target_crs <- st_crs(shape_valid)

# Disable S2 processing to use GEOS for spatial operations
sf_use_s2(FALSE)

# Filter SRDB coordinates within shapefile boundaries
SRDB_coords <- srdb %>%
  select(Record_number, Latitude, Longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(target_crs) %>%
  {.[rowSums(st_within(., shape_valid, sparse = FALSE)) > 0, ]}

# Convert rs_coord to sf assuming the column names are 'Long' and 'Lat'
monet_rs_coords <- rs_coord %>%
  filter(Site_Code != "PUUM") %>% #Take out one
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  st_transform(target_crs)

# Plotting both sets of coordinates
ggplot() +
  geom_sf(data = shape_valid, fill = NA, color = "gray", linetype = "solid") +
  geom_sf(data = SRDB_coords, aes(color = 'SRDB Source'), size = 2, alpha = 0.7) +
  geom_sf(data = monet_rs_coords, aes(color = 'Monet RS Source'), size = 2, alpha = 0.7) +
  scale_color_manual(values = c('SRDB Source' = 'blue', 'Monet RS Source' = 'red')) +
  theme_minimal() +
  labs(title = "Map of Coordinates from Two Sources",
       color = "Data Source") +
  theme(legend.position = "bottom")


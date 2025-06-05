
#Get clay and pH data from SoilGrids
#KAM June 5, 2025

# load packages
library(sf)
library(terra)
library(gdalUtilities)

# Define geographic coordinates for the bounding box
# This is a coarse box for CONUS, approximated by PNNL AI incubator
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

# black magic for the Interrupted Goode Homolosine (igh) projection
proj_string <- "+proj=igh +datum=WGS84 +no_defs"

# Transform geographic coordinates to IGH
bbox_igh_sf <- st_transform(bbox_geo_sf, crs = proj_string)

# Get the bounding box of our ROI
# now in the appropriate igh projection
bbox <- st_bbox(bbox_igh_sf)

# and then shift things around so that
# gdalUtilities can read it in
ulx = bbox$xmin
uly = bbox$ymax
lrx= bbox$xmax
lry = bbox$ymin
(bb <- c(ulx, uly, lrx, lry))

# OK now for the MAIN EVENT

# Soil Grids url
sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"

# black-magic that tells gdal we're speaking Goode's language
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'

# gdal_translate goes to SG's webpage and
# downloads the .vrt data in a .tif in your workspace
# this is retrieving the mean variable of interest for surface soil
# many other data types are available

# pH
gdal_translate(paste0(sg_url,'phh2o/phh2o_0-5cm_mean.vrt'), # ph * 10
               "./crop_roi_igh_ph.tif",
               tr=c(250,250),
               projwin=bb,
               projwin_srs =igh)

ph_rst = rast("./crop_roi_igh_ph.tif")
plot(ph_rst/10) # ph units
hist(ph_rst/10)

# clay
gdal_translate(paste0(sg_url,'clay/clay_0-5cm_mean.vrt'), #g/kg
               "./crop_roi_igh_clay.tif",
               tr=c(250,250),
               projwin=bb,
               projwin_srs =igh)

clay_rst = rast("./crop_roi_igh_clay.tif")
plot(clay_rst/10) #g/100g ie., %
hist(clay_rst/10)

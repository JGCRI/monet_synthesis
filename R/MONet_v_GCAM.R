library(sf)
library(dplyr)
library(ggplot2)
GCAM_clay_content <- read.csv("./from Kanishka_clay/mapped_clay_KN.csv")

GCAM_clay_usa <- GCAM_clay_content%>%
  filter(iso == "usa", c_type == "clay content (0-30 cms)")%>%
  group_by(iso, glu_code, GCAM_GLU_name, c_type)%>%
  summarize(gcam_average = mean(weighted_average), gcam_median_value = median(median_value), gcam_min_value = min(min_value), gcam_max_value = max(max_value), gcam_q1_value = mean(q1_value), gcam_q3_value = mean(q3_value))


global_basins <- read_sf("./data/shapefiles/gcam_boundaries_moirai_3p1_0p5arcmin_wgs84/main_outputs/glu_boundaries_moirai_landcells_3p1_0p5arcmin.shp")

global_basins_prj <- global_basins%>%st_transform(crs = st_crs(conus_valid))

conus_basins <- st_intersection(global_basins, conus_valid)

all_clay <- bind_rows(clay_loc_sf_btm, clay_loc_sf_top)

clay_basins <- st_intersection(all_clay, conus_basins)

GCAM_clay_usa

clay_basin_summary <- clay_basins%>%
  data.frame()%>%
  group_by(core_section, glu_id, glu_nm)%>%
  summarize(Clay_percent_mean = mean(Clay_percent), monet_sd = sd(Clay_percent),
            monet_min = min(Clay_percent), monet_max = max(Clay_percent), monet_n = n())

MONet_GCAM_clay <- clay_basin_summary%>%
  left_join(GCAM_clay_usa, by = c("glu_id" = "glu_code"))



MONet_GCAM_clay%>%
  mutate(ymin = gcam_q1_value,
         ymax = gcam_q3_value)%>%
  ggplot(aes(x = Clay_percent_mean , y = gcam_average , color = glu_nm, shape = core_section))+
  geom_point()+
  geom_errorbar(aes(x = Clay_percent_mean , ymin = ymin, ymax = ymax ), alpha = 0.5)+
  geom_errorbar(aes(x = gcam_average , xmin = Clay_percent_mean - monet_sd, xmax = Clay_percent_mean + monet_sd), alpha = 0.5)+
  geom_abline(intercept = 0, slope = 1, color = "black")+
  stat_smooth(formula = y~x,method = "lm", se = TRUE, level = 0.95)+
  labs(title = "Clay Content GCAM vs MONet by Basin",
       x = "MONet", y = "GCAM")+
  theme_bw()


library(dplyr)

new_data <- read.csv(
  "Ref_veg_carbon_clay_Mg_per_ha_Mar24.csv"
)

new_data %>%
  filter(c_type == "clay (0-30 cms)") %>%
  arrange(iso, management) -> clay

clay %>%
  filter(management == "Cropland",
         iso == "usa") -> usa_crop_clay

hist(usa_crop_clay$median_value)
mean(usa_crop_clay$median_value)
sd(usa_crop_clay$median_value)


hist(usa_crop_clay$weighted_average)
mean(usa_crop_clay$weighted_average)
sd(usa_crop_clay$weighted_average)

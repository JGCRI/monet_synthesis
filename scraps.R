
# Select annual respiration from SRDB dadta
srdb %>%
  dplyr::select(Rs_annual, Record_number) -> srdb_slim

# Rename the 'place' column in srdb_data$annual to match 'Record_number' in srdb_slim
srdb_annual <- srdb_data$annual %>%
  rename(Record_number = place)

# Combine datasets by 'Record_number'
SRDB_data <- srdb_slim %>%
  inner_join(srdb_annual, by = "Record_number")

#TODO Rs conversion here
# Convert Rs from Rs annual to mgC/gSoil/day
bulk_density_g_cm2 <- 1.85
effective_soil_depth_cm <- 10

conversion_factor = 1000 * (1/(365 * bulk_density_g_cm2 * 10^5))

SRDB_mg_co2_c_g_soil_day <- SRDB_data%>%
  mutate(Rs_mg_co2_g_24h = Rs_annual * conversion_factor)


# Linear regression model to test the effect of MAP and MAT on Rs_annual
SRDB_model <- lm(Rs_annual ~ MAP + MAT, data = SRDB_mg_co2_c_g_soil_day)
summary(SRDB_model)

# Print regression summary
print(summary(SRDB_model))

# Plot Rs_annual vs MAP
plot_MAP <- ggplot(SRDB_mg_co2_c_g_soil_day, aes(x = MAP, y = Rs_mg_co2_g_24h)) +
  geom_point(color = myColors["comp_resp"], alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Effect of MAP on Rs (mg_co2_g_24h)",
       x = "Mean Annual Precipitation (MAP)",
       y = "Annual Soil Respiration (Rs_annual)") +
  theme_bw()+
  theme(axis.title.y = element_text(size = 7))

# Plot Rs_annual vs MAT
plot_MAT <- ggplot(SRDB_mg_co2_c_g_soil_day, aes(x = MAT, y = Rs_mg_co2_g_24h)) +
  geom_point(color = myColors["comp_resp"], alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Effect of MAT on Rs (mg_co2_g_24h)",
       x = "Mean Annual Temperature (MAT)",
       y = "Annual Soil Respiration (Rs_annual)") +
  theme_bw()+
  theme(axis.title.y = element_text(size = 7))

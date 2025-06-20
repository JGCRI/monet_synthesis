

monet_rs %>%
  select(Sample_Name, respiration_ppm_co2_c,
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
combined_monet <- monet_rs_metadata %>%
  inner_join(monet_data$annual, by = c("Site_Code" = "place"))

###
###
###TO DO
###
#currently only using the raw ppm C-CO2 data
#need to generate the same model and plots
#for mg C-CO2 per 24 / 96 h per gram of soil data
###

# Compute linear models and summarize using select() inside group_by
lm_models <- combined_monet %>%
  group_by(Core_Section) %>%
  summarise(
    model_MAT = list(lm(respiration_ppm_co2_c ~ MAT, data = select(., MAT, respiration_ppm_co2_c))),
    model_MAP = list(lm(respiration_ppm_co2_c ~ MAP, data = select(., MAP, respiration_ppm_co2_c)))
  )

# Display model summaries for 'MAT' and 'MAP'
for (i in 1:nrow(lm_models)) {
  cat("Core Section: ", lm_models$Core_Section[i], "\n")
  cat("Model MAT Summary:\n")
  print(summary(lm_models$model_MAT[[i]]))
  cat("\nModel MAP Summary:\n")
  print(summary(lm_models$model_MAP[[i]]))
  cat("\n----------------------\n")
}

# View the model summaries
print(model_summaries)

# Plotting Rs_annual vs MAT and MAP for each Core_Section separately
# Change 'Rs_annual' column as necessary to match the respiration data you are plotting

p_monet_MAT_core <- ggplot(combined_monet, aes(x = MAT, y = respiration_ppm_co2_c, color = Core_Section)) +
  geom_point() +
  geom_smooth(method = "lm", col = "black") +
  facet_wrap(~Core_Section) +
  labs(title = "Effect of MAT on Respiration by Core Section for Monet Data",
       x = "Mean Annual Temperature (MAT)",
       y = "Respiration (CO2 ppm in core sub-section)") +
  theme_minimal() +
  theme(legend.position = "bottom")

p_monet_MAP_core <- ggplot(combined_monet, aes(x = MAP, y = respiration_ppm_co2_c, color = Core_Section)) +
  geom_point() +
  geom_smooth(method = "lm", col = "black") +
  facet_wrap(~Core_Section) +
  labs(title = "Effect of MAP on Respiration by Core Section for Monet Data",
       x = "Mean Annual Precipitation (MAP)",
       y = "Respiration (CO2 ppm in core sub-section)") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_monet_MAT_core)
print(p_monet_MAP_core)

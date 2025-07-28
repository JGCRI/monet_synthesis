
str(combined_monet)

# Step 1: Compute weighted averages for each Site_Code and Core_Section
rs_combined <- combined_monet %>%
  dplyr::select( "ID",
"respiration_mg_co2_c_g_soil_day_24hour",
"respiration_mg_co2_c_g_soil_day_96hour",
"Site_Code", "Core_Section", "MAT", "MAP"
  ) %>% na.omit() %>%
  group_by(ID) %>%
  mutate(
    # Combine 24-hour and 96-hour measurements with equal weights (50% each)
    weighted_time_respiration = (1/3) * respiration_mg_co2_c_g_soil_day_24hour +
                                (2/3) * respiration_mg_co2_c_g_soil_day_96hour)

# Step 2: Apply weights for TOP and BTM sections within each Site_Code
rs_index <- rs_combined %>%
  group_by(Site_Code) %>%
  summarise(
    rs_index = sum(
      case_when(
        Core_Section == "TOP" ~ 0.65 * weighted_time_respiration, # Weight TOP section at 65%
        Core_Section == "BTM" ~ 0.35 * weighted_time_respiration  # Weight BTM section at 35%
      ),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# Step 3: Add MAT and MAP values for each Site_Code
rs_index_results <- rs_index %>%
  left_join(
    combined_monet %>% dplyr::select(Site_Code, MAT, MAP) %>% distinct(),
    by = "Site_Code"
  )

# View the final weighted Rs index table
print(rs_index_results)


str(combined_data)


# Perform the conversion using the formula
converted_srdb <- combined_data %>%
  mutate(
    Rs_daily_mg_per_g_soil = (Rs_annual * 1000) / (1.25 * 10 * 10000 * 365) # Conversion formula
  ) %>%
  slice_sample(n = 15)

# View the converted data
print(head(converted_srdb))


# Add "source" column to distinguish the datasets
converted_srdb <- converted_srdb %>%
  mutate(
    respiration_rate = Rs_daily_mg_per_g_soil, # Rename for consistency
    source = "SRDB"
  ) %>%
  dplyr::select(respiration_rate, MAT, MAP, source)

rs_index_results <- rs_index_results %>%
  mutate(
    respiration_rate = rs_index, # Rename for consistency
    source = "MONet"
  ) %>%
  dplyr::select(respiration_rate, MAT, MAP, source)

# Combine the two dataframes
combined_data_for_model <- bind_rows(converted_srdb, rs_index_results)

# View the combined dataframe
print(head(combined_data_for_model))

# Linear model testing if respiration rates differ between the two datasets
model <- lm(respiration_rate ~ source + MAT + MAP, data = combined_data_for_model)

# View model summary
summary(model)


library(dplyr)
library(ggplot2)
library(ggpmisc) # For displaying regression equations and R² values

# Step 1: Reshape the data (MAT and MAP into a single column)
data_long <- combined_data_for_model %>%
  pivot_longer(cols = c(MAT, MAP), names_to = "climate_parameter", values_to = "climate_value")

# Step 2: Create the facetted plot with regression equations
ggplot(data_long, aes(x = climate_value, y = respiration_rate)) +
  geom_point(aes(color = source), alpha = 0.7, size = 3) + # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "black") + # Linear regression line
  facet_grid(source ~ climate_parameter, scales = "free") + # Facet by source and climate_parameter
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 3
  ) + # Add regression equation and R² to each facet
  labs(
    title = "Facetted Plot: Respiration Rate vs Climate Parameters by Data Source",
    x = "Climate Parameter Value (MAT or MAP)",
    y = "Respiration Rate (mg C per g soil per day)"
  ) +
  scale_color_manual(values = c("SRDB" = "#0072B2", "MONet" = "#D55E00")) +
  theme_minimal()

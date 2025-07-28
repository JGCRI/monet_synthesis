
str(combined_monet)

# Step 1: Compute weighted averages for each Site_Code and Core_Section
rs_combined <- combined_monet %>%
  group_by(Site_Code, Core_Section) %>%
  summarise(
    avg_24hr_respiration = mean(respiration_mg_co2_c_g_soil_day_24hour, na.rm = TRUE),
    avg_96hr_respiration = mean(respiration_mg_co2_c_g_soil_day_96hour, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Combine 24-hour and 96-hour measurements with equal weights (50% each)
    weighted_time_respiration = 0.5 * avg_24hr_respiration + 0.5 * avg_96hr_respiration
  )

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
    Rs_daily_mg_per_g_soil = (Rs_annual * 1000) / (1.85 * 10 * 10000 * 365) # Conversion formula
  )

# View the converted data
print(head(converted_srdb))


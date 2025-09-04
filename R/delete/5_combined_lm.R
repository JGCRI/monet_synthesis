library(ggpmisc)

### Step 1: Prepare Monet Weighted Rs Index
rs_index_results <- combined_monet %>%
  dplyr::select(ID,
                respiration_mg_co2_c_g_soil_day_24hour,
                respiration_mg_co2_c_g_soil_day_96hour,
                Site_Code, Core_Section, MAT, MAP) %>%
  na.omit() %>%
  group_by(ID) %>%
  mutate(weighted_time_respiration = (1/3) * respiration_mg_co2_c_g_soil_day_24hour + (2/3) * respiration_mg_co2_c_g_soil_day_96hour) %>%
  group_by(Site_Code) %>%
  summarise(
    rs_index = sum( case_when(
        Core_Section == "TOP" ~ 0.65 * weighted_time_respiration,
        Core_Section == "BTM" ~ 0.35 * weighted_time_respiration),
      na.rm = TRUE),
    .groups = "drop") %>%
  left_join(combined_monet %>% distinct(Site_Code, MAT, MAP), by = "Site_Code") %>%
  mutate(
    respiration_rate = rs_index,
    source = "MONet"
  ) %>%
  dplyr::select(respiration_rate, MAT, MAP, source)

### Step 2: Prepare SRDB Data
set.seed(42)
converted_srdb <- combined_data %>%
  mutate(
    Rs_daily_mg_per_g_soil = (Rs_annual * 1000) / (1.25 * 10 * 10000 * 365) # Conversion formula
  ) %>%
  na.omit() %>%
  filter(MAT > 8 & MAT < 16.5 & MAP > 200 & MAP < 1230) %>% #filtering to climate range of MONet
  slice_sample(n = 64) %>% #4x MONet sample size
  mutate(
    respiration_rate = Rs_daily_mg_per_g_soil,
    source = "SRDB"
  ) %>%
  dplyr::select(respiration_rate, MAT, MAP, source)

### Step 3: Combine the Two Dataframes
combined_data_for_model <- bind_rows(converted_srdb, rs_index_results)

cd4m <- combined_data_for_model %>%
  group_by(source) %>%
  mutate(respiration_z = (respiration_rate - mean(respiration_rate)) / sd(respiration_rate)) %>%
  ungroup()

### Step 4: Linear Model Testing
model1 <- lm(respiration_z ~ source + MAT + MAP, data = cd4m)
summary(model1)

### Step 5: Reshape Data and Facetted Plot
cd4m %>%
  pivot_longer(cols = c(MAT, MAP), names_to = "climate_parameter", values_to = "climate_value") %>%
  ggplot(aes(x = climate_value, y = respiration_z)) +
  geom_point(aes(color = source), alpha = 0.7, size = 3) + # Scatter plot
  geom_smooth(method = "lm", se = FALSE, aes(color = source)) + # Regression line
  facet_wrap(climate_parameter ~ ., scales = "free_x") + # Facet by source and climate_parameter
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),
        group = source,
        color = source),
    formula = y ~ x,
    parse = TRUE,
    size = 3
  ) + # Regression equation and R²
  labs(
    title = "Rs vs Climate by Data Source",
    x = "Climate Parameter",
    y = "Z-score Respiration Rate (mg C per g soil per day)"
  ) +
  scale_color_manual(values = c("SRDB" = "#0072B2", "MONet" = "#D55E00")) +
  theme_minimal()

model2 <- lm(respiration_rate ~ source + MAT + MAP, data = cd4m)
summary(model2)

cd4m %>%
  pivot_longer(cols = c(MAT, MAP), names_to = "climate_parameter", values_to = "climate_value") %>%
  ggplot(aes(x = climate_value, y = respiration_rate)) +
  geom_point(aes(color = source), alpha = 0.7, size = 3) + # Scatter plot
  geom_smooth(method = "lm", se = FALSE, aes(color = source)) + # Regression line
  facet_wrap(source ~ climate_parameter, scales = "free") + # Facet by source and climate_parameter
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),
        group = source,
        color = source),
    formula = y ~ x,
    parse = TRUE,
    size = 3
  ) + # Regression equation and R²
  labs(
    title = "Rs vs Climate by Data Source",
    x = "Climate Parameter",
    y = "NOTE VARIABLE SCALE \n Respiration Rate (mg C per g soil per day)"
  ) +
  scale_color_manual(values = c("SRDB" = "#0072B2", "MONet" = "#D55E00")) +
  theme_minimal()

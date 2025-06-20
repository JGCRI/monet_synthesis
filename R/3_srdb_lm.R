
srdb %>%
  select(Rs_annual, Record_number) -> srdb_slim

# Rename the 'place' column in srdb_data$annual to match 'Record_number' in srdb_slim
srdb_annual <- srdb_data$annual %>%
  rename(Record_number = place)

# Combine datasets by 'Record_number'
combined_data <- srdb_slim %>%
  inner_join(srdb_annual, by = "Record_number")

# Inspect the combined data
print(combined_data)

# Linear regression model to test the effect of MAP and MAT on Rs_annual
model <- lm(Rs_annual ~ MAP + MAT, data = combined_data)
summary(model)

# Print regression summary
print(summary(model))

# Plot Rs_annual vs MAP
plot_MAP <- ggplot(combined_data, aes(x = MAP, y = Rs_annual)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Effect of MAP on Rs_annual",
       x = "Mean Annual Precipitation (MAP)",
       y = "Annual Soil Respiration (Rs_annual)") +
  theme_minimal()

# Plot Rs_annual vs MAT
plot_MAT <- ggplot(combined_data, aes(x = MAT, y = Rs_annual)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Effect of MAT on Rs_annual",
       x = "Mean Annual Temperature (MAT)",
       y = "Annual Soil Respiration (Rs_annual)") +
  theme_minimal()

# Display the plots
print(plot_MAP)
print(plot_MAT)

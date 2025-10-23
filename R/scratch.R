

# Create a combined label for soil order and clay content range
final_data <- final_data %>%
  mutate(
    combined_label = case_when(
      Soil_Order == "Mollisols"   ~ "Mollisols: 20–30%",
      Soil_Order == "Inceptisols" ~ "Inceptisols: 10–25%",
      Soil_Order == "Entisols"    ~ "Entisols: 5–15%",
      Soil_Order == "Alfisols"    ~ "Alfisols: 20–35%",
      Soil_Order == "N/A"         ~ "No Data Available",
      TRUE                        ~ NA_character_
    )
  )

# Define a custom discrete color palette using viridis
soil_colors <- viridis(
  n = 5,          # Number of categories
  option = "viridis" # Choose "magma," "inferno," etc.
)

# Create a named vector for colors, matching the combined labels
combined_label_colors <- setNames(
  soil_colors,
  c(
    "Mollisols: 20–30%",
    "Inceptisols: 10–25%",
    "Entisols: 5–15%",
    "Alfisols: 20–35%",
    "No Data Available"
  )
)

# Apply the color scale in ggplot
ggplot(data = final_data[final_data$Soil_Order != "N/A",]) +
  geom_sf(aes(fill = combined_label)) + # Use combined_label for fill mapping
  scale_fill_manual(values = combined_label_colors) +
  geom_sf(data = filtered_points, aes(color = Clay_percent), size = 2) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  ggtitle("Clay Content by Soil Order")

ggplot(data = final_data[final_data$Soil_Order != "N/A",]) +
  geom_sf(aes(fill = combined_label)) + # Use combined_label for fill mapping
  scale_fill_manual(values = combined_label_colors) +
  geom_sf(data = filtered_points, color = "red", size = 2) + # Add red points
  theme_minimal() + labs(fill = "Order") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  ggtitle("Clay Content by Soil Order")

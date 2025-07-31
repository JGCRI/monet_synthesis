library(patchwork) # Patchwork package for combining plots

# Create a summary plot for y-axis range comparison
scale_comparison <- cd4m %>%
  group_by(source) %>%
  summarise(y_min = min(respiration_rate, na.rm = TRUE), y_max = max(respiration_rate, na.rm = TRUE)) %>%
  ggplot(aes(x = source, y = y_max, fill = source)) +
  geom_col() +
  geom_text(aes(label = paste0("Max: ", round(y_max, 2))), vjust = -0.5, size = 3) +
  labs(
    title = "Scale Comparison",
    x = "Source",
    y = "Maximum Respiration Rate"
  ) +
  scale_fill_manual(values = c("SRDB" = "#0072B2", "MONet" = "#D55E00")) +
  theme_minimal()

# Main plot with inset combined
main_plot <- cd4m %>%
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
    title = "Respiration Rate vs Climate \nNOTE VARIABLE SCALE BETWEEN SOURCES",
    x = "Climate Parameter",
    y = "Respiration Rate (mg C per g soil per day)"
  ) +
  scale_color_manual(values = c("SRDB" = "#0072B2", "MONet" = "#D55E00")) +
  theme_minimal()

# Combine the main plot and the scale comparison plot
main_plot + scale_comparison

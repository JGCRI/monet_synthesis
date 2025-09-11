
#future home of 1:1 plots

sg_pH_buffer%>%
  mutate(ymin = sg_mean - sg_sd,
         ymax = sg_mean + sg_sd)%>%
  ggplot(aes(x = pH, y = sg_mean, color = core_section, fill = core_section))+
  geom_point()+
  geom_errorbar(aes(x = pH, ymin = ymin, ymax = ymax))+

  geom_abline(intercept = 0, slope = 1, color = "black")+
  stat_smooth(formula = y~x,method = "lm", se = TRUE, level = 0.95)+
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),
        group = core_section,
        color = core_section),
    formula = y ~ x,
    parse = TRUE,
    size = 3
  ) + # Regression equation and R²
  labs(title = "pH Soilgrids vs MONet",
       x = "MONet", y = "Soilgrids")+
  theme_bw()

ggsave("graphs/pH_OneToOne.png")

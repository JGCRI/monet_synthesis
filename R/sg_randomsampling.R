

load("R_data/combined_data.Rdata")

sample_sizes_pH_top <- pH_MONet_sg_top %>%
  as.data.frame()%>%
  dplyr::select(Climate, source)%>%
  group_by(Climate, source) %>%
  summarise(n = n())%>%
  pivot_wider(names_from = "source", values_from = "n")%>%
  filter(MONet > 1)%>%
  mutate(soilGrids = MONet)%>%
  mutate(axis_label = paste0(Climate, "\n", "MONet = ", MONet, ", Soilgrids = ", soilGrids))

pH_samples <- sg_random_sample(pH_MONet_sg_top, sample_sizes_pH_top, seeds = c(1, 5, 10))

sample_colors <- c("#8080C0", "#D6AA80", "#D98040", "#CC5500")
names(sample_colors) <- unique(pH_samples$source)

pH_samples%>%
  mutate(Experiment = as.factor(if_else(source == "MONet", "MONet_pH", source)))%>%
  left_join(sample_sizes_pH_top, by = c("Climate"))%>%
  filter(!is.na(axis_label))%>%
  ggplot(aes(x = pH, y = axis_label,
             fill = source, color = source)) +
  geom_violin(alpha = 0.7,
              position = position_dodge(width = 0.8),
              scale = "width") +  # Equal widths regardless of sample size
  geom_boxplot(fill = NA,
               position = position_dodge(width = 0.8),
               outlier.shape = NA)+
  theme_bw()+
  labs(
    title = "MONet and Soilgrids pH by Climate Zone",
    x = "pH",
    y = "Climate Zone"
  )+
  scale_y_discrete(labels = label_wrap_gen(30))+
  scale_fill_manual(name = "Date Source", values = sample_colors)+
  scale_color_manual(name = "Data Source", values = sample_colors)

rm(pH_samples)
ggsave("graphs/pH_randomsample.png")

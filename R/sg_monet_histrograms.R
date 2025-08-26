
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(sf)

load("R_data/processed_data.Rdata")
load("R_data/sg_data.Rdata")

myColors <- c( "#000080","#4040A0","#8080C0", "#D6AA80", "#D98040", "#CC5500")
names(myColors) <- c("MONet_resp", "MONet_pH", "MONet_clay",
                     "comp_clay", "comp_pH", "comp_resp")

process_soil_data <- function(type, section, MONet_df, sg_df, climate_mapping_df, sg_column_name) {
  # Process MONet data
  MONet_data <- MONet_df %>%
    left_join(climate_mapping_df, by = c("Climate", "Code")) %>%
    filter(core_section == section) %>%
    dplyr::select(Climate, !!type) %>%
    mutate(source = "MONet")

  # Process SoilGrids data
  sg_data <- sg_df %>%
    left_join(climate_mapping_df, by = c("zone" = "Code")) %>%
    filter(Climate %in% unique(MONet_data$Climate)) %>%
    rename(!!type := sg_column_name) %>%
    dplyr::select(Climate, !!type) %>%
    mutate(source = "soilGrids",
           !!type := !!sym(type) / 10) # convert units

  # Combine MONet and soilGrids data
  combined_data <- bind_rows(MONet_data, sg_data)
  return(combined_data)
}

clay_MONet_sg_top <- process_soil_data(
  type = "Clay_percent",
  section = "TOP",
  MONet_df = clay_zones,
  sg_df = clay_top_long,
  climate_mapping_df = climate_zone_mapping,
  sg_column_name = "crop_roi_igh_clay_0.5cm"
)

clay_MONet_sg_btm <- process_soil_data(
  type = "Clay_percent",
  section = "BTM",
  MONet_df = clay_zones,
  sg_df = clay_btm_long,
  climate_mapping_df = climate_zone_mapping,
  sg_column_name = "crop_roi_igh_clay_15.30cm"
)

clay_top <- clay_MONet_sg_top%>%
  mutate(Experiment = as.factor(if_else(source == "MONet", "MONet_clay", "comp_clay")))%>%
  ggplot(aes(x = Clay_percent, fill = Experiment))+
  geom_histogram(mapping = aes(y = after_stat(density)), position = "identity", alpha = 0.7)+
  theme_bw()+
  scale_fill_manual(name = "Experiment", values = myColors, labels = c("comp_clay" = "Soilgrids", "MONet_clay" = "MONet"))+
  labs(
    title = "Soilgrids and MONet Clay Share Distribution (Top sample)",
    x = "Clay Content (%)"
  )

clay_btm <- clay_MONet_sg_btm%>%
  mutate(Experiment = as.factor(if_else(source == "MONet", "MONet_clay", "comp_clay")))%>%
  ggplot(aes(x = Clay_percent, fill = Experiment))+
  geom_histogram(mapping = aes(y = after_stat(density)), position = "identity", alpha = 0.7)+
  theme_bw()+
  scale_fill_manual(name = "Experiment", values = myColors, labels = c("comp_clay" = "Soilgrids", "MONet_clay" = "MONet"))+
  labs(
    title = "Soilgrids and MONet Clay Share Distribution (Bottom sample)",
    x = "Clay Content (%)"
  )

pH_MONet_sg_top <- process_soil_data(
  type = "pH",
  section = "TOP",
  MONet_df = pH_zones,
  sg_df = pH_top_long,
  climate_mapping_df = climate_zone_mapping,
  sg_column_name = "crop_roi_igh_ph_0.5cm"
)

pH_MONet_sg_btm <- process_soil_data(
  type = "pH",
  section = "BTM",
  MONet_df = pH_zones,
  sg_df = pH_btm_long,
  climate_mapping_df = climate_zone_mapping,
  sg_column_name = "crop_roi_igh_ph_15.30cm"
)


pH_top <- pH_MONet_sg_top%>%
  mutate(Experiment = as.factor(if_else(source == "MONet", "MONet_pH", "comp_pH")))%>%
  ggplot(aes(x = pH, fill = Experiment))+
  geom_histogram(mapping = aes(y = after_stat(density)), position = "identity", alpha = 0.7)+
  theme_bw()+
  scale_fill_manual(name = "Experiment", values = myColors, labels = c("comp_pH" = "Soilgrids", "MONet_pH" = "MONet"))+
  labs(
    title = "Soilgrids and MONet pH Distribution (Top sample)",
    x = "pH"
  )

pH_btm <- pH_MONet_sg_btm%>%
  mutate(Experiment = as.factor(if_else(source == "MONet", "MONet_pH", "comp_pH")))%>%
  ggplot(aes(x = pH, fill = Experiment))+
  geom_histogram(mapping = aes(y = after_stat(density)), position = "identity", alpha = 0.7)+
  theme_bw()+
  scale_fill_manual(name = "Experiment", values = myColors, labels = c("comp_pH" = "Soilgrids", "MONet_pH" = "MONet"))+
  labs(
    title = "Soilgrids and MONet pH Distribution (Bottom sample)",
    x = "pH"
  )

clay_hist <- ggarrange(clay_top, clay_btm, nrow = 2, ncol = 1)
ggsave(plot = clay_hist, "clay_hist.png")
ggsave(plot = clay_top, "clay_top.png")
ggsave(plot = clay_btm, "clay_btm.png")

pH_hist <- ggarrange(pH_top, pH_btm, nrow = 2, ncol = 1)
ggsave(plot = pH_hist, "pH_hist.png")
ggsave(plot = pH_top, "pH_top.png")
ggsave(plot = pH_btm, "pH_btm.png")

save(pH_MONet_sg_top,
     clay_MONet_sg_top,
     file = "R_data/combined_data.Rdata")

process_plot_data <- function(soil_data, n_samples = 1000){
  plot_data <- soil_data %>%
    group_by(Climate, source) %>%
    slice_sample(n = 1000) %>%
    ungroup()

  sample_sizes <- plot_data %>%
    as.data.frame()%>%
    dplyr::select(Climate, source)%>%
    group_by(Climate, source) %>%
    summarise(n = n())%>%
    pivot_wider(names_from = "source", values_from = "n")%>%
    filter(MONet > 1)%>%
    mutate(axis_label = paste0(Climate, "\n", "MONet = ", MONet, ", Soilgrids = ", soilGrids))

  plot_data_samples <- left_join(plot_data, sample_sizes, by = "Climate")

  return(plot_data_samples)
}

plot_clay_top <- process_plot_data(clay_MONet_sg_top)

plot_clay_btm <- process_plot_data(clay_MONet_sg_btm)

plot_pH_top <- process_plot_data(pH_MONet_sg_top)

plot_pH_btm <- process_plot_data(pH_MONet_sg_btm)


save(plot_clay_btm, plot_clay_top, plot_pH_btm, plot_pH_top, file = "R_data/violin_data.Rdata")

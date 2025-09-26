
library(sf)
library(dplyr)
library(ggplot2)
library(stringdist)

# Web Soil Survey data, user-defined AOI
attributes <- st_read("data/wss/plot_soilmu_a_aoi.shp")
colnames(attributes)

taxonomy_1 <- read.csv("data/wss/Report - MANU - Legend by Symbol_GrandMesa.csv")
taxonomy_2 <- read.csv("data/wss/Report - MANU - Legend by Symbol_Gunnison.csv")
taxonomy_3 <- read.csv("data/wss/Report - MANU - Legend by Symbol_Taylor.csv")

taxonomy <- rbind(taxonomy_1, taxonomy_2, taxonomy_3)

colnames(taxonomy)[colnames(taxonomy) == "musym"] <- "MUSYM"

combined_data <- attributes %>% left_join(taxonomy, by = "MUSYM")


# Combine similar `muname` values in `combined_data`
combined_data <- combined_data %>%
  mutate(
    muname_grouped = case_when(
      muname %in% c("Water", "WATER") ~ "Water",
      muname %in% c("Rock outcrop", "Rock land",
                    "Rock slides", "Rubble land",
                    "Shale rock land") ~ "Rock catch-all",
      muname %in% c("Landslides and Gullied land",
                    "Stony colluvial land",
                    "Alluvial land") ~ "Floodplain and Landslides",
      TRUE ~ muname # Keep all other values as-is
    )
  )

# Plot the spatial data with taxonomy information
plot(combined_data["muname_grouped"])  # Replace "taxonomic_attribute" with relevant column name


# Hmm....not enough soil taxonomix information there...
# bringing in some additional files from WSS
tax1 <- read.csv("data/wss/Taxonomic Classification of the Soils_grandmesa.csv")
orders1 <- tax1[,4:5]
tax2 <- read.csv("data/wss/Taxonomic Classification of the Soils_gunnison.csv")
orders2 <- tax2[,4:5]
tax3 <- read.csv("data/wss/Taxonomic Classification of the Soils_taylor.csv")
orders3 <- tax3[,4:5]

# combine
orders <- rbind(orders1, orders2, orders3)

# Remove rows with empty or whitespace-only entries in `Soil.Name` or `Taxonomic.Classification`
orders_cleaned <- orders %>%
  filter(
    trimws(Soil.Name) != "" & trimws(Taxonomic.Classification) != "" # Keep rows with actual data
  ) %>%
  arrange(Soil.Name) # Sort by `Soil.Name`

# Add some rows for 'other' categories
new_rows <- data.frame(
  Soil.Name = c("Water", "Floodplain and Landslides", "Rock catch-all"),
  Taxonomic.Classification = c("Water", "Floodplain and Landslides", "Rock catch-all")
)

orders_updated <- rbind(orders_cleaned, new_rows)

# Perform fuzzy matching using stringdistmatrix
distance_matrix <- stringdistmatrix(tolower(combined_data$muname_grouped), tolower(orders_updated$Soil.Name), method = "jw")

# Find the closest match for each entry in `muname_grouped`
closest_matches <- apply(distance_matrix, 1, which.min)

# Add the matched `Soil.Name` and `Taxonomic.Classification` to `combined_data`
combined_data <- combined_data %>%
  mutate(
    Soil.Name = orders_updated$Soil.Name[closest_matches],
    Taxonomic.Classification = orders_updated$Taxonomic.Classification[closest_matches]
  )

# Extract the soil subgroup (last term) from Taxonomic.Classification
combined_data <- combined_data %>%
  mutate(
    Subgroup = gsub(".*\\s(\\S+)$", "\\1", Taxonomic.Classification), # Regex to isolate subgroup name only
    Modifier_Subgroup = ifelse( # Existing logic for Modifier_Subgroup
      grepl("^\\S+$", Taxonomic.Classification),
      paste("Unspecified", Taxonomic.Classification),
      gsub(".*\\s(\\S+\\s\\S+)$", "\\1", Taxonomic.Classification)
    )
  )


# Check the unique values of the Subgroup column
unique_subgroups <- unique(combined_data$Subgroup)
print(unique_subgroups)

# Visualize spatial data by Modifier_Subgroup
plot(combined_data["Modifier_Subgroup"])  # Replace with the relevant plot column

# Define a lookup table for subgroup endings and their corresponding soil orders
subgroup_to_order <- list(
  "olls"      = "Mollisols",
  "epts"      = "Inceptisols",
  "ents"      = "Entisols",
  "erts"      = "Vertisols",
  "alfs"      = "Alfisols",
  "ults"      = "Ultisols",
  "ox"        = "Oxisols",
  "ids"       = "Aridisols",
  "ists"      = "Histosols",
  "ods"        = "Spodosols"
)

# Update the dataframe with a new column for soil order based on subgroup ending
combined_data <- combined_data %>%
  mutate(
    Soil_Order = case_when(
      grepl("Water|Rock catch-all|Floodplain and Landslides", Taxonomic.Classification, ignore.case = TRUE) ~ "N/A", # For non-soil categories
      TRUE ~ sapply(Subgroup, function(x) {
        match <- sapply(names(subgroup_to_order), function(end) grepl(end, x, ignore.case = TRUE))
        if (any(match)) {
          # Return the corresponding soil order based on the first match
          subgroup_to_order[[names(subgroup_to_order)[which(match)[1]]]]
        } else {
          NA_character_ # If no match, assign NA
        }
      })
    )
  )

final_data <- combined_data %>%
  mutate(
    Subgroup = ifelse(Soil_Order == "N/A", "N/A", Subgroup),
    Modifier_Subgroup = ifelse(Soil_Order == "N/A", "N/A", Modifier_Subgroup)
  )

ggplot(data = final_data[final_data$Modifier_Subgroup != "N/A",]) +
  geom_sf(aes(fill = Modifier_Subgroup)) + # Use fill to group by soil subgroup
  ggtitle("Soil Orders") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  ) +
  labs(fill = "Soil Subgroup") # Customize legend title

# Define clay content mapping based on Soil_Order
final_data <- final_data %>%
  mutate(
    clay_content = case_when(
      Soil_Order == "Mollisols"   ~ "20–30%",
      Soil_Order == "Inceptisols" ~ "10–25%",
      Soil_Order == "Entisols"    ~ "5–15%",
      Soil_Order == "Alfisols"    ~ "20–35%",
      Soil_Order == "N/A"         ~ "N/A",
      TRUE                        ~ NA_character_ # Default if no match
    )
  )

library(raster)
library(ggplot2)
library(ggnewscale)
library(scales)
library(dplyr)
library(cowplot)
library(tidyr)

load("D:/PROslide_RIO/DATA2/final_train.Rd")

# Define landcover labels
landcover_labels <- c(
  "1" = "Rocky/Sediments", 
  "2" = "Forest/Shrub", 
  "3" = "Grass/Woody", 
  "4" = "Water/Flood",
  "5" = "Favela", 
  "6" = "Residential", 
  "7" = "Agri/Empty", 
  "8" = "Infra/Comm/Ind"
)

# Recode slide column to factor with custom labels
final_train <- final_train %>%
  mutate(
    slide = factor(slide, levels = c(TRUE, FALSE), labels = c("slide", "no-slide")),
    landcover_label = factor(
      as.character(landcover19),
      levels = names(landcover_labels),
      labels = landcover_labels
    ),
    landcover_group = case_when(
      landcover_label == "Favela" ~ "Favela",
      landcover_label == "Residential" ~ "Residential",
      TRUE ~ "Others"
    ),
    landcover_group = factor(landcover_group, levels = c("Favela", "Residential", "Others"))
  )list.files()

# Count TRUE/FALSE per group
label_df <- final_train %>%
  group_by(landcover_group, slide) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = slide, values_from = n, values_fill = 0) %>%
  mutate(label = paste0("slide: ", `slide`, "\nno-slide: ", `no-slide`))

# Plot with renamed legend and counts
ggplot(final_train, aes(x = slope, y = dtm, color = slide)) +
  geom_point(alpha = 0.3, size = 1) +
  facet_wrap(vars(landcover_group)) +
  geom_label(data = label_df,
             aes(x = 75, y = 950, label = label),
             inherit.aes = FALSE, hjust = 1, vjust = 1, size = 3.5) +
  labs(x = "Slope (°)", y = "Elevation (m)", colour = "Landslide") +
  
  theme(legend.position = "bottom")

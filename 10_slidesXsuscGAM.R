library(ggplot2)
library(dplyr)
library(raster)
library(sf)
library(lubridate)
library(mapview)
library(parallel)
library(doParallel)

pred = raster("D:/PROslide_RIO/DATA2/pred_megam.tif")

rcl = matrix(c(-Inf, 0, 0,   0, 0.211765, 1,  0.211765, 0.580392, 2,  0.580392, 1, 3), ncol=3, byrow=TRUE) #This is the Original MoNOE

reclassify(pred, rcl, filename="D:/PROslide_RIO/DATA2/pred_megam_class.tif", overwrite = TRUE)


pred_class = raster("D:/PROslide_RIO/DATA2/pred_megam_class.tif")

# Read the shapefile using sf package
rioslides <- sf::st_read("D:/PROslide_RIO/DATA/landslides_2023.shp")

rioslides <- rioslides %>%
  dplyr::filter(tipologia1 == 1 | tipologia2 == 1 | tipologia3 == 1 | tipologia4 == 1)


# Define factor levels for the new values
rcl <- data.frame(ID = c(1, 2, 3),
                  level = c("Baixo", "Medio", "Alto"))

# Apply factor levels to the raster
pred_class <- ratify(pred_class)
levels(pred_class) <- list(rcl)

# Define the colors
colors <- c('Baixo' = "gray", 'Medio' = "#987645", 'Alto' = "#65828D")

# Selecting only the necessary fields
rioslides_reduced <- rioslides %>%
  dplyr::select(geometry)


# Extract the coordinates
coordinates <- sf::st_coordinates(rioslides)
x_coords <- coordinates[, "X"]
y_coords <- coordinates[, "Y"]

# Add the coordinates to the original sf object
rioslides$X <- x_coords
rioslides$Y <- y_coords

# Add an ID
rioslides$ID <- seq(nrow(rioslides))

# Remove rows with NA in coordinates
rioslides <- rioslides[!rowSums(is.na(rioslides[, c("X", "Y")])),]

# Convert X and Y to numeric if they are not already
rioslides$X <- as.numeric(rioslides$X)
rioslides$Y <- as.numeric(rioslides$Y)

# Create a matrix of coordinates
coords_matrix <- cbind(rioslides$X, rioslides$Y)

# Create SpatialPoints
sp_points <- SpatialPoints(coords = coords_matrix)

# Convert to SpatialPointsDataFrame
rioslides_spdf <- SpatialPointsDataFrame(sp_points, data = as.data.frame(rioslides))




# Get the total number of available cores
totalCores <- detectCores()

# Calculate 60% of the available cores
numCores <- floor(totalCores * 0.3)

# Begin cluster with the calculated number of cores
beginCluster(numCores)

# Extracting from predictions
rioslides_spdf_extract <- raster::extract(pred_class, rioslides_spdf, df = TRUE)
endCluster()


rioslides   <- merge(rioslides_spdf_extract,   rioslides,   by.x = 'ID', by.y = 'ID')

rioslides <- rioslides[rioslides$pred_megam_class != 255, ]















Summary <- rioslides %>%
  dplyr::group_by(pred_megam_class) %>%
  dplyr::summarise(n_slide = n()) %>%
  filter(!is.na(pred_megam_class))

pred_recode <- c("1" = "Low",
                 "2" = "Medium",
                 "3" = "High")

Summary$pred_megam_class = as.character(Summary$pred_megam_class)
Summary$pred_recode <- as.character(pred_recode[Summary$pred_megam_class])

Summary$pred_recode <- factor(Summary$pred_recode, levels = c("Low", "Medium", "High"))

# Add area coverage percentages
area_coverage <- c("Low" = 57, "Medium" = 24, "High" = 19)
Summary$area_coverage <- area_coverage[Summary$pred_recode]

# Create pie chart for area coverage
area_coverage_df <- data.frame(
  class = names(area_coverage),
  percentage = area_coverage
)



library(ggplot2)
library(gridExtra)
library(cowplot)

# Define a common theme for both plots with further increased font sizes
bar_theme <- theme(
  plot.title = element_text(size = 28, face = "bold"),  # Increased more from 24 to 28
  axis.title.x = element_text(size = 22),  # Increased more from 18 to 22
  axis.title.y = element_text(size = 22),  # Increased more from 18 to 22
  axis.text.x = element_text(size = 18),  # Increased more from 14 to 18
  axis.text.y = element_text(size = 18),  # Increased more from 14 to 18
  legend.text = element_text(size = 22),  # Increased more from 18 to 22
  legend.title = element_text(size = 22)  # Increased more from 18 to 22
)

pizza_theme <- theme(
  plot.title = element_text(size = 28, face = "bold"),  # Increased more from 24 to 28
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  legend.text = element_text(size = 22),  # Increased more from 18 to 22
  legend.title = element_text(size = 22),  # Increased more from 18 to 22
  legend.position = "bottom"
)

# Filter out NA from Summary data
Summary <- Summary %>%
  filter(!is.na(pred_recode))

# Create pie chart for area coverage
gg_area_coverage <- ggplot(area_coverage_df, aes(x = "", y = percentage, fill = class)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(name = "Susceptibility class:", values = c('Low' = "gray", 'Medium' = "#9B835B", 'High' = "#65828D")) +
  labs(title = "(A) Area Coverage by Susceptibility Class") +
  geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5), size = 8) +  # Further increased size of labels
  pizza_theme +
  theme(legend.position = "none")

# Create bar chart for number of landslides
gg_landslides <- ggplot(Summary, aes(x = pred_recode, y = n_slide, fill = pred_recode)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n_slide), vjust = 2, size = 8) +  # Add labels to the bars
  scale_fill_manual(name = "Susceptibility class:", values = c('Low' = "gray", 'Medium' = "#9B835B", 'High' = "#65828D")) +
  labs(title = "(B) Number of Landslides by Susceptibility Class", x = "Susceptibility Class", y = "Number of Landslides") +
  bar_theme +
  theme(legend.position = "none")

# Extract the legend from the pie chart
legend <- cowplot::get_legend(gg_area_coverage + theme(legend.position = "bottom", legend.direction = "horizontal"))

# Arrange the plots side by side with a common legend below
combined_plot <- plot_grid(
  gg_area_coverage, gg_landslides, 
  ncol = 2, rel_widths = c(1, 1)
)
combined_plot_with_legend <- plot_grid(combined_plot, legend, ncol = 1, rel_heights = c(1, 0.1), align = 'v')

# Save the combined plot as an image
#ggsave("D:/PROslide_RIO/PRESENTATIONS/combined_plot.png", plot = combined_plot_with_legend, width = 20, height = 9)
#ggsave("D:/PROslide_RIO/Rcodes/Shinny_app_RioSlide/combined_plot.png", plot = combined_plot_with_legend, width = 10, height = 4)
ggsave("D:/PROslide_RIO/Figs/combined_plot.png", plot = combined_plot_with_legend, width = 20, height = 9)





combined_plot_one_above_other <- plot_grid(
  gg_area_coverage, gg_landslides, 
  ncol = 1, rel_heights = c(1, 1)
)
combined_plot_one_above_other_with_legend <- plot_grid(combined_plot_one_above_other, legend, ncol = 1, rel_heights = c(1, 0.1), align = 'v')

# Save the one above the other combined plot as an image
ggsave("D:/PROslide_RIO/Figs/combined_plot_one_above_other.png", plot = combined_plot_one_above_other_with_legend, width = 10, height = 18)




library(ggplot2)
library(dplyr)
library(raster)
library(sf)
library(lubridate)
library(mapview)


RJ = sf::st_read("D:/PROslide_RIO/DATA/StudyArea.shp")

pred = raster("D:/PROslide_RIO/Susc_heuristic/suscetibilidade_rio.tif")
slope <- raster("D:/PROslide_RIO/DATA/slope.tif")

# Read the shapefile using sf package
rioslides <- sf::st_read("D:/PROslide_RIO/DATA/landslides_2023.shp")
summary(as.factor(rioslides$tipologia1))
#0    1    2    3    4    5    6    7    8    9   10   11 
#59 1659   32   25    1  323   55  449   12   71  155  157 

rioslides <- rioslides %>%
  dplyr::filter(tipologia1 == 1 | tipologia2 == 1 | tipologia3 == 1 | tipologia4 == 1)

rioslides$year <- lubridate::year(rioslides$data) 
# Filter out rows where the year is 3748
rioslides <- rioslides %>%
  dplyr::filter(is.na(year) | year != 3748)

rioslides$has_date <- ifelse(is.na(rioslides$data), "No Date", "Has Date")
#sf::st_write(rioslides, "D:/PROslide_RIO/DATA/only_landslides_2023.shp", append=FALSE)

# Reclassify the values 1, 2, 3 to new values 1, 2, 3
reclass_matrix <- matrix(c(1, 1, 1,
                           2, 2, 2,
                           3, 3, 3),
                         ncol = 3, byrow = TRUE)
reclassified_raster <- reclassify(pred, reclass_matrix)

# Define factor levels for the new values
rcl <- data.frame(ID = c(1, 2, 3),
                  level = c("Baixo", "Medio", "Alto"))

# Apply factor levels to the raster
reclassified_raster <- ratify(reclassified_raster)
levels(reclassified_raster) <- list(rcl)

# Define the colors
colors <- c('Baixo' = "white", 'Medio' = "#EFC000FF", 'Alto' = "darkorange1")

# # Selecting only the necessary fields
# rioslides_reduced <- rioslides %>%
#   dplyr::select(geometry, year)



# mapview(RJ, col.regions = "red", alpha.regions = 0) +
#   mapview(reclassified_raster, col.regions = colors, alpha = 0.5, maxpixels = 107792256/10) +
#   mapview(rioslides_reduced, zcol = "year", cex = 2, lwd = 0.5, na.color = "lightblue")






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

gc()
# Extracting from predictions
#beginCluster()
rioslides_spdf_extract <- raster::extract(pred, rioslides_spdf, df = TRUE)
#endCluster()


rioslides   <- merge(rioslides_spdf_extract,   rioslides,   by.x = 'ID', by.y = 'ID')

names(rioslides)[names(rioslides) == "count_"] <- "suscetibilidade_rio"
# Select only the specified columns using dplyr's select
rioslides <- dplyr::select(rioslides, suscetibilidade_rio, ID, geometry, year, has_date)
#st_write(rioslides, "D:/PROslide_RIO/Rcodes/Shinny_app_RioSlide/landslides_2023_with_pred.shp", overwrite= TRUE)

rioslides <- rioslides[rioslides$suscetibilidade_rio != 255, ]

Summary <- rioslides %>%
  dplyr::group_by(suscetibilidade_rio) %>%
  dplyr::summarise(n_slide = n())

pred_recode <- c("1" = "Low",
                 "2" = "Medium",
                 "3" = "High")

Summary$suscetibilidade_rio = as.character(Summary$suscetibilidade_rio)
Summary$pred_recode <- as.character(pred_recode[Summary$suscetibilidade_rio])

Summary$pred_recode <- factor(Summary$pred_recode, levels = c("Low", "Medium", "High"))

# Add area coverage percentages
area_coverage <- c("Low" = 59, "Medium" = 29, "High" = 12)
Summary$area_coverage <- area_coverage[Summary$pred_recode]

# Create pie chart for area coverage
area_coverage_df <- data.frame(
  class = names(area_coverage),
  percentage = area_coverage
)
library(ggplot2)
library(gridExtra)
library(cowplot)

# Define a common theme for both plots with increased font sizes
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

# Order the levels of the factor for filling
area_coverage_df$class <- factor(area_coverage_df$class, levels = c("Low", "Medium", "High"))
Summary$pred_recode <- factor(Summary$pred_recode, levels = c("Low", "Medium", "High"))

# Define colors with transparency
colors_with_alpha <- c('Low' = alpha("#3e961b", 0.5), 'Medium' = alpha("#eaea44", 0.5), 'High' = alpha("#c1435d", 0.5))

# Create pie chart for area coverage
gg_area_coverage <- ggplot(area_coverage_df, aes(x = "", y = percentage, fill = class)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(name = "Susceptibility class:", values = colors_with_alpha) +
  labs(title = "(A) Area Coverage by Susceptibility Class") +
  geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5), size = 8) +  # Further increased size of labels
  pizza_theme +
  theme(legend.position = "none")

# Create bar chart for number of landslides
gg_landslides <- ggplot(Summary, aes(x = pred_recode, y = n_slide, fill = pred_recode)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n_slide), vjust = 5, size = 8) +  # Add labels to the bars
  scale_fill_manual(name = "Susceptibility class:", values = colors_with_alpha) +
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

# Add the caption
final_plot <- ggdraw() +
  draw_plot(combined_plot_with_legend, 0, 0.1, 1, 0.9) 

# Save the final plot as an image
#ggsave("D:/PROslide_RIO/PRESENTATIONS/A_heur_combined_plot.png", plot = final_plot, width = 20, height = 9)
#ggsave("D:/PROslide_RIO/Rcodes/Shinny_app_RioSlide/www/A_heur_combined_plot.png", plot = final_plot, width = 10, height = 4)
ggsave("D:/PROslide_RIO/Figs/A_heur_combined_plot.png", plot = final_plot, width = 20, height = 9)





combined_plot_one_above_other <- plot_grid(
  gg_area_coverage, gg_landslides, 
  ncol = 1, rel_heights = c(1, 1)
)
combined_plot_one_above_other_with_legend <- plot_grid(combined_plot_one_above_other, legend, ncol = 1, rel_heights = c(1, 0.1), align = 'v')

# Save the one above the other combined plot as an image
ggsave("D:/PROslide_RIO/Figs/A_heur_combined_plot_b.png", plot = combined_plot_one_above_other_with_legend, width = 10, height = 18)












pred_pts <- rasterToPoints(pred, spatial = TRUE)
pred_df  <- as.data.frame(pred_pts) #%>% select(hillshade_la,x,y)

col_pred <- c("white", "yellow", "#FB9530")

ggplot() +
  geom_sf(data = RJ, fill = NA) +
  geom_sf(data = rioslides, aes(color = has_date), size= .1) +
  
  geom_raster(data= pred_df, aes(x = x,  y = y, fill = as.factor(count_)))+
  scale_fill_manual(values = col_pred, name="Pred classes", na.value="transparent")+
  scale_alpha(name = "", range = c(0.6, 0), guide = "none")+
  ggnewscale::new_scale_fill()+
  
  theme_minimal() +
  labs(title = "Landslides in RJ", 
       subtitle = "Points colored based on presence of date") +
  theme(
    axis.text = element_blank(),  # Remove axis text
    axis.title = element_blank(), # Remove axis title
    axis.ticks = element_blank(), # Remove axis ticks
    legend.position = "bottom"   # Place legend at the bottom
  )

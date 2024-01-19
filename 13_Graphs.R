library(ggplot2)
library(sf)
library(viridis)
library(tidyr) 
library(dplyr)
library(scales)


# Read the shapefiles
final_rioslides <- st_read("D:/PROslide_RIO/DATA2/final_rioslides.shp")
study_area <- st_read("D:/PROslide_RIO/DATA/StudyArea.shp")


map_theme= theme(legend.title =  element_text(size = 8),
                 legend.position = "bottom",#c(0.25, .9),
                 legend.direction = "horizontal",  # Make the color legend horizontal
                 legend.box = "horizontal",
                 plot.title =element_text(size=10, hjust=0.08),
                 axis.text = element_blank(),
                 axis.title = element_blank(),
                 axis.ticks = element_blank(),
                 axis.line = element_blank(),
                 legend.text = element_text(size = 7),  # Adjust the legend text size
                 legend.key.width = unit(1, "cm"),  # Increase the legend key width
                 legend.key.height = unit(1/4, "cm")  # Increase the legend key height
)

################################################################################
################################################################################
################################################################################
final_rioslides$data <- as.Date(final_rioslides$data)

# Extract the month and year from the date
final_rioslides$Year <- format(as.Date(final_rioslides$data), "%Y")
final_rioslides$Month <- format(as.Date(final_rioslides$data), "%m")

# Now, we count the occurrences by month and year
monthly_counts <- final_rioslides %>%
  group_by(Year, Month) %>%
  summarise(Count = n(), .groups = 'drop') %>% as.data.frame()

# Ensure there is an entry for every combination of month and year, filling with 0 where data is missing
full_grid <- expand.grid(Year = unique(final_rioslides$Year), Month = sprintf("%02d", 1:12))
monthly_counts <- merge(full_grid, monthly_counts, by = c("Year", "Month"), all.x = TRUE)
monthly_counts$Count[is.na(monthly_counts$Count)] <- NA

# Convert Month to a factor to have proper ordering in the plot
monthly_counts$Month <- factor(monthly_counts$Month, levels = rev(sprintf("%02d", 1:12)), labels = rev(month.abb))

max_count <- max(monthly_counts$Count, na.rm = TRUE)
breaks <- seq(0, max_count, by = 100)

colors <- c("white", viridis(length(breaks) + 50, option = "viridis"))

# Now we can plot
gg1=ggplot(monthly_counts, aes(x = Year, y = Month, fill = Count)) +
  geom_tile(color = "white") + # Add borders to the tiles
  geom_text(aes(label = Count),  color = ifelse(monthly_counts$Count < 5 | monthly_counts$Count > 400, "black", "white"), 
             fill = "white", # Background color of the label
             label.size = NA, # Remove the border around the label
             label.padding = unit(0.25, "lines")) + # Adjust label padding
  scale_fill_gradientn(
    colours = colors,
    values = rescale(breaks), # Rescale the breaks to [0, 1]
    limits = c(0, max_count),
    breaks = breaks, # Set breaks
    labels = breaks, # Use breaks as labels
    name = "# Events"
  ) +
  theme_minimal() +
  labs(title = "Monthly Landslide Occurrences",
       x = "Year",
       y = "Month",
       fill = "Count")+
  theme (axis.title = element_text( face="bold", colour = "black"))
gg1
ggsave("D:/PROslide_RIO/Figs/monthly_occurrences.png", plot = gg1, width = 10, height = 10, dpi = 500)

################################################################################
################################################################################
################################################################################
################################################################################
# Calculate the median of DlyRnfl for each Year and Month
median_rainfall <- final_rioslides %>%
  group_by(Year, Month) %>%
  summarise(
    MedianDlyRnfl = round(median(DlyRnfl, na.rm = TRUE),1),
    MedianMxHrlyI = round(median(MxHrlyI, na.rm = TRUE),1),
    MedianAccm2dy = round(median(accm2dy, na.rm = TRUE),1),
    MedianAccm5dy = round(median(accm5dy, na.rm = TRUE),1),
    MedianAccm10d = round(median(accm10d, na.rm = TRUE),1),
    MedianAccm15d = round(median(accm15d, na.rm = TRUE),1)
  ) %>%
  ungroup() %>%
  st_drop_geometry() # Drop the geometry column


# Ensure there is an entry for every combination of month and year, filling with 0 where data is missing
median_rainfall <- merge(full_grid, median_rainfall, by = c("Year", "Month"), all.x = TRUE)
#median_rainfall$MedianDlyRnfl[is.na(median_rainfall$MedianDlyRnfl)] <- 0

# Convert Month to a factor to have proper ordering in the plot
median_rainfall$Month <- factor(median_rainfall$Month, levels = rev(sprintf("%02d", 1:12)), labels = rev(month.abb))
################################################################################
################################################################################
################################################################################
max_count <- max(median_rainfall$MedianDlyRnfl, na.rm = TRUE)
breaks <- seq(0, max_count, by = 50)

colors <- c("white", viridis(length(breaks) + 50, option = "mako"))

# Plot the heatmap
gg_daily1=ggplot(median_rainfall, aes(x = Year, y = Month, fill = MedianDlyRnfl)) +
  geom_tile(color = "lightgray") +
  geom_text(aes(label = MedianDlyRnfl), size=2.5, color = ifelse(median_rainfall$MedianDlyRnfl < 100, "black", "white"))+
             
  scale_fill_gradientn(
    colours = rev(colors),
    values = rescale(breaks), # Rescale the breaks to [0, 1]
    limits = c(0, max_count+3),
    breaks = breaks, # Set breaks
    labels = breaks, # Use breaks as labels
    name = "Rainfall\n(mm)"
  ) +
  theme_minimal() +
  labs(title = "Monthly Median of Daily\nRainfall Associated with Landslides",
       x = "Year",
       y = "Month",
       fill = "Median Daily Rainfall (mm)")+
  theme (axis.title = element_text( face="bold", colour = "black"), plot.title =element_text(size=12))


# Assuming you've calculated the breaks and colors for ggb
max_count_gga <- max(final_rioslides$DlyRnfl, na.rm = TRUE)

gg_daily2=final_rioslides %>%
  ggplot() +
  geom_sf(data = study_area, fill = "white", color = "black") +
  geom_sf(aes(color = DlyRnfl), alpha = 0.9, shape = 15) +
  scale_color_gradientn(
    colours = rev(colors),  # Ensure the same color palette is used
    values = rescale(breaks),  # Rescale the breaks to [0, 1]
    limits = c(0, max(final_rioslides$DlyRnfl, na.rm = TRUE)), 
    breaks = seq(0, max(final_rioslides$DlyRnfl, na.rm = TRUE), by = 100),  # Set breaks similar to ggb
    #labels = breaks,  # Use breaks as labels
    na.value = "red",  # Color for NA values
    name = "Daily Rainfall (mm)"
  ) +
  
  map_theme +
  labs(title = "Daily observed rainfall",
       color = "Daily Rainfall (mm)")
################################################################################
################################################################################
################################################################################
max_count <- max(median_rainfall$MedianMxHrlyI, na.rm = TRUE)
breaks <- seq(0, max_count, by = 10)

colors <- c("white", viridis(length(breaks) + 50, option = "mako"))

# Plot the heatmap
gg_hour1=ggplot(median_rainfall, aes(x = Year, y = Month, fill = MedianMxHrlyI)) +
  geom_tile(color = "lightgray") +
  geom_text(aes(label = MedianMxHrlyI), size=2.5, color = ifelse(median_rainfall$MedianMxHrlyI < 15, "black", "white"))+
             
  scale_fill_gradientn(
    colours = rev(colors),
    values = rescale(breaks), # Rescale the breaks to [0, 1]
    limits = c(0, max_count+3),
    breaks = breaks, # Set breaks
    labels = breaks, # Use breaks as labels
    name = "Rainfall\n(mm)"
  ) +
  theme_minimal() +
  labs(title = "Monthly Median of Maximum Hourly\nRainfall Intensity Associated with Landslides",
       x = "Year",
       y = "Month",
       fill = "Median Max Hourly Intensity (mm/h)")+
  theme (axis.title = element_text( face="bold", colour = "black"), plot.title =element_text(size=12))

################################################################################

max_count_gga <- max(final_rioslides$MxHrlyI, na.rm = TRUE)
gg_hour2=final_rioslides %>%
  ggplot() +
  geom_sf(data = study_area, fill = "white", color = "black") +
  geom_sf(aes(color = MxHrlyI), alpha = 0.9, shape = 15) +
  scale_color_gradientn(
    colours = rev(colors),  # Ensure the same color palette is used
    values = rescale(breaks),  # Rescale the breaks to [0, 1]
    limits = c(0, max(final_rioslides$MxHrlyI, na.rm = TRUE)), 
    breaks = seq(0, max(final_rioslides$MxHrlyI, na.rm = TRUE), by = 25),  # Set breaks similar to ggb
    #labels = breaks,  # Use breaks as labels
    na.value = "red",  # Color for NA values
    name = "Max Hourly\nIntensity (mm/h)"
  ) +
  
  map_theme +
  labs(title = "Max Hourly Intensity (mm/h)",
       color = "Max Hourly Intensity (mm/h)")
################################################################################
################################################################################
max_count <- max(median_rainfall$MedianAccm2dy, na.rm = TRUE)
breaks <- seq(0, max_count, by = 50)

colors <- c("white", viridis(length(breaks) + 50, option = "mako"))

# Plot the heatmap
gg_2day_a=ggplot(median_rainfall, aes(x = Year, y = Month, fill = MedianAccm2dy)) +
  geom_tile(color = "lightgray") +
  geom_text(aes(label = MedianAccm2dy), size=2.5, color = ifelse(median_rainfall$MedianAccm2dy < 100, "black", "white"))+
             
  scale_fill_gradientn(
    colours = rev(colors),
    values = rescale(breaks), # Rescale the breaks to [0, 1]
    limits = c(0, max_count+3),
    breaks = breaks, # Set breaks
    labels = breaks, # Use breaks as labels
    name = "Rainfall\n(mm)"
  ) +
  theme_minimal() +
  labs(title = "Monthly Median of 2-Day Accumulated\nRainfall Associated with Landslides",
       x = "Year",
       y = "Month",
       fill = "Median 2-Day Accumulated Rainfall (mm)")+
  theme (axis.title = element_text( face="bold", colour = "black"), plot.title =element_text(size=12))
################################################################################
max_count_gga <- max(final_rioslides$accm2dy, na.rm = TRUE)
gg_2day_b=final_rioslides %>%
  ggplot() +
  geom_sf(data = study_area, fill = "white", color = "black") +
  geom_sf(aes(color = accm2dy), alpha = 0.9, shape = 15) +
  scale_color_gradientn(
    colours = rev(colors),  # Ensure the same color palette is used
    values = rescale(breaks),  # Rescale the breaks to [0, 1]
    limits = c(0, max(final_rioslides$accm2dy, na.rm = TRUE)), 
    breaks = seq(0, max(final_rioslides$accm2dy, na.rm = TRUE), by = 100),  # Set breaks similar to ggb
    #labels = breaks,  # Use breaks as labels
    na.value = "red",  # Color for NA values
    name = "2-Day Accumulated\nRainfall (mm)"
  ) +
  
  map_theme +
  labs(title = "2-Day Accumulated Rainfall (mm)",
       color = "2-Day Accumulated Rainfall (mm)")
################################################################################
################################################################################
max_count <- max(median_rainfall$MedianAccm5dy, na.rm = TRUE)
breaks <- seq(0, max_count, by = 50)

colors <- c("white", viridis(length(breaks) + 50, option = "mako"))

# Plot the heatmap
gg_5day_a=ggplot(median_rainfall, aes(x = Year, y = Month, fill = MedianAccm5dy)) +
  geom_tile(color = "lightgray") +
  geom_text(aes(label = MedianAccm5dy), size=2.5, color = ifelse(median_rainfall$MedianAccm5dy < 150, "black", "white"))+
             
  scale_fill_gradientn(
    colours = rev(colors),
    values = rescale(breaks), # Rescale the breaks to [0, 1]
    limits = c(0, max_count+3),
    breaks = breaks, # Set breaks
    labels = breaks, # Use breaks as labels
    name = "Rainfall\n(mm)"
  ) +
  theme_minimal() +
  labs(title = "Monthly Median of 5-Day Accumulated\nRainfall Associated with Landslides",
       x = "Year",
       y = "Month",
       fill = "Median 5-Day Accumulated Rainfall (mm)")+
  theme (axis.title = element_text( face="bold", colour = "black"), plot.title =element_text(size=12))
################################################################################
################################################################################
max_count_gga <- max(final_rioslides$accm5dy, na.rm = TRUE)
gg_5day_b=final_rioslides %>%
  ggplot() +
  geom_sf(data = study_area, fill = "white", color = "black") +
  geom_sf(aes(color = accm5dy), alpha = 0.9, shape = 15) +
  scale_color_gradientn(
    colours = rev(colors),  # Ensure the same color palette is used
    values = rescale(breaks),  # Rescale the breaks to [0, 1]
    limits = c(0, max(final_rioslides$accm5dy, na.rm = TRUE)), 
    breaks = seq(0, max(final_rioslides$accm5dy, na.rm = TRUE), by = 100),  # Set breaks similar to ggb
    #labels = breaks,  # Use breaks as labels
    na.value = "red",  # Color for NA values
    name = "5-Day Accumulated\nRainfall (mm)"
  ) +
  
  map_theme +
  labs(title = "5-Day Accumulated Rainfall (mm)",
       color = "5-Day Accumulated Rainfall (mm)")
################################################################################
################################################################################
################################################################################
max_count <- max(median_rainfall$MedianAccm10d, na.rm = TRUE)
breaks <- seq(0, max_count, by = 50)

colors <- c("white", viridis(length(breaks) + 50, option = "mako"))

# Plot the heatmap
gg_10day_a=ggplot(median_rainfall, aes(x = Year, y = Month, fill = MedianAccm10d)) +
  geom_tile(color = "lightgray") +
  geom_text(aes(label = MedianAccm10d), size=2.5, color = ifelse(median_rainfall$MedianAccm10d < 170, "black", "white"))+
             
  scale_fill_gradientn(
    colours = rev(colors),
    values = rescale(breaks), # Rescale the breaks to [0, 1]
    limits = c(0, max_count+3),
    breaks = breaks, # Set breaks
    labels = breaks, # Use breaks as labels
    name = "Rainfall\n(mm)"
  ) +
  theme_minimal() +
  labs(title = "Monthly Median of 10-Day Accumulated\nRainfall Associated with Landslides",
       x = "Year",
       y = "Month",
       fill = "Median 10-Day Accumulated Rainfall (mm)")+
  theme(axis.title = element_text( face="bold", colour = "black"), plot.title =element_text(size=12))
################################################################################
max_count_gga <- max(final_rioslides$accm10d, na.rm = TRUE)
gg_10day_b=final_rioslides %>%
  ggplot() +
  geom_sf(data = study_area, fill = "white", color = "black") +
  geom_sf(aes(color = accm10d), alpha = 0.9, shape = 15) +
  scale_color_gradientn(
    colours = rev(colors),  # Ensure the same color palette is used
    values = rescale(breaks),  # Rescale the breaks to [0, 1]
    limits = c(0, max(final_rioslides$accm10d, na.rm = TRUE)), 
    breaks = seq(0, max(final_rioslides$accm10d, na.rm = TRUE), by = 100),  # Set breaks similar to ggb
    #labels = breaks,  # Use breaks as labels
    na.value = "red",  # Color for NA values
    name = "10-Day Accumulated\nRainfall (mm)"
  ) +
  
  map_theme +
  labs(title = "10-Day Accumulated Rainfall (mm)",
       color = "10-Day Accumulated Rainfall (mm)")

################################################################################
################################################################################
################################################################################
max_count <- max(median_rainfall$MedianAccm15d, na.rm = TRUE)
breaks <- seq(0, max_count, by = 50)

colors <- c("white", viridis(length(breaks) + 50, option = "mako"))

# Plot the heatmap
gg_15day_a=ggplot(median_rainfall, aes(x = Year, y = Month, fill = MedianAccm15d)) +
  geom_tile(color = "lightgray") +
  geom_text(aes(label = MedianAccm15d), size=2.5, color = ifelse(median_rainfall$MedianAccm15d < 200, "black", "white"))+
             
  scale_fill_gradientn(
    colours = rev(colors),
    values = rescale(breaks), # Rescale the breaks to [0, 1]
    limits = c(0, max_count+3),
    breaks = breaks, # Set breaks
    labels = breaks, # Use breaks as labels
    name = "Rainfall\n(mm)"
  ) +
  theme_minimal() +
  labs(title = "Monthly Median of 15-Day Accumulated\nRainfall Associated with Landslides",
       x = "Year",
       y = "Month",
       fill = "Median 15-Day Accumulated Rainfall (mm)")+
  theme (axis.title = element_text( face="bold", colour = "black"), plot.title =element_text(size=12))
################################################################################
max_count_gga <- max(final_rioslides$accm15d, na.rm = TRUE)
gg_15day_b=final_rioslides %>%
  ggplot() +
  geom_sf(data = study_area, fill = "white", color = "black") +
  geom_sf(aes(color = accm15d), alpha = 0.9, shape = 15) +
  scale_color_gradientn(
    colours = rev(colors),  # Ensure the same color palette is used
    values = rescale(breaks),  # Rescale the breaks to [0, 1]
    limits = c(0, max(final_rioslides$accm15d, na.rm = TRUE)), 
    breaks = seq(0, max(final_rioslides$accm15d, na.rm = TRUE), by = 100),  # Set breaks similar to ggb
    #labels = breaks,  # Use breaks as labels
    na.value = "red",  # Color for NA values
    name = "15-Day Accumulated\nRainfall (mm)"
  ) +
  map_theme +
  labs(title = "15-Day Accumulated Rainfall (mm)",
       color = "15-Day Accumulated Rainfall (mm)")






################################################################################
################################################################################
################################################################################
################################################################################

library(cowplot)
# Arrange the plots in a single grid
plot_grid_list <- list(gg_hour1, #gg_hour2, 
                       gg_daily1, #gg_daily2,
                       gg_2day_a,# gg_2day_b, 
                       gg_5day_a, #gg_5day_b, 
                       gg_10day_a, #gg_10day_b, 
                       gg_15day_a#, gg_15day_b
)

# Combine the list of plots into a grid
combined_plot <- plot_grid(plotlist = plot_grid_list, ncol = 3,labels = c("A)", "B)","C)", "D)","E)", "F)"))
combined_plot
# Save the combined plot to a file
ggsave("D:/PROslide_RIO/Figs/combined_plots.tiff", combined_plot, width = 30, height = 20, units = "cm")



################################################################################
################################################################################
################################################################################
################################################################################

# Arrange the plots in a single grid
plot_grid_list2 <- list(gg_hour2, 
                       gg_daily2,
                       gg_2day_b, 
                       gg_5day_b, 
                       gg_10day_b, 
                       gg_15day_b
)

# Combine the list of plots into a grid
combined_plot2 <- plot_grid(plotlist = plot_grid_list2, ncol = 3,
                            labels = c("A)", "B)","C)", "D)","E)", "F)"),
                            label_size = 12, # Adjust size as needed
                            label_x = -0.01, # X position of the labels (0.5 for center)
                            label_y = 0.98) 
combined_plot2

# Save the combined plot to a file
ggsave("D:/PROslide_RIO/Figs/combined_plots2.tiff", combined_plot2, width = 30, height = 15, units = "cm")


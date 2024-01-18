library(ggplot2)
library(sf)
library(viridis)
library(tidyr) 
library(dplyr)
library(scales)


# Read the shapefiles
final_rioslides <- st_read("D:/PROslide_RIO/DATA2/final_rioslides.shp")
study_area <- st_read("D:/PROslide_RIO/DATA/StudyArea.shp")


map_theme= theme(legend.title =  element_text(size = 17),
                 legend.position = c(0.3, .9),
                 legend.direction = "horizontal",  # Make the color legend horizontal
                 legend.box = "horizontal",
                 axis.text = element_blank(),
                 axis.title = element_blank(),
                 axis.ticks = element_blank(),
                 axis.line = element_blank(),legend.text = element_text(size = 12),  # Adjust the legend text size
                 legend.key.width = unit(2, "cm"),  # Increase the legend key width
                 legend.key.height = unit(1, "cm")  # Increase the legend key height
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
ggplot(monthly_counts, aes(x = Year, y = Month, fill = Count)) +
  geom_tile(color = "white") + # Add borders to the tiles
  geom_label(aes(label = Count),  color = "black", 
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
       fill = "Count")

################################################################################
################################################################################
################################################################################
################################################################################
# Calculate the median of DlyRnfl for each Year and Month
median_rainfall <- final_rioslides %>%
  group_by(Year, Month) %>%
  summarise(
    MedianDlyRnfl = median(DlyRnfl, na.rm = TRUE),
    MedianMxHrlyI = median(MxHrlyI, na.rm = TRUE),
    MedianAccm2dy = median(accm2dy, na.rm = TRUE),
    MedianAccm5dy = median(accm5dy, na.rm = TRUE),
    MedianAccm10d = median(accm10d, na.rm = TRUE),
    MedianAccm15d = median(accm15d, na.rm = TRUE)
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
ggplot(median_rainfall, aes(x = Year, y = Month, fill = MedianDlyRnfl)) +
  geom_tile(color = "lightgray") +
  geom_label(aes(label = MedianDlyRnfl),fill = "white",
             label.size = NA)+ # Remove the border around the label
  scale_fill_gradientn(
    colours = rev(colors),
    values = rescale(breaks), # Rescale the breaks to [0, 1]
    limits = c(0, max_count+3),
    breaks = breaks, # Set breaks
    labels = breaks, # Use breaks as labels
    name = "Rainfall (mm)"
  ) +
  theme_minimal() +
  labs(title = "Monthly Median of Daily Rainfall Associated with Landslides",
       x = "Year",
       y = "Month",
       fill = "Median Daily Rainfall (mm)")


# Assuming you've calculated the breaks and colors for ggb
max_count_gga <- max(final_rioslides$DlyRnfl, na.rm = TRUE)

final_rioslides %>%
  ggplot() +
  geom_sf(aes(color = DlyRnfl, size = DlyRnfl), alpha = 0.9, shape = 15) +
  scale_color_gradientn(
    colours = rev(colors),  # Ensure the same color palette is used
    values = rescale(breaks),  # Rescale the breaks to [0, 1]
    limits = c(0, max(final_rioslides$DlyRnfl, na.rm = TRUE)), 
    breaks = seq(0, max(final_rioslides$DlyRnfl, na.rm = TRUE), by = 100),  # Set breaks similar to ggb
    #labels = breaks,  # Use breaks as labels
    na.value = "red",  # Color for NA values
    name = "Daily Rainfall (mm)"
  ) +
  scale_size_continuous(
    range = c(1, 3),  # Adjust maximum size here
    guide = 'none'
  ) +
  geom_sf(data = study_area, fill = NA, color = "black") +
  map_theme +
  labs(color = "Daily Rainfall (mm)")
################################################################################
################################################################################
################################################################################
max_count <- max(median_rainfall$MedianMxHrlyI, na.rm = TRUE)
breaks <- seq(0, max_count, by = 10)

colors <- c("white", viridis(length(breaks) + 50, option = "mako"))

# Plot the heatmap
ggplot(median_rainfall, aes(x = Year, y = Month, fill = MedianMxHrlyI)) +
  geom_tile(color = "lightgray") +
  geom_label(aes(label = MedianMxHrlyI),fill = "white",
             label.size = NA)+ # Remove the border around the label
  scale_fill_gradientn(
    colours = rev(colors),
    values = rescale(breaks), # Rescale the breaks to [0, 1]
    limits = c(0, max_count+3),
    breaks = breaks, # Set breaks
    labels = breaks, # Use breaks as labels
    name = "Rainfall (mm)"
  ) +
  theme_minimal() +
  labs(title = "Monthly Median of Maximum Hourly Rainfall Intensity Associated with Landslides",
       x = "Year",
       y = "Month",
       fill = "Median Max Hourly Intensity (mm/h)")

################################################################################
################################################################################
################################################################################
max_count <- max(median_rainfall$MedianAccm2dy, na.rm = TRUE)
breaks <- seq(0, max_count, by = 50)

colors <- c("white", viridis(length(breaks) + 50, option = "mako"))

# Plot the heatmap
ggplot(median_rainfall, aes(x = Year, y = Month, fill = MedianAccm2dy)) +
  geom_tile(color = "lightgray") +
  geom_label(aes(label = MedianAccm2dy),fill = "white",
             label.size = NA)+ # Remove the border around the label
  scale_fill_gradientn(
    colours = rev(colors),
    values = rescale(breaks), # Rescale the breaks to [0, 1]
    limits = c(0, max_count+3),
    breaks = breaks, # Set breaks
    labels = breaks, # Use breaks as labels
    name = "Rainfall (mm)"
  ) +
  theme_minimal() +
  labs(title = "Monthly Median of 2-Day Accumulated Rainfall Associated with Landslides",
       x = "Year",
       y = "Month",
       fill = "Median 2-Day Accumulated Rainfall (mm)")
################################################################################
################################################################################
################################################################################
max_count <- max(median_rainfall$MedianAccm5dy, na.rm = TRUE)
breaks <- seq(0, max_count, by = 50)

colors <- c("white", viridis(length(breaks) + 50, option = "mako"))

# Plot the heatmap
ggplot(median_rainfall, aes(x = Year, y = Month, fill = MedianAccm5dy)) +
  geom_tile(color = "lightgray") +
  geom_label(aes(label = MedianAccm5dy),fill = "white",
             label.size = NA)+ # Remove the border around the label
  scale_fill_gradientn(
    colours = rev(colors),
    values = rescale(breaks), # Rescale the breaks to [0, 1]
    limits = c(0, max_count+3),
    breaks = breaks, # Set breaks
    labels = breaks, # Use breaks as labels
    name = "Rainfall (mm)"
  ) +
  theme_minimal() +
  labs(title = "Monthly Median of 5-Day Accumulated Rainfall Associated with Landslides",
       x = "Year",
       y = "Month",
       fill = "Median 5-Day Accumulated Rainfall (mm)")
################################################################################
################################################################################
################################################################################
max_count <- max(median_rainfall$MedianAccm10d, na.rm = TRUE)
breaks <- seq(0, max_count, by = 50)

colors <- c("white", viridis(length(breaks) + 50, option = "mako"))

# Plot the heatmap
ggplot(median_rainfall, aes(x = Year, y = Month, fill = MedianAccm10d)) +
  geom_tile(color = "lightgray") +
  geom_label(aes(label = MedianAccm10d),fill = "white",
             label.size = NA)+ # Remove the border around the label
  scale_fill_gradientn(
    colours = rev(colors),
    values = rescale(breaks), # Rescale the breaks to [0, 1]
    limits = c(0, max_count+3),
    breaks = breaks, # Set breaks
    labels = breaks, # Use breaks as labels
    name = "Rainfall (mm)"
  ) +
  theme_minimal() +
  labs(title = "Monthly Median of 10-Day Accumulated Rainfall Associated with Landslides",
       x = "Year",
       y = "Month",
       fill = "Median 10-Day Accumulated Rainfall (mm)")
################################################################################
################################################################################
################################################################################
max_count <- max(median_rainfall$MedianAccm15d, na.rm = TRUE)
breaks <- seq(0, max_count, by = 50)

colors <- c("white", viridis(length(breaks) + 50, option = "mako"))

# Plot the heatmap
ggplot(median_rainfall, aes(x = Year, y = Month, fill = MedianAccm15d)) +
  geom_tile(color = "lightgray") +
  geom_label(aes(label = MedianAccm15d),fill = "white",
             label.size = NA)+ # Remove the border around the label
  scale_fill_gradientn(
    colours = rev(colors),
    values = rescale(breaks), # Rescale the breaks to [0, 1]
    limits = c(0, max_count+3),
    breaks = breaks, # Set breaks
    labels = breaks, # Use breaks as labels
    name = "Rainfall (mm)"
  ) +
  theme_minimal() +
  labs(title = "Monthly Median of 15-Day Accumulated Rainfall Associated with Landslides",
       x = "Year",
       y = "Month",
       fill = "Median 15-Day Accumulated Rainfall (mm)")

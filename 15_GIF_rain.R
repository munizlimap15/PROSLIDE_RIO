# Load necessary libraries
library(sf)
library(raster)
library(ggplot2)
library(viridis)
library(dplyr)
library(sp)
library(gstat)

# Set your working directories
landslide_data_directory <- "D:/PROslide_RIO/DATA2/"
rainfall_raster_directory <- "D:/PROslide_RIO/DATA2/rain_rasters4/"
plot_output_directory <- "D:/PROslide_RIO/Figs/PLOTS/"

# Read landslide data

rioslides <- sf::st_read(paste0(landslide_data_directory, "final_rioslides.shp"))
landslide_dates <- unique(rioslides$data)
landslide_dates <- na.omit(landslide_dates)
landslide_dates <- as.Date(landslide_dates, origin = "1970-01-01")
landslide_dates <- na.omit(landslide_dates)
landslide_dates <- landslide_dates[landslide_dates != as.Date("3748-02-08")]

# Template raster to guarantee perfect fit
template_raster <- raster(paste0(landslide_data_directory, "dtm.asc"))
template_raster <- template_raster * 0
template_raster <- aggregate(template_raster, fact = 20)
template_points <- rasterToPoints(template_raster)
template_sf <- st_as_sf(as.data.frame(template_points), coords = c("x", "y"), crs = crs(template_raster))

# Define the parameters you want to interpolate
#parameters <- c("DailyRainfall", "MaxHourlyIntensity", "accum2days", "accum5days", "accum10days", "accum15days")

current_date <- "2009-01-30"  # Replace with your actual date variable



plot_rainfall_landslides <- function(current_date) {
  raster_filepath <- file.path(rainfall_raster_directory, paste0("combined_", current_date, ".tif"))
  rainfall <- raster(raster_filepath)
  
  # Transform the CRS of the landslide data to match the rainfall raster
  rioslides <- st_transform(rioslides, crs = st_crs(rainfall))
  
  # Select landslides that occurred on the current date
  landslides_today <- rioslides %>% dplyr::filter(data == current_date)
  
  # Convert the raster data to a dataframe for ggplot
  rainfall_df <- as.data.frame(rainfall, xy = TRUE)
  names(rainfall_df) <- c("x", "y", "rainfall")
  rainfall_df$rainfall[rainfall_df$rainfall == 0] <- NA
  
  rainfall_df$rainfall_cat <- cut(
    rainfall_df$rainfall,
    breaks = c(seq(0, 100, by = 10), Inf),  # Breaks at every 10 units, with an open-ended bin for >100
    labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100", ">100"),
    right = FALSE,  # Indicates that intervals should be closed on the right and open on the left
    include.lowest = TRUE
  )
  
  # Create the plot
  p <- ggplot() +
    geom_raster(data = rainfall_df, aes(x = x, y = y, fill = rainfall_cat)) +  # Use rainfall_cat for coloring
    scale_fill_viridis_d(
      na.value = "transparent",
      guide = guide_legend(title = "Rainfall (mm)", title.position = "top")
    ) +
    geom_sf(data = landslides_today, color = "red", size = 1) +
    labs(
      title = "Rainfall and Landslide Locations",
      subtitle = as.character(current_date),
      fill = "Rainfall"
    ) +
    coord_sf() +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom",
      plot.margin = margin(1, 1, 1, 1, "cm"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
  
  # Save the plot
  ggsave(filename = paste0(plot_output_directory, "plot_", current_date, ".png"), plot = p, width = 10, height = 8)
  
  return(p)
}

# Generate and save plots for each date
for (date in landslide_dates) {
  print(paste("Processing date:", date))
  plot_rainfall_landslides(date)
}

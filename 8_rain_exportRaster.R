#https://rstudio-pubs-static.s3.amazonaws.com/618389_ebbc8a7a05c4467ebd68eea7937a863e.html
#http://www.sistema-alerta-rio.com.br/dados-meteorologicos/maiores-chuvas/
#http://www.sistema-alerta-rio.com.br/dados-meteorologicos/download/dados-pluviometricos/
library(sf)
library(dplyr)
library(raster)
library(gstat)
library(lubridate)

# Read the shapefile
shapefile_path <- "E:/PROslide_RIO/DATA/stations.shp"
stations_sf <- st_read(shapefile_path)
names(stations_sf)[names(stations_sf) == "Estação"] <- "Station"
load("E:/PROslide_RIO/DATA2/daily_totals2.Rd") 


del= subset(daily_totals, Date == as.Date("2009-12-31")) 
summary(del$MaxHourlyIntensity)
#anchieta_june_data <- daily_totals %>%
  #filter(Station == "Anchieta", month(Date) == 6, year(Date) == 2009)

####################################################
# Give the spatial component to the data
daily_totals_sf <- daily_totals %>%
  left_join(stations_sf, by = c("Station" = "Station"))



####################################################
# Template raster to guarantee perfect fit
template_raster <- raster("E:/PROslide_RIO/DATA2/dtm.asc")
template_raster = template_raster * 0
# Aggregating the raster to 100m resolution
template_raster <- aggregate(template_raster, fact = 20)
# Convert the raster to points
template_points <- rasterToPoints(template_raster)
# Convert the points to an sf object
template_sf <- st_as_sf(as.data.frame(template_points), coords = c("x", "y"), crs = crs(template_raster))


# Get the unique dates for which you have data
#unique_dates <- unique(daily_totals$Date)
rioslides = sf::st_read("C:/Users/pedro/Documents/PROslide_RIO/DATA/landslides_2023.shp")
landslide_dates <- unique(rioslides$data)
landslide_dates <- na.omit(landslide_dates)
landslide_dates <- as.Date(landslide_dates, origin = "1970-01-01")
# Remove NA dates (which include the invalid date)
landslide_dates <- na.omit(landslide_dates)
# Filter out the specific unwanted date
landslide_dates <- landslide_dates[landslide_dates != as.Date("3748-02-08")]
#landslide_dates <- landslide_dates[landslide_dates == as.Date("2009-12-31")]
# Define the parameters you want to interpolate
parameters <- c("DailyRainfall", "MaxHourlyIntensity", "accum2days", "accum5days", "accum10days", "accum15days")

####################################################
# Create the dir to receive the raster files
#dir.create("E:/PROslide_RIO/DATA2/rain_rasters3/", recursive = TRUE)
raster_output_directory = "E:/PROslide_RIO/DATA2/rain_rasters4/"

####################################################
# Determine same CRS
if ( sf::st_crs(stations_sf) !=  raster::crs(template_raster)) {
  stations_sf <- sf::st_transform(stations_sf, raster::crs(template_raster))
}



####################################################
####################################################
####################################################
for (date in landslide_dates) {
  date = as.Date(date, origin = "1970-01-01")
  
  # Initialize a raster stack for this date
  raster_stack <- stack(lapply(1:length(parameters), function(x) template_raster))
  
  # Loop over each parameter
  for (i in seq_along(parameters)) {
    param <- parameters[i]
    
    rainfall_data <- daily_totals_sf %>%
      dplyr::ungroup() %>%
      dplyr::filter(Date == date) %>%
      dplyr::select(Station, Date, !!rlang::sym(param)) %>%
      na.omit()
    # Merge rainfall data with station locations
    rainfall_data_sp <- merge(stations_sf, rainfall_data, by = "Station")
    
    # The power value (often denoted as() controls how the influence of sampled points decreases with distance. 
    # A higher power value means that closer points will have much more influence on the interpolated value than points farther away. 
    # Higher values of p: Lead to an even more localized influence of each data point, 
    # making the interpolation increasingly sensitive to nearby points and potentially leading to more pronounced local variations in the interpolated surface.
    idw_output <- idw(formula = as.formula(paste(param, "~ 1")), 
                      locations = rainfall_data_sp, newdata = template_sf,
                      idp = 4)
    
    # Convert IDW output to RasterLayer
    raster_output <- raster(resolution = 50, crs = "+proj=utm +zone=23 +south +ellps=GRS80 +units=m +no_defs", 
                            xmn = 623157.7, xmx = 695657.7, ymn = 7445838, ymx = 7484138) %>% 
      rasterize(idw_output, ., field = "var1.pred", fun = "max", background = 0)
    
    # Insert the IDW output into the appropriate layer of the raster stack
    raster_layer <- rasterize(idw_output, template_raster, field = "var1.pred", fun = "max", background = 0)
    raster_stack[[i]] <- raster_layer
  }
  
  # Name layers of the stack
  names(raster_stack) <- parameters
  print(date)
  # Save the raster stack for this date
  output_filepath <- file.path(raster_output_directory, paste0("combined_", date, ".tif"))
  writeRaster(raster_stack, filename = output_filepath, format = "GTiff", overwrite = TRUE)
}



#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################




# 
# library(sf)
# library(dplyr)
# library(raster)
# library(gstat)
# 
# # Read the shapefile
# shapefile_path <- "E:/PROslide_RIO/DATA/stations.shp"
# stations_sf <- st_read(shapefile_path)
# names(stations_sf)[names(stations_sf) == "Estação"] <- "Station"
# load("E:/PROslide_RIO/DATA2/daily_totals.Rd") 
# 
# ####################################################
# # Give the spatial component to the data
# daily_totals_sf <- daily_totals %>%
#   left_join(stations_sf, by = c("Station" = "Station"))
# 
# 
# ####################################################
# # Template raster to guarantee perfect fit
# template_raster <- raster("E:/PROslide_RIO/DATA2/dtm.asc")
# template_raster = template_raster * 0
# # Aggregating the raster to 100m resolution
# template_raster <- aggregate(template_raster, fact = 20)
# # Convert the raster to points
# template_points <- rasterToPoints(template_raster)
# # Convert the points to an sf object
# template_sf <- st_as_sf(as.data.frame(template_points), coords = c("x", "y"), crs = crs(template_raster))
# 
# 
# # Get the unique dates for which you have data
# #unique_dates <- unique(daily_totals$Date)
# rioslides = sf::st_read("C:/Users/pedro/Documents/PROslide_RIO/DATA/landslides_2023.shp")
# landslide_dates <- unique(rioslides$data)
# landslide_dates <- na.omit(landslide_dates)
# landslide_dates <- as.Date(landslide_dates, origin = "1970-01-01")
# 
# 
# # Define the parameters you want to interpolate
# parameters <- c("DailyRainfall", "MaxHourlyIntensity", "accum2days", "accum5days", "accum10days", "accum15days")
# 
# ####################################################
# # Create the dir to recieve the raster files
# dir.create("E:/PROslide_RIO/DATA2/rain_rasters/", recursive = TRUE)
# raster_output_directory = "E:/PROslide_RIO/DATA2/rain_rasters/"
# 
# ####################################################
# # Determine same CRS
# if ( sf::st_crs(stations_sf) !=  raster::crs(template_raster)) {
#   stations_sf <- sf::st_transform(stations_sf, raster::crs(template_raster))
# }
# 
# ####################################################
# ####################################################
# ####################################################
# # # Loop over each date and parameter
# # for (date in landslide_dates) {
# #   date=as.Date(date, origin = "1970-01-01")
# #   for (param in parameters) {
# #     # Extract data for a single day and parameter
# #     rainfall_data <- daily_totals_sf %>%
# #       dplyr::ungroup() %>%
# #       dplyr::filter(Date == date) %>%
# #       dplyr::select(Station, Date, !!rlang::sym(param)) %>%
# #       na.omit()
# #     
# #     # Merge rainfall data with station locations
# #     rainfall_data_sp <- merge(stations_sf, rainfall_data, by = "Station")
# #     
# #     # Perform IDW interpolation for the chosen day and parameter
# #     # Use the template raster's grid
# #     idw_output <- idw(formula = as.formula(paste(param, "~ 1")), 
# #                       locations = rainfall_data_sp, newdata = template_sf)
# #     
# #     # Convert IDW output to RasterLayer
# #     raster_output <- raster(resolution = 100, crs = "+proj=utm +zone=23 +south +ellps=GRS80 +units=m +no_defs", 
# #                               xmn = 623157.7, xmx = 695657.7, ymn = 7445838, ymx = 7484138) %>% 
# #       rasterize(idw_output, ., field = "var1.pred", fun = "max", background = 0)
# #     
# #     #formatted_date <- format(date, "%Y-%m-%d")
# #     print(date)
# #     # Save the raster
# #     output_filepath <- file.path(raster_output_directory, paste0(param, "_", date,  ".tif"))
# #     writeRaster(raster_output, filename = output_filepath, format = "GTiff", overwrite = TRUE)
# #   }
# # }
# # 
# # 




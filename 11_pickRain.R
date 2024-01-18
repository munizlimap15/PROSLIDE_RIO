library(sf)
library(raster)
library(dplyr)

# Load landslide data (replace with the correct path and file)
rioslides = sf::st_read("D:/PROslide_RIO/DATA/landslides_2023.shp")
rioslides <- rioslides %>% 
  filter(!st_is_empty(geometry))

#rioslides =subset(rioslides, data==as.Date("2010-04-06"))
# Function to extract values from multi-band raster
extract_multiband <- function(raster_stack, points) {
  extracted_values <- lapply(1:nlayers(raster_stack), function(i) {
    raster::extract(raster_stack[[i]], points)
  })
  return(do.call(cbind, extracted_values))
}

# Initialize a dataframe to store the extracted values
rioslides = rioslides %>% mutate(ID = row_number())
landslide_raster_values <- data.frame(ID = rioslides$ID) # Assuming each landslide has a unique ID
#rioslides= sample_n(rioslides, 100)

##########################################
landslide_dates <- unique(rioslides$data)
landslide_dates <- as.Date(landslide_dates, origin = "1970-01-01")
# Remove NA dates (which include the invalid date)
landslide_dates <- na.omit(landslide_dates)
# Filter out the specific unwanted date
landslide_dates <- landslide_dates[landslide_dates != as.Date("3748-02-08")]

##########################################

raster_output_directory = "D:/PROslide_RIO/DATA2/rain_rasters4/"

parameters <- c("DailyRainfall", "MaxHourlyIntensity", "accum2days", "accum5days", "accum10days", "accum15days")

# Convert sf object to SpatialPointsDataFrame
rioslides_sp <- as(rioslides, "Spatial")


# Loop over each raster file
for (date in unique(landslide_dates)) {
  #date="2010-04-06"
  date = as.Date(date, origin = "1970-01-01")
  
  slides_today_sp = subset(rioslides_sp, data==as.Date(date))
  ids <- slides_today_sp$ID
  raster_file_path <- file.path(raster_output_directory, paste0("combined_", date, ".tif"))
  
  # Load the multi-band raster for this date
  raster_stack <- stack(raster_file_path)
  
  # Extract values for all bands at landslide locations
  extracted_values <- extract_multiband(raster_stack, slides_today_sp)
  
  date_as_character <- as.character(as.Date(date, origin = "1970-01-01"))
  combined_data_name <- paste0("combined_data_", date_as_character)
  
  combined_data <- cbind(ID = ids, extracted_values) %>% as.data.frame()
  assign(combined_data_name, combined_data)
  
  head(combined_data)
  #landslide_raster_values<- as.data.frame(extracted_values) %>% mutate (ID=i)
}


# Pattern to match the data frames
pattern <- "^combined_data_"

# List of all data frames that match the pattern
data_frame_names <- ls(pattern = pattern)

list_of_data_frames <- lapply(data_frame_names, function(name) get(name))

# Combine all data frames into one
combined_all_data <- do.call(rbind, list_of_data_frames)
head(combined_all_data)
# Merging with original landslide data
final_rioslides <- merge(rioslides, combined_all_data, by = "ID")

rm(list = data_frame_names)
###############################

final_rioslides <- final_rioslides %>% rename(DailyRainfall = V2, 
                                              MaxHourlyIntensity = V3, 
                                              accum2days  = V4,
                                              accum5days  = V5, 
                                              accum10days = V6, 
                                              accum15days = V7)



head(final_rioslides)
test <- subset(final_rioslides, DailyRainfall == 0)
summary(as.factor(test$data))

################################################################################
################################################################################
################################################################################
# Convert the columns to numeric and round to 1 decimal place
final_rioslides$DailyRainfall      <- round(as.numeric(final_rioslides$DailyRainfall), 1)
final_rioslides$MaxHourlyIntensity <- round(as.numeric(final_rioslides$MaxHourlyIntensity), 1)
final_rioslides$accum2days         <- round(as.numeric(final_rioslides$accum2days), 1)
final_rioslides$accum5days         <- round(as.numeric(final_rioslides$accum5days), 1)
final_rioslides$accum10days        <- round(as.numeric(final_rioslides$accum10days), 1)
final_rioslides$accum15days        <- round(as.numeric(final_rioslides$accum15days), 1)

summary(final_rioslides$DailyRainfall)
summary(final_rioslides$MaxHourlyIntensity)
summary(final_rioslides$accum2days)
summary(final_rioslides$accum5days)
summary(final_rioslides$accum10days)
summary(final_rioslides$accum15days)

# # Subsetting the final_rioslides to include only the specified variables
# final_rioslides_subset <- final_rioslides[c("DailyRainfall", "MaxHourlyIntensity", "accum2days", 
#                                             "accum5days", "accum10days", "accum15days", 
#                                             "geometry", "data")]
st_write(final_rioslides, 'D:/PROslide_RIO/DATA2/final_rioslides.shp')




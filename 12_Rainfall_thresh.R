# Load necessary libraries
library(dplyr)
library(lubridate)
library(zoo)
library(sf)
library(stars)
library(tidyr)

load("D:/PROslide_RIO/DATA2/all_data2.Rd") 

all_data$Date <- as.Date(all_data$DateTime)







# First, ensure the data is in chronological order
all_data <- all_data %>%
  arrange(Station, DateTime)

# Identify the start of new rainfall events
all_data <- all_data %>%
  mutate(EventStart = Rainfall_15min > 0 & lag(Rainfall_15min <= 0, default = TRUE))

# Create a cumulative sum to flag each event with an ID
all_data <- all_data %>%
  group_by(Station) %>%
  mutate(EventID = cumsum(EventStart)) %>%
  ungroup()

# Now, filter the events, keeping only rows where Rainfall_15min > 0
rainy_events <- all_data %>%
  filter(Rainfall_15min > 0)
# Calculate the duration and total rainfall of each event
rainfall_events <- rainy_events %>%
  group_by(Station, EventID) %>%
  summarize(
    Start = min(DateTime),
    End = max(DateTime),
    Duration = if_else(Start == End, 15, as.numeric(difftime(End, Start, units = "mins")) + 15),
    TotalRainfall = sum(Rainfall_15min, na.rm = TRUE),
    .groups = 'drop'
  )

rainfall_events <- rainfall_events %>%
  filter(Duration <= 1440, # Duration of no more than 1 day
         TotalRainfall <= 1500, # Total rainfall of no more than 1500
         !is.na(EventID), # Exclude missing EventID
         !is.na(Start), # Exclude missing Start
         !is.na(End)) # Exclude missing End


# Function to calculate MaxHourlyIntensity for a given event
calculate_max_hourly_intensity_for_event <- function(event_id, station, data) {
  event_data <- filter(data, EventID == event_id, Station == station)
  if (nrow(event_data) == 0 || all(is.na(event_data$Rainfall_15min))) {
    return(0)  # Return 0 if there is no data
  } else {
    # Group data by hour
    event_data$Hour <- as.POSIXct(format(event_data$DateTime, "%Y-%m-%d %H:00:00"))
    # Sum rainfall for each hour
    hourly_sum <- aggregate(Rainfall_15min ~ Hour, data = event_data, sum, na.rm = TRUE)
    # Find the maximum hourly sum
    max_hourly_intensity <- max(hourly_sum$Rainfall_15min, na.rm = TRUE)
    return(max_hourly_intensity)
  }
}


# Function to calculate accumulated rainfall for previous n days for a given event
calculate_accumulated_rainfall_for_event <- function(event_start, station, data, days) {
  start_period <- event_start - days * 24*60*60  # Start of the period (n days before the event)
  end_period <- event_start - 1  # End of the period (just before the event starts)
  
  # Filter data for the specific period and station, then sum the rainfall
  filtered_data <- data %>% filter(Station == station)
  rainfall_sum <- sum(filtered_data$Rainfall_15min[filtered_data$DateTime >= start_period & filtered_data$DateTime < end_period], na.rm = TRUE)
  
  return(rainfall_sum)
}


# Add the new calculations to the rainfall_events data frame
rainfall_events <- rainfall_events %>%
  rowwise() %>%
  mutate(
    MaxHourlyIntensity = calculate_max_hourly_intensity_for_event(EventID, Station, all_data),
    accum2days = calculate_accumulated_rainfall_for_event(Start, Station, all_data, 2),
    accum5days = calculate_accumulated_rainfall_for_event(Start, Station, all_data, 5),
    accum10days = calculate_accumulated_rainfall_for_event(Start, Station, all_data, 10),
    accum15days = calculate_accumulated_rainfall_for_event(Start, Station, all_data, 15)
  ) %>%
  ungroup()


head(rainfall_events)

save(rainfall_events, file = "D:/PROslide_RIO/DATA2/rainfall_events.Rd")
#######################################################
#######################################################
#######################################################

load("D:/PROslide_RIO/DATA2/rainfall_events.Rd")
rioslides = sf::st_read("D:/PROslide_RIO/DATA/landslides_2023.shp")
stations_sf <- sf::st_read("D:/PROslide_RIO/DATA/stations.shp")
# Preparing the rioslides dataset
rioslides <- rioslides %>% 
  filter(!is.na(data)) %>%
  mutate(data = as.Date(data))


# Preparing the rainfall_events dataset
rainfall_events <- rainfall_events %>%
  left_join(stations_sf, by = c("Station" = "Estação")) %>%
  st_as_sf() %>%
  st_transform(st_crs(rioslides))

# Spatial join to find the nearest rainfall station for each landslide
nearest_stations_indices <- st_nearest_feature(rioslides, rainfall_events)
rioslides$nearest_station <- rainfall_events$Station[nearest_stations_indices]

rainfall_events$Start2 <- as.Date(rainfall_events$Start)


# Merge landslides with rainfall events based on station name and same day
merged_data <- merge(as.data.frame(rioslides), as.data.frame(rainfall_events),
                     by.x = c("nearest_station", "data"), by.y = c("Station", "Start2"))

nrow(merged_data)














# Merge landslide data with rainfall data based on nearest station and date
joined_data <- rioslides %>%
  rowwise() %>%
  mutate(
    matched_rainfall_event = list(
      rainfall_events %>%
        filter(
          Station == nearest_station,
          as.Date(Start) <= data + 1,  # Include rainfall events that ended one day after the landslide
          as.Date(End) + 1 >= data  # Include rainfall events that started one day before the landslide
        ) %>%
        # Rename the geometry column to avoid duplication
        rename(rainfall_geometry = geometry)
    )
  ) %>%
  tidyr::unnest(matched_rainfall_event)

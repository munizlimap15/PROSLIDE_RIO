library(dplyr)
library(readr)
library(stringr)  # Load the stringr package
library(lubridate)
library(uuid)

# Function to parse a single line
parse_line <- function(line, station_name) {
  line_parts <- unlist(strsplit(line, "\\s+"))
  num_parts <- length(line_parts)
  
  # Assuming that when HBV is present, there should be 8 parts
  # (Date, Time, HBV, 15 min, 01 h, 04 h, 24 h, 96 h)
  if (num_parts == 8) {
    return(data.frame(
      Date = line_parts[1],
      Time = line_parts[2],
      HBV = line_parts[3],  # Now HBV is correctly assigned
      Rainfall_15min = as.numeric(line_parts[4]),
      Rainfall_1h = as.numeric(line_parts[5]),
      Station = station_name
    ))
  } else if (num_parts == 7) {  # When HBV is missing
    return(data.frame(
      Date = line_parts[1],
      Time = line_parts[2],
      HBV = NA,  # Placeholder for missing HBV
      Rainfall_15min = as.numeric(line_parts[3]),  # Adjusting index
      Rainfall_1h = as.numeric(line_parts[4]),  # Adjusting index
      Station = station_name
    ))
  } else {
    return(NULL)  # In case of an unexpected format
  }
}


process_file <- function(file) {
  station_name_line <- readLines(file, n = 1)
  station_name <- str_extract(station_name_line, "(?<=Estação: ).+")
  if (is.na(station_name) || station_name == "") {
    warning(sprintf("Station name not found in filD: %s", file))
    return(NULL)
  }
  station_name <- ifelse(station_name %in% names(name_mapping), name_mapping[station_name], station_name)
  
  lines <- readLines(file)
  data_lines <- lines[-(1:6)]  # Skip header lines
  
  parsed_data <- lapply(data_lines, function(line) parse_line(line, station_name)) %>% bind_rows()
  
  if (nrow(parsed_data) == 0) {
    warning(sprintf("No data parsed from filD: %s", file))
    return(NULL)
  }
  
  parsed_data$DateTime <- as.POSIXct(paste(parsed_data$Date, parsed_data$Time), format = "%d/%m/%Y %H:%M:%S",
                                     tz = "America/Sao_Paulo")
 dplyr::select(parsed_data, Station, DateTime, Rainfall_15min, Rainfall_1h)
}

# Define the directory containing the files
file_directory <- "D:/PluvioRIO/All_unziped"
#file_directory <- "D:/PluvioRIO/test"

# List all files and filter for the years 2010 to 2016
files <- list.files(file_directory, pattern = "_Plv\\.txt$", full.names = TRUE)
files <- files[grepl("2009|201[0-6]", files)]
#files <- files[grepl("2009\\d{2}_Plv\\.txt$", files)]
#files <- head(files, 100)  # Select only the first 100 files

name_mapping <- c(
  "Barra/Rio Centro" = "Barra/Riocentro",
  "Est. Grajau/Jacarepagua" = "Est. Grajaú/Jacarepaguá",
  "Grajau" = "Grajaú",
  "Grande Meier" = "Grande Méier",
  "Iraja" = "Irajá",
  "Jacarepagua/Cidade de Deus" = "Jacarepaguá/Cidade de Deus",
  "Jacarepagua/Tanque" = "Jacarepaguá/Tanque",
  "Jardim Botanico" = "Jardim Botânico",
  "Sao Cristovao" = "São Cristóvão",
  "Saude" = "Saúde"
)

# Example usage
all_data <- lapply(files, process_file) %>% bind_rows()
save(all_data, file = "D:/PROslide_RIO/DATA2/all_data3.Rd")
#load("D:/PROslide_RIO/DATA2/all_data3.Rd") 

#all_data$Date <- as.Date(all_data$DateTime)
all_data$Date <- substr(all_data$DateTime, 1, 10)
all_data$Date <- as.Date(all_data$Date)

specific_day_data <- all_data %>% 
  filter(Date == as.Date("2009-12-31") & Station=="Jardim Botânico")

specific_day_data <- all_data %>% 
  filter(Date == as.Date("2011-04-26") & Station=="Tijuca/Muda")


summary(as.factor(specific_day_data$Date))
summary(as.factor(specific_day_data$DateTime))
# specific_day_data <- all_data %>% 
#   filter(Date == as.Date("2008-02-01") & Station=="São Cristóvão")

# View the subsetted data
head(specific_day_data)





head(all_data)
# 
all_data <- all_data %>%
  mutate(Hour = as.POSIXct(format(DateTime, "%Y-%m-%d %H:00:00"), tz = "America/Sao_Paulo"))

names(all_data)






# 
# 
# # First, ensure the data is in chronological order
# all_data <- all_data %>%
#   arrange(Station, DateTime)
# 
# # Identify the start of new rainfall events
# all_data <- all_data %>%
#   mutate(EventStart = Rainfall_15min > 0 & lag(Rainfall_15min <= 0, default = TRUE))
# 
# # Create a cumulative sum to flag each event with an ID
# all_data <- all_data %>%
#   group_by(Station) %>%
#   mutate(EventID = cumsum(EventStart)) %>%
#   ungroup()
# 
# # Now, filter the events, keeping only rows where Rainfall_15min > 0
# rainy_events <- all_data %>%
#   filter(Rainfall_15min > 0)
# 
# # Calculate the duration of each rainfall event
# rainfall_events <- rainy_events %>%
#   group_by(Station, EventID) %>%
#   summarize(
#     Start = min(DateTime),
#     End = max(DateTime),
#     Duration = if_else(Start == End, 
#                        15, 
#                        as.numeric(difftime(End, Start, units = "mins")) + 15),
#     TotalRainfall = sum(Rainfall_15min, na.rm = TRUE),
#     .groups = 'drop' 
#   )
# 
# # View the rainfall events with their IDs
# head(rainfall_events)
# summary(rainfall_events)
# 
# rainfall_events <- rainfall_events %>%
#   filter(Duration <= 1440, # Duration of no more than 1 day
#          TotalRainfall <= 1500, # Total rainfall of no more than 1500
#          !is.na(EventID), # Exclude missing EventID
#          !is.na(Start), # Exclude missing Start
#          !is.na(End)) # Exclude missing End
# 
# # Viewing the summary of the filtered data
# summary(rainfall_events)
# 





# Calculate hourly rainfall
hourly_totals <- all_data %>%
  group_by(Station, Hour) %>%
  summarize(HourlyRainfall = sum(Rainfall_15min, na.rm = TRUE))

#summarize(HourlyRainfall = Rainfall_1h)
# Find the maximum hourly rainfall for each day
max_hourly_intensity <- hourly_totals %>%
  group_by(Station, Date = as.Date(Hour)) %>%
  summarize(MaxHourlyIntensity = max(HourlyRainfall, na.rm = TRUE))

# Assuming 'all_data' is your existing data frame
# First, calculate daily totals
daily_totals <- all_data %>% 
  group_by(Station, Date = as.Date(DateTime)) %>% 
  summarize(DailyRainfall = sum(Rainfall_15min, na.rm = TRUE))

# Merge with your existing daily_totals
daily_totals <- daily_totals %>%
  left_join(max_hourly_intensity, by = c("Station", "Date"))

# Define a function to calculate accumulated rainfall for a given number of days
calc_accum_rainfall <- function(data, days) {
  data %>% 
    group_by(Station) %>% 
    mutate(!!paste0("accum", days, "days") := purrr::map_dbl(Date, ~ sum(DailyRainfall[Date >= . - days + 1 & Date <= .], na.rm = TRUE)))
}

# Apply the function for 2, 5, 10, and 15 days
daily_totals <- daily_totals %>%
  calc_accum_rainfall(2) %>%
  calc_accum_rainfall(5) %>%
  calc_accum_rainfall(10) %>%
  calc_accum_rainfall(15)

# Add 'rainyday' column
daily_totals <- daily_totals %>%
  mutate(rainyday = DailyRainfall > 0)

daily_totals

# View or write the result
print(daily_totals)
specific_day_data2 <- daily_totals %>%
  filter(Date == as.Date("2009-12-31") & Station=="Jardim Botânico")

summary(daily_totals)
save(daily_totals, file = "D:/PROslide_RIO/DATA2/daily_totals2.Rd")
load("D:/PROslide_RIO/DATA2/daily_totals2.Rd") 


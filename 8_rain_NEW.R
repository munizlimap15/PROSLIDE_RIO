library(dplyr)
library(readr)
library(stringr)  # Load the stringr package

# Function to parse a single line
parse_line <- function(line, station_name) {
  line_parts <- unlist(strsplit(line, "\\s+"))
  if (length(line_parts) >= 6) {
    return(data.frame(
      Date = line_parts[1],
      Time = line_parts[2],
      Rainfall_15min = as.numeric(line_parts[4]),
      Rainfall_1h = as.numeric(line_parts[5]),
      Station = station_name
    ))
  } else {
    return(NULL)
  }
}


process_file <- function(file) {
  station_name_line <- readLines(file, n = 1)
  station_name <- str_extract(station_name_line, "(?<=Estação: ).+")
  if (is.na(station_name) || station_name == "") {
    warning(sprintf("Station name not found in file: %s", file))
    return(NULL)
  }
  station_name <- ifelse(station_name %in% names(name_mapping), name_mapping[station_name], station_name)
  
  lines <- readLines(file)
  data_lines <- lines[-(1:6)]  # Skip header lines
  
  parsed_data <- lapply(data_lines, function(line) parse_line(line, station_name)) %>% bind_rows()
  
  if (nrow(parsed_data) == 0) {
    warning(sprintf("No data parsed from file: %s", file))
    return(NULL)
  }
  
  parsed_data$DateTime <- as.POSIXct(paste(parsed_data$Date, parsed_data$Time), format = "%d/%m/%Y %H:%M:%S")
  dplyr::select(parsed_data, Station, DateTime, Rainfall_15min, Rainfall_1h)
}

# Define the directory containing the files
file_directory <- "E:/PluvioRIO/All_unziped"

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
save(all_data, file = "E:/PROslide_RIO/DATA2/all_data2.Rd")
#load("E:/PROslide_RIO/DATA2/all_data2.Rd") 



all_data$Date <- as.Date(all_data$DateTime)

specific_day_data <- all_data %>% 
  filter(Date == as.Date("2009-12-31") & Station=="Jardim Botânico")

# View the subsetted data
head(specific_day_data)





head(all_data)
# 
all_data <- all_data %>%
  mutate(Hour = as.POSIXct(format(DateTime, "%Y-%m-%d %H:00:00"), tz = "UTC"))

names(all_data)

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

save(daily_totals, file = "E:/PROslide_RIO/DATA2/daily_totals2.Rd")
load("E:/PROslide_RIO/DATA2/daily_totals2.Rd") 


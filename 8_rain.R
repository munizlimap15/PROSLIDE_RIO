library(dplyr)
library(readr)
library(stringr)  # Load the stringr package

# # set the working directory to the folder containing the sub-folders
# setwd("E:/PluvioRIO/Origin_ZIP")
# 
# # get the path of the parent directory
# parent_dir <- normalizePath("..")
# 
# # create a new directory for extracted files in the parent directory
# dir.create(file.path(parent_dir, "All_unziped2"))
# 
# # get a list of all sub-folders in the current working directory
# subfolders <- list.dirs(".", recursive = TRUE)
# 
# # loop through each sub-folder
# for (subfolder in subfolders) {
#   # get a list of all zip files in the current sub-folder
#   zipfiles <- list.files(subfolder, pattern = ".zip", full.names = TRUE)
#   
#   # loop through each zip file in the current sub-folder and extract it
#   for (zipfile in zipfiles) {
#     # extract the file into the All_unziped directory in the parent directory
#     unzip(zipfile, exdir = file.path(parent_dir, "All_unziped"))
#   }
# }


# Define the directory containing the files
file_directory <- "E:/PluvioRIO/All_unziped"

# List all files and filter for the years 2010 to 2016
files <- list.files(file_directory, pattern = "_Plv\\.txt$", full.names = TRUE)
files <- files[grepl("2009|201[0-6]", files)]
#files <- files[grepl("2009", files)]

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

process_file <- function(file) {
  station_name_line <- read_lines(file, n_max = 1)
  station_name <- str_extract(station_name_line, "(?<=Estação: ).+")
  
  # Check if the station name is in the name_mapping, if so, replace it
  station_name <- ifelse(station_name %in% names(name_mapping), name_mapping[station_name], station_name)
  
  
  # Check if the file has at least 7 lines (6 to skip + 1 for data)
  if (length(read_lines(file)) < 7) {
    warning(sprintf("File has insufficient lines and will be skipped: %s", file))
    return(NULL)
  }
  
  data <- read.table(file, skip = 6, fill = TRUE, sep = "")
  column_names <- c("Dia", "Hora","15_min", "01_h", "04_h", "24_h", "96_h")
  colnames(data) <- column_names[1:ncol(data)]
  
  data$DateTime <- as.POSIXct(paste(data$Dia, data$Hora), format = "%d/%m/%Y %H:%M:%S")
  data$Station <- station_name
  
  # Convert the 'Rainfall' column to numeric, replacing non-numeric values with NA
  data$Rainfall    <- as.numeric(as.character(data$`15_min`))
  data$Rainfall_1h <- as.numeric(as.character(data$`01_h`))
  #data$Rainfall_4h <- as.numeric(as.character(data$`04_h`))
  # Skip the warnings check
  # if (any(warnings())) {
  #   warning(sprintf("Non-numeric values found in Rainfall column in file: %s", file))
  # }
  
  data <- dplyr::select(data, Station, DateTime, Rainfall,Rainfall_1h)
  
  return(data)
}

# Read and combine data from all files
all_data <- lapply(files, process_file) %>% bind_rows()
all_data$Date <- as.Date(all_data$DateTime)

specific_day_data <- all_data %>% 
  filter(Date == as.Date("2009-12-31") & Station=="Jardim Botânico")

# View the subsetted data
head(specific_day_data)






head(all_data)
# 
all_data <- all_data %>%
  mutate(Hour = as.POSIXct(format(DateTime, "%Y-%m-%d %H:00:00"), tz = "UTC"))

# Calculate hourly rainfall
hourly_totals <- all_data %>%
  group_by(Station, Hour) %>%
  summarize(HourlyRainfall = sum(Rainfall, na.rm = TRUE))
  #summarize(HourlyRainfall = Rainfall_1h)

# Find the maximum hourly rainfall for each day
max_hourly_intensity <- hourly_totals %>%
  group_by(Station, Date = as.Date(Hour)) %>%
  summarize(MaxHourlyIntensity = max(HourlyRainfall, na.rm = TRUE))

# Assuming 'all_data' is your existing data frame
# First, calculate daily totals
daily_totals <- all_data %>% 
  group_by(Station, Date = as.Date(DateTime)) %>% 
  summarize(DailyRainfall = sum(Rainfall, na.rm = TRUE))

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

save(daily_totals, file = "E:/PROslide_RIO/DATA2/daily_totals.Rd")
load("E:/PROslide_RIO/DATA2/daily_totals.Rd") 

# Calculate the median for each day across all stations
#median_daily_totals <- aggregate(cbind(DailyRainfall, accum2days, accum5days, accum10days, accum15days) ~ Date, data = daily_totals, median)

# View the results
#print(median_daily_totals)

################################################################################
################################################################################
################################################################################
################################################################################
rioslides = sf::st_read("C:/Users/pedro/Documents/PROslide_RIO/DATA/landslides_2023.shp")

rioslides <- rioslides %>%
  dplyr::filter(tipologia1 == 1 | tipologia2 == 1 | tipologia3 == 1 | tipologia4 == 1)

rioslides$has_date <- ifelse(is.na(rioslides$data), "No Date", "Has Date")
summary(as.factor(rioslides$has_date))
#Has Date  No Date 
#701      963

rioslides <- rioslides %>%
  filter(!is.na(data))
################################################################################
################################################################################
################################################################################
################################################################################

library(sf)
library(dplyr)
library(raster)

# Read the shapefile
shapefile_path <- "E:/PROslide_RIO/DATA/stations.shp"
stations_sf <- st_read(shapefile_path)
#Unifying the names
# stations_sf$Estação <- gsub("Barra/Rio Centro", "Barra/Riocentro", stations_sf$Estação)
# stations_sf$Estação <- gsub("Est. Grajaú/Jacarepagua", "Est. Grajaú/Jacarepaguá", stations_sf$Estação)
# stations_sf$Estação <- gsub("Grajau", "Grajaú", stations_sf$Estação)
# stations_sf$Estação <- gsub("Iraja", "Irajá", stations_sf$Estação)
# stations_sf$Estação <- gsub("Jardim Botanico", "Jardim Botânico", stations_sf$Estação)
# stations_sf$Estação <- gsub("Sao Cristovao", "São Cristóvão", stations_sf$Estação)
# stations_sf$Estação <- gsub("Saude", "Saúde", stations_sf$Estação)
# Ensure rioslides is an sf object and has the correct CRS
# If necessary, transform rioslides to match the CRS of stations_sf
rioslides_sf <- st_transform(rioslides, st_crs(stations_sf))

# Calculate distances and find the nearest station for each landslide
nearest_stations <- st_join(rioslides_sf, stations_sf, join = st_nearest_feature) 

# Merge with daily rainfall data (assuming daily_totals has Station and Date columns)
merged_data <- nearest_stations %>%
  left_join(daily_totals, by = c("Estação" = "Station", "data" = "Date"))%>%
  mutate(ID = row_number()) 


# View the results
summary(merged_data)








# Assuming sp is your spatial object and df is your DataFrame
# and they both have a 'Station' and 'Date' field.
daily_totals= as.data.frame(daily_totals)

# Calculate median per day in daily_totals
medians_per_day <- daily_totals %>%
  group_by(Date) %>%
  summarize(
    DailyRainfall_median = median(DailyRainfall, na.rm = TRUE),
    MaxHourlyIntensity_median = median(MaxHourlyIntensity, na.rm = TRUE),
    accum2days_median = median(accum2days, na.rm = TRUE),
    accum5days_median = median(accum5days, na.rm = TRUE),
    accum10days_median = median(accum10days, na.rm = TRUE),
    accum15days_median = median(accum15days, na.rm = TRUE),
    rainyday_median = median(rainyday, na.rm = TRUE),
    .groups = 'drop'
  )

# Merge medians_per_day with merged_data based on date
merged_data <- left_join(merged_data, medians_per_day, by = c("data" = "Date"))

# Fill NAs in merged_data with medians and remove the temporary median columns
fields_to_fill <- c("DailyRainfall", "MaxHourlyIntensity", "accum2days", "accum5days", "accum10days", "accum15days", "rainyday")
for (field in fields_to_fill) {
  median_field_name <- paste0(field, "_median")
  
  # Fill NAs with medians
  na_indices <- is.na(merged_data[[field]])
  merged_data[[field]][na_indices] <- merged_data[[median_field_name]][na_indices]
  
  # Remove the temporary median column
  merged_data[[median_field_name]] <- NULL
}

# Check the summary of merged_data
summary(merged_data)

















###############################################
############# Add shp and hillshade
##############################################
# Read the study area shapefile
study_area <- st_read("E:/PROslide_RIO/DATA/StudyArea.shp")

###############################################
############# VIZUALIZATION
##############################################

merged_data <- merged_data %>%
  filter(!is.na(data)) 

# "DailyRainfall"     
# "MaxHourlyIntensity" 
# "accum2days"         
# "accum5days"         
# "accum10days"        
# "accum15days"        
# "rainyday"
library(ggplot2)
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

gg1=merged_data%>%
  ggplot() +
  geom_sf(aes(color = DailyRainfall, size = DailyRainfall), alpha = 0.9, shape = 15) +
  scale_color_viridis_c(option = "B", na.value = "red",
    limits = c(0, max(merged_data$DailyRainfall, na.rm = TRUE)), direction=-1,
    breaks = seq(0, max(merged_data$DailyRainfall, na.rm = TRUE), by = 50)) +
  scale_size_continuous(
    range = c(1, 5),  # Adjust maximum size here
    guide = 'none') +
  geom_sf(data = study_area, fill = NA, color = "black") +
  #theme_minimal() +
  map_theme +
  labs(color = "Daily Rainfall (mm)")

################################################################################
gg2=merged_data%>%
  ggplot() +
  geom_sf(aes(color = MaxHourlyIntensity, size = MaxHourlyIntensity), alpha = 0.9, shape = 15) +
  scale_color_viridis_c(option = "B", 
                        limits = c(0, max(merged_data$MaxHourlyIntensity, na.rm = TRUE)), direction=-1,
                        breaks = seq(0, max(merged_data$MaxHourlyIntensity, na.rm = TRUE), 
                                     by = round(max(merged_data$MaxHourlyIntensity, na.rm = TRUE)/4))) +
  scale_size_continuous(
    range = c(1, 5),  # Adjust maximum size here
    guide = 'none') +
  geom_sf(data = study_area, fill = NA, color = "black") +
  map_theme +
  labs(color = "Max hourly intensity (mm/h)")


################################################################################
gg3=merged_data%>%
  ggplot() +
  geom_sf(aes(color = accum2days, size = accum2days), alpha = 0.9, shape = 15) +
  scale_color_viridis_c(option = "B", 
                        limits = c(0, max(merged_data$accum2days, na.rm = TRUE)), direction=-1,
                        breaks = seq(0, max(merged_data$accum2days, na.rm = TRUE), 
                                     by = round(max(merged_data$accum2days, na.rm = TRUE)/4))) +
  scale_size_continuous(
    range = c(1, 5),  # Adjust maximum size here
    guide = 'none') +
  geom_sf(data = study_area, fill = NA, color = "black") +
  map_theme +
  labs(color = "Accumulated rainfall - 2days (mm)")

################################################################################
gg4=merged_data%>%
  ggplot() +
  geom_sf(aes(color = accum5days, size = accum5days), alpha = 0.9, shape = 15) +
  scale_color_viridis_c(option = "B", 
                        limits = c(0, max(merged_data$accum5days, na.rm = TRUE)), direction=-1,
                        breaks = seq(0, max(merged_data$accum5days, na.rm = TRUE), 
                                     by = round(max(merged_data$accum5days, na.rm = TRUE)/4))) +
  scale_size_continuous(
    range = c(1, 5),  # Adjust maximum size here
    guide = 'none') +
  geom_sf(data = study_area, fill = NA, color = "black") +
  #theme_minimal() +
  map_theme +
  labs(color = "Accumulated rainfall - 5 days (mm)")

################################################################################
gg5=merged_data%>%
  ggplot() +
  geom_sf(aes(color = accum10days, size = accum10days), alpha = 0.9, shape = 15) +
  scale_color_viridis_c(option = "B", 
                        limits = c(0, max(merged_data$accum10days, na.rm = TRUE)), direction=-1,
                        breaks = seq(0, max(merged_data$accum10days, na.rm = TRUE), 
                                     by = round(max(merged_data$accum10days, na.rm = TRUE)/4))) +
  scale_size_continuous(
    range = c(1, 5),  # Adjust maximum size here
    guide = 'none') +
  geom_sf(data = study_area, fill = NA, color = "black") +
  #theme_minimal() +
  map_theme +
  labs(color = "Accumulated rainfall - 10 days (mm)")
################################################################################
gg6=merged_data%>%
  ggplot() +
  geom_sf(aes(color = accum15days, size = accum15days), alpha = 0.9, shape = 15) +
  scale_color_viridis_c(option = "B", 
                        limits = c(0, max(merged_data$accum15days, na.rm = TRUE)), direction=-1,
                        breaks = seq(0, max(merged_data$accum15days, na.rm = TRUE), 
                                     by = round(max(merged_data$accum15days, na.rm = TRUE)/4))) +
  scale_size_continuous(
    range = c(1, 5),  # Adjust maximum size here
    guide = 'none') +
  geom_sf(data = study_area, fill = NA, color = "black") +
  #theme_minimal() +
  map_theme +
  labs(color = "Accumulated rainfall - 15 days (mm)")

##################################################################
my_theme= theme(
  axis.text = element_text(size = 20),  # Adjust the axis text size
  axis.title = element_text(size = 22),  # Adjust the axis title size
  legend.text = element_text(size = 22),  # Adjust the legend text size
  legend.title = element_text(size = 22)  # Adjust the legend title size
)


hist1 <- ggplotGrob(ggplot(merged_data, aes(x = DailyRainfall)) +
                      geom_histogram(binwidth = 10, fill = "blue", color = "white") +
                      theme_minimal() +
                      my_theme+
                      labs(x = "", y = "n slides"))

hist2 <- ggplotGrob(ggplot(merged_data, aes(x = MaxHourlyIntensity)) +
                      geom_histogram(binwidth = 10, fill = "blue", color = "white") +
                      theme_minimal() +
                      my_theme+
                      labs(x = "", y = "n slides"))

hist3 <- ggplotGrob(ggplot(merged_data, aes(x = accum2days)) +
                      geom_histogram(binwidth = 10, fill = "blue", color = "white") +
                      theme_minimal() +
                      my_theme+
                      labs(x = "", y = "n slides"))

hist4 <- ggplotGrob(ggplot(merged_data, aes(x = accum5days)) +
                      geom_histogram(binwidth = 10, fill = "blue", color = "white") +
                      theme_minimal() +
                      my_theme+
                      labs(x = "", y = "n slides"))

hist5 <- ggplotGrob(ggplot(merged_data, aes(x = accum10days)) +
                      geom_histogram(binwidth = 10, fill = "blue", color = "white") +
                      theme_minimal() +
                      my_theme+
                      labs(x = "", y = "n slides"))

hist6 <- ggplotGrob(ggplot(merged_data, aes(x = accum15days)) +
                      geom_histogram(binwidth = 10, fill = "blue", color = "white") +
                      theme_minimal() +
                      my_theme+
                      labs(x = "", y = "n slides"))


library(patchwork)

fig1 <- gg1 + hist1 +
  plot_layout(ncol = 1, nrow = 2, heights = c(4, 1))

fig2 <- gg2 + hist2 +
  plot_layout(ncol = 1, nrow = 2, heights = c(4, 1))

fig3 <- gg3 + hist3 +
  plot_layout(ncol = 1, nrow = 2, heights = c(4, 1))

fig4 <- gg4 + hist4 +
  plot_layout(ncol = 1, nrow = 2, heights = c(4, 1))

fig5 <- gg5 + hist5 +
  plot_layout(ncol = 1, nrow = 2, heights = c(4, 1))

fig6 <- gg6 + hist6 +
  plot_layout(ncol = 1, nrow = 2, heights = c(4, 1))



png("E:/PROslide_RIO/PRESENTATIONS/DailyRainfall2.png", width =1200, height = 800)
print(fig1)
dev.off()


png("E:/PROslide_RIO/PRESENTATIONS/MaxHourlyIntensity2.png", width =1200, height = 800)
print(fig2)
dev.off()

png("E:/PROslide_RIO/PRESENTATIONS/accum2days2.png", width =1200, height = 800)
print(fig3)
dev.off()

png("E:/PROslide_RIO/PRESENTATIONS/accum5days2.png", width =1200, height = 800)
print(fig4)
dev.off()

png("E:/PROslide_RIO/PRESENTATIONS/accum10days2.png", width =1200, height = 800)
print(fig5)
dev.off()

png("E:/PROslide_RIO/PRESENTATIONS/accum15days2.png", width =1200, height = 800)
print(fig6)
dev.off()


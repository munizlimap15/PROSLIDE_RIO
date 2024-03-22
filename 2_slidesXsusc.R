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
st_write(rioslides, "D:/PROslide_RIO/Rcodes/Shinny_app_RioSlide/landslides_2023_with_pred.shp", overwrite= TRUE, append=TRUE)

rioslides <- rioslides[rioslides$suscetibilidade_rio != 255, ]

summary(as.factor(rioslides$suscetibilidade_rio))

Summary<- rioslides %>%
  dplyr::group_by(suscetibilidade_rio)%>%
  dplyr::summarise(n_slide = n())


pred_recode <- c("1" = "Low",
                 "2" = "Medium",
                 "3" = "High")


Summary$suscetibilidade_rio=as.character(Summary$suscetibilidade_rio)
Summary$pred_recode      <- as.character(pred_recode  [Summary$suscetibilidade_rio])

Summary$pred_recode <- factor(Summary$pred_recode, levels=c("Low", "Medium", "High"))

gg=ggplot(Summary) +
  aes(x = pred_recode, weight = n_slide, fill = pred_recode) + # Added fill inside aes
  geom_bar(color="black") +
  scale_fill_manual(values = alpha(c('Low' = "white", 'Medium' = "#EFC000FF", 'High' = "darkorange1"), 0.7)) +
  theme_minimal()+
  geom_text(aes(label = n_slide, y = n_slide / 2), vjust = -0.5) + # Adding the text
  labs(title = "", y="N slides", x="Prediction Categories",
       #caption = "Aqui, apenas os pontos citados como 'Escorregamento de solo' foram usados (Tipologia = 1). Esses mesmos somam 1656 ocorrencias.")+
       caption = "Only the obs mentioned as 'shallow landslides' were used (Typology = 1).")+
  theme(legend.position = "none",
        text = element_text(size=18),
        plot.caption = element_text(face = "italic", size=10, color="gray40"))


png("E:/PROslide_RIO/PRESENTATIONS/plot2.png", width = 500, height = 400)
print(gg)
dev.off()

gg=ggplot(Summary) +
  aes(x = pred_recode, weight = n_slide, fill = pred_recode) + # Added fill inside aes
  geom_bar(color="black") +
  scale_fill_manual(values = alpha(c('Low' = "#008000", 'Medium' = "yellow", 'High' = "red"), 0.7)) +
  theme_minimal()+
  geom_text(aes(label = n_slide, y = n_slide / 2), vjust = -0.5) + # Adding the text
  labs(title = "", y="N slides", x="Prediction Categories",
       #caption = "Aqui, apenas os pontos citados como 'Escorregamento de solo' foram usados (Tipologia = 1). Esses mesmos somam 1656 ocorrencias.")+
       #caption = "Only the obs mentioned as 'shallow landslides' were used (Typology = 1)."
       )+
  theme(legend.position = "none",
        text = element_text(size=18),
        plot.caption = element_text(face = "italic", size=10, color="gray40"))


png("D:/PROslide_RIO/Rcodes/Shinny_app_RioSlide/plot2.png", width = 500, height = 400)
print(gg)
dev.off()



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

library(ggplot2)
library(dplyr)
library(raster)
library(sf)
library(lubridate)
library(mapview)


pred = raster("E:/PROslide_RIO/DATA2/pred_megam.tif")

rcl = matrix(c(-Inf, 0, 0,   0, 0.211765, 1,  0.211765, 0.580392, 2,  0.580392, 1, 3), ncol=3, byrow=TRUE) #This is the Original MoNOE

reclassify(pred, rcl, filename="E:/PROslide_RIO/DATA2/pred_megam_class.tif", overwrite = TRUE)


pred_class = raster("E:/PROslide_RIO/DATA2/pred_megam_class.tif")

# Read the shapefile using sf package
rioslides <- sf::st_read("D:/PROslide_RIO/DATA/landslides_2023.shp")

rioslides <- rioslides %>%
  dplyr::filter(tipologia1 == 1 | tipologia2 == 1 | tipologia3 == 1 | tipologia4 == 1)


# Define factor levels for the new values
rcl <- data.frame(ID = c(1, 2, 3),
                  level = c("Baixo", "Medio", "Alto"))

# Apply factor levels to the raster
pred_class <- ratify(pred_class)
levels(pred_class) <- list(rcl)

# Define the colors
colors <- c('Baixo' = "gray", 'Medio' = "#987645", 'Alto' = "#65828D")

# Selecting only the necessary fields
rioslides_reduced <- rioslides %>%
  dplyr::select(geometry)


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

# Extracting from predictions
beginCluster()
rioslides_spdf_extract <- raster::extract(pred_class, rioslides_spdf, df = TRUE)
endCluster()


rioslides   <- merge(rioslides_spdf_extract,   rioslides,   by.x = 'ID', by.y = 'ID')

rioslides <- rioslides[rioslides$pred_megam_class != 255, ]



Summary <- rioslides %>%
  dplyr::group_by(pred_megam_class)%>%
  dplyr::summarise(n_slide = n())%>%
  filter(!is.na(pred_megam_class))


pred_recode <- c("1" = "Low",
                 "2" = "Medium",
                 "3" = "High")


Summary$pred_megam_class=as.character(Summary$pred_megam_class)
Summary$pred_recode      <- as.character(pred_recode  [Summary$pred_megam_class])

Summary$pred_recode <- factor(Summary$pred_recode, levels=c("Low", "Medium", "High"))

gg=ggplot(Summary) +
  aes(x = pred_recode, weight = n_slide, fill = pred_recode) + # Added fill inside aes
  geom_bar(color="black") +
  scale_fill_manual(values = alpha(c('Low' = "gray", 'Medium' = "#9B835B", 'High' = "#65828D"), 0.7)) +
  theme_minimal()+
  geom_text(aes(label = n_slide, y = n_slide / 2), vjust = -0.5) + # Adding the text
  labs(title = "", y="N slides", x="Prediction Categories",
       #caption = "Aqui, apenas os pontos citados como 'Escorregamento de solo' foram usados (Tipologia = 1). Esses mesmos somam 1656 ocorrencias.")+
       caption = "Only the obs mentioned as 'shallow landslides' were used (Typology = 1).")+
  theme(legend.position = "none",
        text = element_text(size=18),
        plot.caption = element_text(face = "italic", size=10, color="gray40"))


png("E:/PROslide_RIO/PRESENTATIONS/plottttttt2.png", width = 500, height = 400)
print(gg)
dev.off()


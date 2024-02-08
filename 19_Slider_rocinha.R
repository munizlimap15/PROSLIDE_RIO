library(sf)
library(raster)
library(leaflet)
#remotes::install_github('trafficonese/leaflet.extras2')
library(leaflet.extras2)


rioslides <- sf::st_read("D:/PROslide_RIO/DATA/landslides_2023.shp")
rioslides <- rioslides %>%
  dplyr::filter(tipologia1 == 1 | tipologia2 == 1 | tipologia3 == 1 | tipologia4 == 1)
study_area          <- st_read("StudyArea.shp")
Limite_Favelas_2019 <- st_read("Limite_Favelas_2019.shp")
Limite_Favelas_2019 <- st_simplify(Limite_Favelas_2019, dTolerance = 1)
#Limite_Favelas_2019 <- sample_n(Limite_Favelas_2019,2)


#1. Load Libraries and Rasters
pred                = raster("D:/PROslide_RIO/Susc_heuristic/suscetibilidade_rio.tif")
pred2               = raster("D:/PROslide_RIO/DATA2/pred_megam_class.tif")


rocinha <- Limite_Favelas_2019[Limite_Favelas_2019$nome == "Rocinha", ]

mapview::mapview(rocinha)

################################################################################
################################################################################
################################################################################
################################################################################


# Crop rasters to Rocinha
pred_crop <- crop(pred, as(rocinha, "Spatial"))
pred2_crop <- crop(pred2, as(rocinha, "Spatial"))

# Create color palettes for the rasters
pal1 <- colorNumeric(palette = "viridis", domain = values(pred_crop))
pal2 <- colorNumeric(palette = "viridis", domain = values(pred2_crop))

# Create a leaflet map
leaflet() %>% 
  addTiles() %>% 
  addRasterImage(pred_crop, colors = pal1, opacity = 0.8, group = "Layer1") %>% 
  addRasterImage(pred2_crop, colors = pal2, opacity = 0.8, group = "Layer2") %>% 
  addLayersControl(overlayGroups = c("Layer1", "Layer2"), options = layersControlOptions(collapsed = FALSE)) %>% 
  addProviderTiles(providers$OpenStreetMap) %>%
  addSidebyside(layerId = "sidecontrols", rightId = "Layer1", leftId = "Layer2")

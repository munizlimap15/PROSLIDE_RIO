# library
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
#library(rgdal)
library(mapview)
library(raster)
#library(rgeos)
library(sp)
library(sf)

#filename <- file.choose()

study_area          <- st_read("StudyArea.shp")

size <- 4000
#hex_points <- spsample(gBuffer(as_Spatial(study_area), byid=TRUE, width=10000), type = "hexagonal", cellsize = size)
# Assuming study_area is an sf object
study_area2 <- st_buffer(study_area, dist = 1000)
# Create a regular grid
grid <- st_make_grid(study_area, cellsize = size, square= FALSE) %>% st_sfc()

plot(grid)
plot(study_area, add=TRUE)

hex_grid      <- st_intersection( st_as_sf(study_area),st_as_sf(grid))   #clip polygon
hex_grid      <- as(hex_grid, "sf")

#Add a random value to color the hexagons
hex_grid$value= sample(x = 1:5, size = nrow(hex_grid), replace = TRUE)

#mapview::mapview(hex_grid, zcol="value")


color_palette <- c("#547A75", "#5092C1", "#81B6D0", "#A4C3D0", "#A8A184")
#color_palette <- c("#0A5255", "#55AD8C", "#6DF1F5", "#DDF2DC", "#B6DA7F")

p=ggplot() +
  geom_sf(data= hex_grid, aes(fill = value), shape = "circle", lwd = .8, color="white") +
  #scale_fill_distiller(palette="PuBu")+
  #scale_fill_gradient(low = "#FFD42F", high = "#015995")+
  scale_fill_gradientn(colors = color_palette) +
    theme_void() +
  theme(legend.position = "none")+
  #geom_sf(data=clip, aes(fill = leng), size =1,  alpha = 10/10,color="blue")+
  geom_sf(data=study_area,  lwd =1, alpha = 1/10, color="black", fill = "white") +
  theme(
    panel.background = element_rect(fill='transparent', color=NA),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )
p
ggsave('D:/PROslide_RIO/Rcodes/Shinny_app_RioSlide/www/myplot.png', p, bg='transparent')


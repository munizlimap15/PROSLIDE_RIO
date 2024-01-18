library(ggplot2)
library(sf)
library(viridis)
library(tidyr) 
library(dplyr)
library(scales)

# Read the shapefiles
final_rioslides <- st_read("D:/PROslide_RIO/DATA2/final_rioslides.shp")
study_area <- st_read("D:/PROslide_RIO/DATA/StudyArea.shp")


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

################################################################################
################################################################################
################################################################################

gg1=final_rioslides%>%
  ggplot() +
  geom_sf(aes(color = DlyRnfl, size = DlyRnfl), alpha = 0.9, shape = 15) +
  scale_color_viridis_c(option = "B", na.value = "red",
                        limits = c(0, max(final_rioslides$DlyRnfl, na.rm = TRUE)), direction=-1,
                        breaks = seq(0, max(final_rioslides$DlyRnfl, na.rm = TRUE), by = 100)) +
  scale_size_continuous(
    range = c(1, 5),  # Adjust maximum size here
    guide = 'none') +
  geom_sf(data = study_area, fill = NA, color = "black") +
  #theme_minimal() +
  map_theme +
  labs(color = "Daily Rainfall (mm)")



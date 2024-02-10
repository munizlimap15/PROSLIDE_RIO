library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)
library(cowplot)
library(raster)
## install.packages("devtools")
#devtools::install_github("yutannihilation/ggsflabel")
#library(ggsflabel)


setwd("D:/PROslide_RIO/Figs/per_district")

municip <- st_read("D:/PROslide_RIO/Rcodes/Shinny_app_RioSlide/StudyArea.shp")

district <-st_read("D:/PROslide_RIO/FromPrefeitura/Limite_de_Bairros/Limite_de_Bairros.shp") %>% st_transform(st_crs(municip))
district <- st_join(district, municip, join = st_intersects, largest = TRUE)

district <- st_cast(district, "POLYGON")

district <- district %>% 
  mutate(area = st_area(geometry))


summary(district$area)
# Define a threshold area size
threshold_area <- set_units(6118, m^2)

# Subset districts larger than the threshold
district <- district[district$area > threshold_area, ]


district_grouped <- district %>%
  group_by(rp) %>%
  summarize(
    área = sum(área, na.rm = TRUE), # Replace with the appropriate summary functions for your variables
    # Include other variables as needed and summarize them accordingly
    geometry = st_union(geometry),
    .groups = "drop"
  )

hillshade <- raster("D:/PROslide_RIO/SAGA/hillshade.tif")
#slp <- raster("E:/PEDRO/National_scale/PREDICTORS_SLOPEUNITS_PUBLICATION_pixelscale100m/slope")#melr_noflat_t
pred = raster("D:/PROslide_RIO/DATA2/pred_megam_class.tif")


points <- st_read("D:/PROslide_RIO/Rcodes/Shinny_app_RioSlide/landslides_2023_with_pred.shp")#landslides_2023.shp %>% st_transform(st_crs(municip))
# Extract coordinates
coords <- st_coordinates(points)

# Add coordinates as new columns
points$x <- coords[, "X"]
points$y <- coords[, "Y"]


logo_file <- "D:/PROslide_RIO/Rcodes/Shinny_app_RioSlide/www/Untitled.jpg"

# no_model <- sf::read_sf(dsn = "D:/PEDRO/Slope_unitsPublication/flat_terrain.shp")
# no_model <- st_transform(no_model, st_crs(states))


pred <- resample(pred, hillshade, method = "bilinear")

#water <- sf::read_sf(dsn = "C:/Users/pedro/Documents/Portfolio_codes/Lima_2001_NewVIZ/Water/stehendeGewaesser.shp")

# 4. Plot a separated ggplot map for each municipality with the prediction raster overlayed
#for (i in 1:5) {
for (i in 1:length(district_grouped$rp)) {
  bairro <- district_grouped[i,] %>% as_Spatial()
  
  rp=bairro$rp
  
  save_name=bairro$rp
  
  #pred_clip <- crop(pred, extent(bairro))
  if (!is.null(intersect(extent(pred), extent(bairro)))) {
    pred_clip <- crop(pred, extent(bairro))
  } else {
    cat("The raster and bairro do not overlap. Skipping this iteration.\n")
    next
  }
  
  
  
  
  
  pred_clip <- mask(pred_clip, bairro)
  
  #slp_clip <- crop(slp, extent(bairro))
  #slp_clip <- mask(slp_clip, bairro)
  
  hill_clip <- crop(hillshade, extent(bairro))
  
  pred_df       <- as.data.frame(pred_clip, xy = TRUE) 
  
  #slp_df       <- as.data.frame(slp_clip, xy = TRUE) 
  
  hillshade_values <- getValues(hill_clip)
  hillshade_df <- data.frame(x = coordinates(hill_clip)[, 1], 
                             y = coordinates(hill_clip)[, 2], 
                             hill = hillshade_values) 
  
  points_in <- st_intersection(points, st_as_sf(bairro))
  n_slide  <- nrow(points_in)
  #no_model_municip <- st_intersection(no_model, st_as_sf(bairro))
  # Extract x and y coordinate ranges from the bairro's geometry
  bairro <- st_as_sf(bairro)
  x_range <- range(st_coordinates(bairro)[, 1])
  y_range <- range(st_coordinates(bairro)[, 2])
  
  
  pred_df$Susceptibility <-   factor(pred_df$pred_megam_class, levels = c("1", "2", "3"), labels = c("Low", "Medium", "High"))
  
  
  
  intersects <- st_intersects(district, bairro, sparse = FALSE)
  
  # Create a logical vector to filter districts
  within_bairro <- apply(intersects, MARGIN = 1, FUN = any)
  
  # Filter the district data to only include those within bairro
  district_within_bairro <- district[within_bairro, ]
  district_within_bairro <- st_intersection(district_within_bairro, bairro)
  # Calculate the area of the intersected geometries
  district_within_bairro$area <- st_area(district_within_bairro)
  
  # Set a minimum area threshold to filter out small slivers
  min_area_threshold <- min(district_within_bairro$area) * 0.1 # adjust threshold as appropriate
  
  # Filter out small slivers
  district_within_bairro <- district_within_bairro[district_within_bairro$area > min_area_threshold, ]
  #####################################################################
  #####################################################################
  #####################################################################
  #####################################################################
  plot2=ggplot() + 
    #annotation_map_tile() +  # Add this line to include a basemap; adjust zoom level as needed
    geom_raster(data= hillshade_df, aes(x = x,  y = y, fill = hill),alpha=1)+
    scale_fill_gradient(low = "white", high = "black", guide = "none") +
    scale_alpha(name = "", range = c(0.6, 0), guide = "none")+
    ggnewscale::new_scale_fill()+
    
    geom_raster(data= na.omit(pred_df), aes(x = x,  y = y, fill = Susceptibility),alpha=0.7)+
    scale_fill_manual(values = c("white", "#987645", "#65828D")) +
    ggnewscale::new_scale_fill()+
    
    #geom_sf(data = no_model_municip, aes(fill = "No modelled\n terrain"), color = "pink", size=3, alpha=0.5) +
    #scale_fill_manual(name = "", values = c("No modelled\n terrain" = "pink")) +
    
    geom_point(data=points_in, aes(x=x, y= y, color = "Landslide Points"), shape = 17, size=1) +
    scale_color_manual(name = "", values = c("Landslide Points" = "black")) + # Define the color and legend name for the points
    
    geom_sf(data = bairro, fill = NA, color = "black", size=5) +
    
    geom_sf(data = district_within_bairro, fill = NA, color = "black", size=5) +
    #geom_sf_label(data = district_within_bairro,aes(label = nome))+
    #ggtitle("Planning Regions (RP), Administrative Regions (RA), and Neighborhoods of the Municipality of Rio de Janeiro") +
    ggtitle(paste0("Predictions for Municipality\nPlanning Regions (RP) ", rp))+
    coord_sf(xlim = x_range, ylim = y_range)+
    
    ggspatial::annotation_north_arrow(location = "bl",
                                      pad_y = unit(0, "cm"),
                                      height = unit(.75, "cm"),
                                      width = unit(.5, "cm"),
                                      style = north_arrow_fancy_orienteering)+
    ggspatial::annotation_scale(location = "br",pad_y = unit(0, "cm"),pad_x = unit(0, "cm"))+
    theme(legend.position = "bottom",#c(0.25, 0.1),
          #legend.box = "vertical", # Stack legends vertically
          legend.key = element_rect(color = "black"),
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          plot.caption = element_text(size = 6, color = "gray30"))+ 
    labs(subtitle = "Source: Prefeitura do Rio de Janeiro; IPP; RJ",
         caption = "Note: These predictions are intended for regional-scale interpretation. Local-scale decisions should not be based solely on this data.\n Please exercise caution and consider the context of the scale when interpreting these maps.") +
    annotate("text", x = min(x_range), y = max(y_range), hjust =0,vjust =-1.5,
             label = paste0("Number of observed landslides: ", n_slide), 
             color = "red", size = 3)
  
  ###############################################################################################
  ###############################################################################################
  ###############################################################################################
  plot2=ggdraw() +
    draw_plot(plot2)+
    draw_image(logo_file,  x = .35, y = .45, scale = .3)
  
  
  
  mini_map <- ggplot(data = district_grouped) +
    geom_sf(fill = "white", color = "darkgray") + # All municipalities in white
    geom_sf(data = district_grouped[i,], fill = "red", color = "red", size=3) +
    
    ggspatial::annotation_north_arrow(location = "bl",
                                      pad_y = unit(0, "cm"),
                                      height = unit(.75, "cm"),
                                      width = unit(.5, "cm"),
                                      style = north_arrow_fancy_orienteering)+
    ggspatial::annotation_scale(location = "br",pad_y = unit(0, "cm"),pad_x = unit(0, "cm"))+
    theme(legend.position = "bottom",#c(0.25, 0.1),
          #legend.background = element_rect(fill = "darkgray"),
          legend.key = element_rect(color = "black"),
          axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank())
  
  
  #gradient_cols <- c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#091042")
  
  # # Create a data frame for the rectangles
  # rect_df <- data.frame(
  #   xmin = c(0, 0.2, 0.4, 0.6, 0.8),
  #   xmax = c(0.2, 0.4, 0.6, 0.8, 1.0),
  #   ymin = -Inf,
  #   ymax = Inf,
  #   fill = gradient_cols
  # )
  
  # histogram1 <- ggplot(pred_df, aes(x = melr_noflat)) +
  #   geom_rect(data = rect_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
  #                                 fill = fill), inherit.aes = FALSE, alpha = 0.5) +
  #   
  #   scale_fill_identity() +
  #   geom_histogram(binwidth = 0.01, fill = "blue", color = "black", alpha = 0.7) + # Adjust binwidth as needed
  #   scale_x_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) + # Setting both breaks and limits
  #   labs(title = "Distribution of the predictions",
  #        x = "Landslide susceptibility",
  #        y = "Frequency") 
  
  
  plot_left=cowplot::plot_grid(mini_map, mini_map,mini_map,#histogram1,histogram2,# histogram2,
                               ncol = 1, nrow = 3,
                               rel_heights = c(0.6, 0.7, 0.7))
  
  plot2=cowplot::plot_grid(plot_left, plot2,
                           ncol = 2, nrow = 1,
                           rel_widths = c(0.5,1))+
    theme(
      panel.background = element_rect(fill = "white"), # Set panel background to white
      plot.background = element_rect(fill = "white"),  # Set plot background to white
      legend.background = element_rect(fill = "white") # Set legend background to white
    )
  
  
  
  # Save the figure as a PNG file
  filename <- paste0("a", i, "_", save_name, ".png")
  ggsave(filename, plot2, width = 12, height = 8)
  # Print the filename for reference
  cat("Saved:", filename, "\n")
}

# Reset the working directory to its original value if needed
# setwd("..")



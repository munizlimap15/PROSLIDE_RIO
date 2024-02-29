library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)
library(cowplot)
library(raster)
library(exactextractr)
library(tidyr)
library(ggrepel)
## install.packages("devtools")
#devtools::install_github("yutannihilation/ggsflabel")
#library(ggsflabel)


setwd("D:/PROslide_RIO/Figs/per_favela")

municip <- st_read("D:/PROslide_RIO/Rcodes/Shinny_app_RioSlide/StudyArea.shp")

favelas <- st_read("D:/PROslide_RIO/FromPrefeitura/Limite_Favelas_2019/Limite_Favelas_2019.shp")  %>% st_transform(st_crs(municip))
favelas <- st_join(favelas, municip, join = st_intersects, largest = TRUE)
#To make paqueta a new "rp"
# favelas <- favelas %>%
#   mutate(rp = ifelse(nome == "Paquetá", "Paquetá", rp))

#favelas <- st_cast(favelas, "POLYGON")

favelas <- favelas %>% 
  mutate(area = st_area(geometry))


summary(favelas$area)

favelas <- favelas %>%
  filter(pop_sabren > 10000)


hillshade <- raster("D:/PROslide_RIO/SAGA/hillshade.tif")
#slp <- raster("E:/PEDRO/National_scale/PREDICTORS_SLOPEUNITS_PUBLICATION_pixelscale100m/slope")#melr_noflat_t
pred = raster("D:/PROslide_RIO/Susc_heuristic/suscetibilidade_rio.tif")


points <- st_read("D:/PROslide_RIO/Rcodes/Shinny_app_RioSlide/landslides_2023_with_pred.shp")#landslides_2023.shp %>% st_transform(st_crs(municip))
points <- points %>% 
  distinct(geometry, .keep_all = TRUE)
# Extract coordinates
coords <- st_coordinates(points)

# Add coordinates as new columns
points$x <- coords[, "X"]
points$y <- coords[, "Y"]


logo_file <- "D:/PROslide_RIO/Rcodes/Shinny_app_RioSlide/www/myplot.png"

# no_model <- sf::read_sf(dsn = "D:/PEDRO/Slope_unitsPublication/flat_terrain.shp")
# no_model <- st_transform(no_model, st_crs(states))


pred <- resample(pred, hillshade, method = "bilinear")

output_file <- "D:/PROslide_RIO/Figs/per_favelas/percentages.txt"

# 4. Plot a separated ggplot map for each municipality with the prediction raster overlayed
#for (i in 1:5) {
for (i in 1:length(favelas$objectid)) {
  
  favela <- favelas[i,] %>% as_Spatial()
  
  save_name=favela$nome
  n_people= favela$pop_sabren
  area_favela= favela$area
  density= n_people/area_favela
 
  
  #pred_clip <- crop(pred, extent(favela))
  if (!is.null(intersect(extent(pred), extent(favela)))) {
    pred_clip <- crop(pred, extent(favela))
  } else {
    cat("The raster and favela do not overlap. Skipping this iteration.\n")
    next
  }
  
  
  pred_clip <- mask(pred_clip, favela)
  
  
  #slp_clip <- crop(slp, extent(favela))
  #slp_clip <- mask(slp_clip, favela)
  
  hill_clip <- crop(hillshade, extent(favela))
  
  pred_df       <- as.data.frame(pred_clip, xy = TRUE) 
  
  #slp_df       <- as.data.frame(slp_clip, xy = TRUE) 
  
  hillshade_values <- getValues(hill_clip)
  hillshade_df <- data.frame(x = coordinates(hill_clip)[, 1], 
                             y = coordinates(hill_clip)[, 2], 
                             hill = hillshade_values) 
  
  points_in <- st_intersection(points, st_as_sf(favela))
  n_slide  <- nrow(points_in)
  #no_model_municip <- st_intersection(no_model, st_as_sf(favela))
  # Extract x and y coordinate ranges from the favela's geometry
  favela <- st_as_sf(favela)
  x_range <- range(st_coordinates(favela)[, 1])
  y_range <- range(st_coordinates(favela)[, 2])
  
  
  pred_df$Susceptibility <-   factor(pred_df$count_, levels = c("1", "2", "3"), labels = c("Low", "Medium", "High"))
  
  
  
  # intersects <- st_intersects(favelas, favela, sparse = FALSE)
  # 
  # # Create a logical vector to filter favelass
  # within_favela <- apply(intersects, MARGIN = 1, FUN = any)
  # 
  # # Filter the favelas data to only include those within favela
  # favelas_within_favela <- favelas[within_favela, ]
  # favelas_within_favela <- st_intersection(favelas_within_favela, favela)
  # # Calculate the area of the intersected geometries
  # favelas_within_favela$area <- st_area(favelas_within_favela)
  # 
  # # Set a minimum area threshold to filter out small slivers
  # min_area_threshold <- min(favelas_within_favela$area) * 0.1 # adjust threshold as appropriate
  # 
  # # Filter out small slivers
  # favelas_within_favela <- favelas_within_favela[favelas_within_favela$area > min_area_threshold, ]
  # favelas_within_favela$centroid <- sf::st_centroid(favelas_within_favela$geometry)
  # 
  # # Then, create a new data frame that contains the labels and their positions
  # label_data <- favelas_within_favela %>%
  #   dplyr::mutate(geometry = st_centroid(geometry)) %>%
  #   dplyr::select(nome, geometry) %>%
  #   st_as_sf()  # Ensure label_data is an sf object
  ###########################################################################################
  ###########################################################################################
  ###########################################################################################
  # # Perform extraction
  # extraction <- exact_extract(pred, favelas_within_favela, function(values, coverage_fraction) {
  #   t <- table(factor(values, levels = c(1, 2, 3)))
  #   return(t / sum(t) * 100)
  # })
  # 
  
  # # Transpose the extraction data to align rows with favelass and columns with categories
  # percentages_transposed <- t(extraction)
  # percentages <- as.data.frame(percentages_transposed)
  # 
  # # Now, set the column names correctly
  # colnames(percentages) <- c("Low", "Medium", "High")
  # 
  # # Ensure the favelas names match the number of rows in percentages
  # if (length(favelas_within_favela$nome) == nrow(percentages)) {
  #   percentages$favelas <- favelas_within_favela$nome
  # } else {
  #   warning("The number of favelass does not match the number of rows in the percentages data frame.")
  # }
  
  # # Reshaping the data to a long format using pivot_longer
  # percentages_long <- pivot_longer(
  #   percentages,
  #   cols = c("Low", "Medium", "High"),
  #   names_to = "Category",
  #   values_to = "Percentage"
  # )
  # percentages_long$Category <- factor(percentages_long$Category, levels = c("Low", "Medium", "High"), labels = c("Low", "Medium", "High"))
  # percentages_long$Category <- factor(percentages_long$Category, levels = rev(levels(percentages_long$Category)))
  # 
  # percentages_long$favelas <- gsub(" ", "\n", percentages_long$favelas)
  # 
  # # Now, plot the data with the reversed category order
  # GG_municip = ggplot(percentages_long, aes(x = favelas, y = Percentage, fill = Category)) +
  #   geom_bar(stat = "identity", position = "fill", width = 0.3, color= "darkgray", alpha = 0.4) +
  #   scale_fill_manual(
  #     values = c("High" = "red", "Medium" = "yellow", "Low" = "#008000"),
  #     name = "Categories", 
  #     labels = c("High", "Medium", "Low")
  #   ) +
  #   
  #   # geom_text(
  #   #   aes(label = paste0(round(Percentage), "%")),
  #   #   position = position_fill(vjust = 0.5), 
  #   #   color = "black", 
  #   #   size = 3.5
  #   # ) +
  #   coord_flip() +
  #   scale_y_continuous(breaks = c(0, 0.5, 1)) +
  #   theme_minimal() +
  #   theme(
  #     title = element_text(size = 10),
  #     legend.position = "none",
  #     axis.text.y = element_text(face = "bold")  # Bold y-axis text
  #   ) +
  #   labs(y = "", x = "", 
  #        title = "Proportional Landslide\nSusceptibility by favelas")
  
  
  
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
    
    geom_raster(data= na.omit(pred_df), aes(x = x,  y = y, fill = Susceptibility),alpha=0.4)+
    scale_fill_manual(values = c("#008000", "yellow", "red")) +
    ggnewscale::new_scale_fill()+
    
    
    geom_point(data=points_in, aes(x=x, y= y, color = "Landslide\nlocations"), shape = 17, size=1) +
    scale_color_manual(name = "", values = c("Landslide\nlocations" = "black")) + # Define the color and legend name for the points
    
    geom_sf(data = favela, fill = NA, color = "black", size=5) +
    
    #geom_sf(data = favelas_within_favela, fill = NA, color = "black", size=5) +
    #geom_sf_label(data = label_data, aes(label = nome), size = 1.5)+
    # geom_sf_label_repel(data = favelas_within_favela,
    #                     aes(label = nome, geometry = geometry),
    #                     size = 3,
    #                     box.padding = unit(0.5, "lines"),
    #                     point.padding = unit(0.5, "lines"))+
    #geom_sf_label(data = favelas_within_favela,aes(label = nome))+
    #ggtitle("Planning Regions (RP), Administrative Regions (RA), and Neighborhoods of the Municipality of Rio de Janeiro") +
    ggtitle(paste0("Predictions for ", save_name))+
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
             label = paste0("Number of observed landslides: ", n_slide," | "
                            ,"Inhabitants (2010): ", n_people, " | "
                            ,"Area (m²): ", round(area_favela,1), " | "
                            ,"Density: ", round(density,3), " people/m²"), 
             color = "black", size = 3)
  
  ###############################################################################################
  ###############################################################################################
  ###############################################################################################
  plot2=ggdraw() +
    draw_plot(plot2)+
    draw_image(logo_file,  x = .35, y = .45, scale = .15)
  
  
  
  mini_map <- ggplot(data = favelas) +
    geom_sf(fill = "white", color = "darkgray") + # All municipalities in white
    geom_sf(data = favelas[i,], fill = "black", color = "black", size=3) +
    
    ggspatial::annotation_north_arrow(location = "bl",
                                      pad_y = unit(0, "cm"),
                                      height = unit(.75, "cm"),
                                      width = unit(.5, "cm"),
                                      style = north_arrow_fancy_orienteering)+
    ggspatial::annotation_scale(location = "br",pad_y = unit(0, "cm"),pad_x = unit(0, "cm"))+
    theme_minimal()+
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
  
  
  # plot_left=cowplot::plot_grid(mini_map, NULL,#histogram1,histogram2,# histogram2,
  #                              ncol = 1, nrow = 2,
  #                              rel_heights = c(1, 3))
  # 
  # plot2=cowplot::plot_grid(plot_left, plot2,
  #                          ncol = 2, nrow = 1,
  #                          rel_widths = c(0.4,1))+
  #   theme(
  #     panel.background = element_rect(fill = "white"), # Set panel background to white
  #     plot.background = element_rect(fill = "white"),  # Set plot background to white
  #     legend.background = element_rect(fill = "white") # Set legend background to white
  #   )
  # 
  
  
  # # Export the percentage data to a .txt file
  # if (i == 1) {
  #   # If it's the first iteration, write a new file with header
  #   write.table(percentages, file = output_file, sep = "\t", row.names = FALSE, col.names = TRUE)
  # } else {
  #   # If it's not the first iteration, append the data without header
  #   write.table(percentages, file = output_file, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE)
  # }
  # 
  
  
  
  
  
  
  # Save the figure as a PNG file
  filename <- paste0(i, "_", save_name, ".png")
  ggsave(filename, plot2, width = 12, height = 9)
  # Print the filename for reference
  cat("Saved:", filename, "\n")
}

# Reset the working directory to its original value if needed
# setwd("..")



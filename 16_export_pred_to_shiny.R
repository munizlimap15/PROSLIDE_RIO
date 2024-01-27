library(raster)
pred = raster("D:/PROslide_RIO/Susc_heuristic/suscetibilidade_rio.tif")

study_area          <- sf::st_read("D:/PROslide_RIO/Rcodes/Shinny_app_RioSlide/StudyArea.shp")

# Ratify the raster to make the RAT accessible
pred_ratified <- ratify(pred)

# Load the RAT (Raster Attribute Table)
rat <- levels(pred)[[1]]

# Ensure the RAT is correctly retrieved
if (!is.null(rat) && "suscetibil" %in% colnames(rat)) {
  
  # Create a factor variable using the 'suscetibil' attribute
  suscetibil_values <- rat$suscetibil[match(pred[], rat$ID)]
  
  # Convert the raster to a factor raster
  pred_factor <- ratify(pred)
  levels(pred_factor)[[1]] <- data.frame(ID = 1:length(unique(suscetibil_values)), suscetibil = unique(suscetibil_values))
  pred_factor[] <- as.integer(factor(suscetibil_values))
  
  # Set names, crs, and other properties if needed
  names(pred_factor) <- "suscetibil"
  crs(pred_factor) <- crs(pred)
  pred_factor <- aggregate(pred_factor, fact=5, fun=modal)
  pred_factor <- mask(pred_factor, study_area)
  # Save the raster with 'suscetibil' categories
  writeRaster(pred_factor, filename="D:/PROslide_RIO/Rcodes/Shinny_app_RioSlide/suscetibilidade_rio.tif", format="GTiff", overwrite=TRUE)
} else {
  cat("The attribute 'suscetibil' does not exist in the raster data, or the RAT is not properly loaded.")
}


######################################################################
######################################################################
######################################################################
######################################################################

# Read the shapefile using sf package
rioslides <- sf::st_read("D:/PROslide_RIO/DATA/landslides_2023.shp")
summary(as.factor(rioslides$tipologia1))
#0    1    2    3    4    5    6    7    8    9   10   11 
#59 1659   32   25    1  323   55  449   12   71  155  157 

rioslides <- rioslides %>%
  dplyr::filter(tipologia1 == 1 | tipologia2 == 1 | tipologia3 == 1 | tipologia4 == 1)

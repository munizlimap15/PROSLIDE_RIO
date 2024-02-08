library(raster)
library(dplyr)
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
  pred_factor <- aggregate(pred_factor, fact=10, fun=modal)
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

pred2 = raster("D:/PROslide_RIO/DATA2/pred_megam_class.tif")
# Ratify the raster to make the RAT accessible
pred_ratified2 <- ratify(pred2)

# Define a data frame with ID and the corresponding 'suscetibil' values
rat <- data.frame(ID = c(1, 2, 3), 
                  suscetibil = c('Low', 'Medium', 'High'))

# Assign this RAT to the ratified raster
levels(pred_ratified2) <- list(rat)

# Ensure the RAT is correctly retrieved
if (!is.null(rat) && "suscetibil" %in% colnames(rat)) {
  
  # Create a factor variable using the 'suscetibil' attribute
  suscetibil_values <- rat$suscetibil[match(pred2[], rat$ID)]
  
  # Convert the raster to a factor raster
  pred2_factor <- ratify(pred2)
  levels(pred2_factor)[[1]] <- data.frame(ID = 1:length(unique(suscetibil_values)), suscetibil = unique(suscetibil_values))
  pred2_factor[] <- as.integer(factor(suscetibil_values))
  
  # Set names, crs, and other properties if needed
  names(pred2_factor) <- "suscetibil"
  crs(pred2_factor) <- crs(pred2)
  pred2_factor <- aggregate(pred2_factor, fact=10, fun=modal)
  pred2_factor <- mask(pred2_factor, study_area)
  # Save the raster with 'suscetibil' categories
  writeRaster(pred2_factor, filename="D:/PROslide_RIO/Rcodes/Shinny_app_RioSlide/suscetibilidade_DD_rio.tif", format="GTiff", overwrite=TRUE)
} else {
  cat("The attribute 'suscetibil' does not exist in the raster data, or the RAT is not properly loaded.")
}

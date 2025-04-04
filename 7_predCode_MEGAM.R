# Load required libraries
library(raster)
library(mgcv)
library(gamm4)  # Load the gamm4 package
library(dplyr)
#install.packages("Matrix")
library(Matrix)


setwd("D:/PROslide_RIO/DATA2")

# Load the training dataset
load("D:/PROslide_RIO/DATA/final_train.Rd")
# Subset the data where slide = 1 (TRUE)
final_train_subset <- subset(final_train, slide == TRUE)

final_train_t <- final_train %>%
  filter((slide == TRUE & slope >= 1))

# Remove rows where slide == TRUE and slope <= 5
final_train_f <- final_train %>%
  filter((slide == FALSE)) %>% sample_n(nrow(final_train_t))

final_train=rbind(final_train_f, final_train_t)
# Show the summary of the subsetted data
summary(final_train_subset)
write.csv(final_train_subset, "D:/PROslide_RIO/DATA2/slides.csv", row.names = FALSE)



# Convert numerical variables to factors if needed
final_train$landcover19 <- as.factor(final_train$landcover19)
final_train$geomorph <- as.factor(final_train$geomorph)
final_train$geol <- as.factor(final_train$geol)




reclass_geol <- function(x) {
  ifelse(x %in% c(1, 2, 5, 8, 15), 1,  # Igneous Rocks
         ifelse(x %in% c(3, 6, 9, 10, 11, 12, 13, 14), 2,  # Metamorphic Rocks
                ifelse(x%in% c(4,7,16, 255), 3, NA)  # Sedimentary Rocks, # Anthropogenic Materials and # Unconsolidated Sediments
                       #ifelse(x == 7, 4,  
                              #ifelse(x == 16, 5, NA)  
                       ))}

# Apply the reclassification function to the 'geol' column
final_train$geol_rec <- sapply(final_train$geol, reclass_geol)

# Convert the reclassified geology to a factor
final_train$geol_rec <- as.factor(final_train$geol_rec)
summary(as.factor(final_train$geol_rec))

final_train$geol = NULL

##############################################
##############################################
# Manipulating the Geological raster 
##############################################
##############################################

# Load the original geology raster
geol_raster <- raster("geol.asc")

# Define the reclassification matrix
reclass_matrix <- matrix(c(1, 1,
                           2, 1,
                           5, 1,
                           8, 1,
                           15, 1,
                           3, 2,
                           6, 2,
                           9, 2,
                           10, 2,
                           11, 2,
                           12, 2,
                           13, 2,
                           14, 2,
                           4, 3,
                           7, 3,
                           16, 3,
                           17, 3), 
                         ncol=2, byrow=TRUE)


# Reclassify the raster
geol_rec <- reclassify(geol_raster, reclass_matrix, right=NA)

# Set the names for the reclassified raster
names(geol_rec) <- "geol_rec"

# Save the reclassified raster to disk
#writeRaster(geol_rec, filename="geol_rec.asc", format="ascii", overwrite=TRUE)

##############################################
##############################################
# Manipulating the Geomorph raster 
##############################################
##############################################

# Function to reclassify geomorphological categories
reclass_geomorph <- function(x) {
  ifelse(x %in% c(1, 2), 1,  # Concave Slopes
         ifelse(x %in% c(3, 4), 2,  # Convex Slopes
                ifelse(x == 5, 3,  # Flat Areas
                       ifelse(x %in% c(6, 8), 4,  # Escarpments
                              ifelse(x == 9, 6,  # Valley Bottoms
                                     ifelse(x %in% c(7, 255), 5, NA)  # Elevation Tops and Rare Categories
                              )))))
}

# Apply the reclassification function to the 'geomorph' column
final_train$geomorph_rec <- sapply(final_train$geomorph, reclass_geomorph)

# Convert the reclassified geomorphology to a factor
final_train$geomorph_rec <- as.factor(final_train$geomorph_rec)
summary(as.factor(final_train$geomorph_rec))

# Remove the original geomorph column if no longer needed
final_train$geomorph = NULL

##############################################
# Manipulating the Geomorphological raster 
##############################################

# Load the original geomorphological raster
geomorph_raster <- raster("geomorph.asc")

# Define the reclassification matrix for the geomorphological raster
geomorph_reclass_matrix <- matrix(c(1, 1,
                                    2, 1,
                                    3, 2,
                                    4, 2,
                                    5, 3,
                                    6, 4,
                                    8, 4,
                                    9, 6,
                                    7, 5,
                                    255, 5), 
                                  ncol=2, byrow=TRUE)

# Reclassify the raster
geomorph_rec <- reclassify(geomorph_raster, geomorph_reclass_matrix, right=NA)

# Set the names for the reclassified raster
names(geomorph_rec) <- "geomorph_rec"

# Save the reclassified raster to disk
#writeRaster(geomorph_rec, filename="geomorph_rec.asc", format="ascii", overwrite=TRUE)














##############################################
##############################################
# Manipulating the Landcover raster to favelas and residential areas to be random intercepts 
##############################################
##############################################

landcover_raster <- raster("landcover19.asc")
# Reclassify the landcover raster to binary values
# Favela/residential = 1; No favela/no residential = 2
reclass_matrix <- matrix(c(5, 1,  # Favela
                           6, 1,  # Residential areas
                           1, 2,  # Other land covers
                           2, 2,
                           3, 2,
                           4, 2,
                           7, 2,
                           8, 2), 
                         ncol = 2, byrow = TRUE)
fav_res <- reclassify(landcover_raster, reclass_matrix, right=NA)

# Set the names for the reclassified raster
names(fav_res) <- "fav_res"
# Export the reclassified raster
#writeRaster(fav_res, filename="fav_res.asc", format="ascii", overwrite=TRUE)

# Add a new column to final_train with the binary values
final_train$fav_res <- ifelse(final_train$landcover19 %in% c(5, 6), 1, 2)

# Convert the new fav_res column to a factor if needed for modeling
final_train$fav_res <- as.factor(final_train$fav_res)
summary(as.factor(final_train$fav_res))

##############################################
##############################################
# Prediction
##############################################
##############################################


# Define the formula for the mixed-effects GAM model
# Include (1|fav_res) to model as a random effect
fo1 <- slide ~ s(dtm) + s(prof_curv) + s(slope)  + geomorph_rec + geol_rec + fav_res #+ landcover19


# Load raster layers as a stack or brick
rasters_list1 <- stack("dtm.asc", "prof_curv.asc", "slope.asc", "geomorph_rec.asc", "geol_rec.asc", "fav_res.asc") #, "landcover19.asc"
# Make predictions
output_filename1 <- "pred_megam.tif"

# Use the predict() function from the gamm4 package
# predict(rasters_list1, gam_fit1$gam, filename=output_filename1, 
#         progress='text', overwrite=TRUE, type="response")
# Fit the mixed-effects GAM model with different optimizer and increased iterations
# gam_fit1 <- gamm4(fo1, random = ~(1|landcover19), data = final_train, family = binomial,
#                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e4)))

# Fit the mixed-effects GAM model with 'fav_res' as a random effect
#https://stackoverflow.com/questions/77481539/error-in-initializeptr-function-cholmod-factor-ldeta-not-provided-by-pack
gam_fit1 <- gamm4(fo1, random = ~(1|fav_res), data = final_train, family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e4)))


# Validate the model
summary(gam_fit1$gam)


predict(rasters_list1, gam_fit1$gam, filename=output_filename1, 
        progress='text', overwrite=TRUE, type="response")


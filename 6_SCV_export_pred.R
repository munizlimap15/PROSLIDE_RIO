# Load necessary libraries
library(pROC)
library(sperrorest)
library(gam)
library(dplyr)
library(tidyr)
library(raster)
library(RSAGA)
library(glmnet)

# Set working directory
setwd("C:/Users/pedro/Documents/PROslide_RIO/DATA")

# Load the training dataset
load("C:/Users/pedro/Documents/PROslide_RIO/DATA/final_train.Rd")
# Define a function to load a raster and set its CRS
load_and_set_crs <- function(file_name, crs_string) {
  if (!file.exists(file_name)) {
    stop(paste("The file", file_name, "does not exist in the current directory."))
  }
  r <- raster(file_name)
  crs(r) <- crs_string
  return(r)
}

# CRS string
crs_string <- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"

# Load raster files and set CRS
dtm              <- load_and_set_crs("dtm.asc", crs_string)
aspect           <- load_and_set_crs("aspect.asc", crs_string)
plan_curv        <- load_and_set_crs("plan_curv.asc", crs_string)
prof_curv        <- load_and_set_crs("prof_curv.asc", crs_string)
rel_slp_position <- load_and_set_crs("rel_slp_position.asc", crs_string)
slope            <- load_and_set_crs("slope.asc", crs_string)
twi              <- load_and_set_crs("twi.asc", crs_string)
tpi              <- load_and_set_crs("tpi.asc", crs_string)
landcover19      <- load_and_set_crs("landcover19.asc", crs_string)
geomorph         <- load_and_set_crs("geomorph.asc", crs_string)
geol             <- load_and_set_crs("geol.asc", crs_string)

# Display summary statistics of the training dataset
summary(final_train)
summary(as.factor(final_train$geol))
summary(as.factor(final_train$geomorph))
summary(as.factor(final_train$aspect))
summary(as.factor(final_train$tpi))

# Data transformation function
my.trafo = function(x) {
  x$geomorph = as.factor(x$geomorph)
  x$geol = as.factor(x$geol)
  x$tpi = as.factor(x$tpi)
  x$landcover19 = as.factor(x$landcover19)
  return(x)
}

# Apply the transformation to the training dataset
final_train = my.trafo(final_train)

# Remove unnecessary columns
final_train$aspect_class = NULL
final_train$tpi_class = NULL
final_train$landcover19_reclass = NULL

# Summary after transformation
summary(final_train)


# Define the formula for the GAM model
fo1 <- slide ~ s(dtm) + s(prof_curv) + s(slope) + landcover19 + geomorph + geol + tpi

# Fit the GAM model
gam_fit <- gam(fo1, data = final_train, family = binomial)

# Perform cross-validation using sperrorest
cv1 = sperrorest(formula=fo1, data=final_train, coords=c("x","y"),
                 model_fun= gam, model_args = list(family=binomial),
                 pred_args=list(type="response"), 
                 smp_fun=partition_cv, smp_args=list(repetition=25, nfold=5, seed1 = 1))

# Display AUROC results
summary(mean(cv1$error_rep$test_auroc))
mean(cv1$error_rep$test_auroc)

# Apply the model to multiple raster layers
multi.local.function(
    #in.grids = c("dtm", "prof_curv", "slope", "landcover19", "geomorph", "geol", "tpi"),  
    #in.grids = c("dtm.asc", "prof_curv.asc", "slope.asc", "landcover19.asc", "geomorph.asc", "geol.asc", "tpi.asc"), 
    in.grids = c("dtm.asc", "landcover19.asc", "geomorph.asc", "geol.asc"),
    out.varnames = "gam_RIO",
    fun = grid.predict, 
    control.predict = list(type = "response"),
    fit = gam_fit,  
    trafo = my.trafo, 
    quiet = FALSE 
)

gam_RIO             <- raster("gam_RIO.asc")


# Set the CRS for the raster
crs(gam_RIO) <- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"

# Save the raster to disk
writeRaster(gam_RIO, filename = "C:/Users/pedro/Documents/PROslide_RIO/DATA/gam_RIO.tif", format = "GTiff", overwrite = TRUE)


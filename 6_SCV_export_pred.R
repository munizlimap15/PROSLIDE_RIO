# Load necessary libraries
library(pROC)
library(sperrorest)
library(gam)
library(dplyr)
library(tidyr)
library(raster)
library(RSAGA)
library(glmnet)
library(ggplot2)
library(patchwork)

setwd("D:/PROslide_RIO/DATA2")

# Load the training dataset
load("D:/PROslide_RIO/DATA/final_train.Rd")

# Remove rows where slide == TRUE and slope <= 5
final_train_t <- final_train %>%
  filter((slide == TRUE & slope >= 5))

# Remove rows where slide == TRUE and slope <= 5
final_train_f <- final_train %>%
  filter((slide == FALSE)) %>% sample_n(nrow(final_train_t))

final_train=rbind(final_train_f, final_train_t)

#final_train$tpi=final_train$tpi
#final_train$tpi2=NULL
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
tpi              <- load_and_set_crs("tpi_class.asc", crs_string)
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
  print("Inside my.trafo, column names are:")
  print(colnames(x))
  x$tpi = as.factor(x$tpi)
  x$landcover19 = as.factor(x$landcover19)
  x$geomorph = as.factor(x$geomorph)
  x$geol = as.factor(x$geol)
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

# Perform 25-fold cross-validation using sperrorest
cv1 <- sperrorest(
  formula = fo1, data = final_train, coords = c("x", "y"),
  model_fun = gam, model_args = list(family = binomial),
  pred_args = list(type = "response"), 
  smp_fun = partition_cv, smp_args = list(repetition = 5, nfold = 25, seed1 = 1)
)

# Display the structure of cv1
str(cv1)

# Display AUROC results
summary(mean(cv1$error_rep$test_auroc))
mean(cv1$error_rep$test_auroc)

# Extract the test AUROC values
test_auroc_values <- cv1$error_rep$test_auroc

# Calculate the median AUROC value
median_auroc <- median(test_auroc_values)

# Extract true labels for each fold
true_labels_list <- lapply(1:25, function(i) {
  test_indices <- cv1$represampling[[1]][[i]]$test
  final_train$slide[test_indices]
})

# Regenerate the predictions for each fold
predicted_probs_list <- lapply(1:25, function(i) {
  test_indices <- cv1$represampling[[1]][[i]]$test
  test_data <- final_train[test_indices, ]
  
  # Generate predictions using the fitted model
  predict(gam_fit, newdata = test_data, type = "response")
})

# Ensure predictions and true labels match in length
if (length(predicted_probs_list) != length(true_labels_list)) {
  stop("Mismatch between the number of folds in predictions and true labels.")
}

# Generate ROC curves for each fold and smooth them
roc_curves <- lapply(1:25, function(i) {
  true_labels <- true_labels_list[[i]]
  predicted_probs <- predicted_probs_list[[i]]
  
  # Generate ROC curve and smooth it
  roc_curve <- roc(true_labels, predicted_probs)
  smooth(roc_curve)
})

# Find the ROC curve closest to the median AUROC
median_roc_curve <- roc_curves[[which.min(abs(test_auroc_values - median_auroc))]]

# Extract data for all ROC curves
roc_data_list <- lapply(roc_curves, function(roc) {
  as.data.frame(roc[c("specificities", "sensitivities")])
})

# Combine all ROC data into one dataframe
roc_data <- bind_rows(roc_data_list, .id = "fold")

# Extract data for the median ROC curve
median_roc_data <- as.data.frame(median_roc_curve[c("specificities", "sensitivities")])

# Plot ROC curves
roc_plot <- ggplot() +
  geom_line(data = roc_data, aes(x = 1 - specificities, y = sensitivities, group = fold), color = "gray") +
  geom_line(data = median_roc_data, aes(x = 1 - specificities, y = sensitivities), color = "black", size = 1.2) +
  
  labs(x = "False Positive Rate", y = "True Positive Rate") +
  ggtitle("Smoothed ROC Curves for 25-fold Cross-Validation") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18)
  ) +
  
  annotate("text", x = 0.5, y = 0.1, label = paste("Median AUROC=", round(median_auroc, 1)), size = 8, color = "black")

# Display the plot
print(roc_plot)

png("D:/PROslide_RIO/Figs/roc_plot.png", width = 1000, height = 900)
print(roc_plot)
dev.off()









# Apply the model to multiple raster layers
multi.local.function(
    in.grids = c("dtm", "prof_curv", "slope", "landcover19", "geomorph", "geol", "tpi_class"),  # Included "tpi_class.asc"
    out.varnames = "gam_RIO",
    fun = grid.predict, 
    control.predict = list(type = "response"),
    fit = gam_fit,  
    trafo = my.trafo,  # Make sure my.trafo accounts for "tpi"
    quiet = FALSE 
)


gam_RIO             <- raster("gam_RIO.asc")


# Set the CRS for the raster
crs(gam_RIO) <- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"

# Save the raster to disk
writeRaster(gam_RIO, filename = "C:/Users/pedro/Documents/PROslide_RIO/DATA/gam_RIO.tif", format = "GTiff", overwrite = TRUE)


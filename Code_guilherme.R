# Load required libraries
library(raster)
library(mgcv)
library(dplyr)
library(ggplot2)
library(gridExtra)  # For arranging plots side by side
library(viridis)

setwd("D:/PROslide_RIO/DATA2")
# 1. Load Pre-Processed Data (replace path as needed)
load("D:/PROslide_RIO/DATA2/final_train.Rd")
summary(final_train$slide)
# Convert landcover19 to factor if not already
final_train$landcover19 <- as.factor(final_train$landcover19)
final_train$geol        <- as.factor(final_train$geol)
#final_train$geomorph <- as.factor(final_train$geomorph)


final_train <- final_train #%>%  filter( slope >= 5)  #geomorph != 5 & #5 = Área Plana
summary(final_train$slide)



# Count the number of TRUE and FALSE in the 'slide' column
n_true <- sum(final_train$slide == "TRUE")
n_false <- sum(final_train$slide == "FALSE")

# Get the minimum count between TRUE and FALSE
n_min <- min(n_true, n_false)

# Randomly sample the same number of TRUE and FALSE rows
final_train <- final_train %>%
  group_by(slide) %>%
  sample_n(n_min) %>%
  ungroup()




summary(final_train$landcover19)



# Fit a GAM model with only slope, dtm, and landcover19 as predictors
# Other variables are commented for future use
# gam_model <- gam(slide ~ s(slope, k = 8) + s(dtm, k = 2) + landcover19,
#                  data = final_train, family = binomial)
gam_model <- gam(slide ~ s(slope, k = 2) + dtm,# + landcover19,#geol + ,#s(dtm, k = 5)
                 data = final_train, family = binomial)
# # Full model (commented)
# fo1 <- slide ~ s(dtm) + s(prof_curv) + s(slope) + geomorph_rec + geol + fav_res

# 2. Predict landslide susceptibility for the dataset
final_train <- final_train %>%
  mutate(prediction = predict(gam_model, newdata = final_train, type = "response"))
# 3. Validate the model
summary(gam_model)


# 1: Rocky Outcrops and Sedimentary Deposits
# 2: Forest and Shrub Cover
# 3: Grass and Woody Cover
# 4: Water Bodies (includes categories 4 and 16)
# 5: Favela (Slum Areas)
# 6: Residential Areas (category 15)
# 7: Agricultural and Undeveloped Areas (includes categories 6 and 14)
# 8: Industrial, Commercial, and Public Infrastructure (includes categories 7, 8, 9, 10, 11, 12, 13)
#


# Combine TRUE and FALSE slide data into one plot, differentiated by shape
# Plot for slope vs prediction
plot_slope <- ggplot(final_train, aes(x = slope, y = prediction, shape = slide)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(method = "gam", formula = y ~ s(x), color = "red", aes(group = 1)) +  # Single smooth line for prediction trend
  labs(title = "Predicted Landslide Susceptibility Based on Slope",
       x = "Slope",
       y = "Predicted Probability of Landslide") +
  theme_minimal() #+
#color_mapping +   # Apply continuous viridis color mapping based on prediction values
#shape_mapping     # Apply shape mapping based on slide (TRUE/FALSE)

# Plot for dtm vs prediction
plot_dtm <- ggplot(final_train, aes(x = dtm, y = prediction, shape = slide)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(method = "gam", formula = y ~ s(x), color = "red", aes(group = 1)) +  # Single smooth line for prediction trend
  labs(title = "Predicted Landslide Susceptibility Based on DTM",
       x = "DTM (Elevation)",
       y = "Predicted Probability of Landslide") +
  theme_minimal() #+
#color_mapping +   # Apply continuous viridis color mapping based on prediction values
#shape_mapping     # Apply shape mapping based on slide (TRUE/FALSE)

# Arrange the two plots side by side with a unified legend
grid.arrange(plot_slope, plot_dtm, ncol = 2)


#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################


# 4. Stack the aligned rasters
rasters_list <- stack("dtm.asc", "slope.asc") # Add additional layers if needed


# Load the mask (ensure it has 0/1 values or adjust it)
mask_raster <- raster("F:/PROslide_RIO/GUILHERME/slp_less_5_foc.tif")  # Replace with the actual path to your mask
mask_raster[mask_raster != 0] <- NA  # Keep only pixels with value 0 as valid

# Apply the mask to each layer in the stack
masked_stack <- mask(rasters_list, mask_raster)




# 9. Predict landslide susceptibility across the rasters
output_filename <- "pred_dtm_slp2_plus5.tif"
temp_output <- "temp_pred_dtm_slp2_plus5.tif" # Temporary file for unrounded predictions

# Make predictions and save to a temporary file
predict(masked_stack, gam_model, filename = temp_output,
        progress = 'text', overwrite = TRUE, type = "response")

# Load the temporary raster file
pred_raster <- raster(temp_output)

# Round raster values to 4 decimal places
values(pred_raster) <- round(values(pred_raster), 4)

# Save the rounded raster to the final output file
writeRaster(pred_raster, filename = output_filename, format = "GTiff", overwrite = TRUE)

# Optionally, delete the temporary file
file.remove(temp_output)
#######################################################################################
#######################################################################################
#######################################################################################
# Calculate quantile breaks (5 quantiles)

quantile_breaks <- quantile(values(pred_raster), probs = seq(0, 1, by = 0.2), na.rm = TRUE)
quantile_breaks
#0%    20%    40%    60%    80%   100% 
#  0.0001 0.2699 0.4617 0.6352 0.7645 0.9491 

# Assign quantile classes to each pixel
quantile_classes <- cut(values(pred_raster), 
                        breaks = quantile_breaks, 
                        include.lowest = TRUE, 
                        labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))

# Create a new raster for quantile classes
quantile_raster <- pred_raster
values(quantile_raster) <- as.integer(quantile_classes)  # Convert factor to numeric


#quantile_output <- "pred_dtm_slp2_plus5_quant.tif"
#writeRaster(quantile_raster, filename = quantile_output, format = "GTiff", overwrite = TRUE)

# Define a focal matrix (e.g., 3x3 moving window)
focal_matrix <- matrix(1, nrow = 21, ncol = 21)

# Apply focal statistics (e.g., mean)
focal <- focal(quantile_raster, w = focal_matrix, fun = modal , na.rm = TRUE)

# Save the output raster
writeRaster(focal, "pred_dtm_slp2_plus5_quant_focal.tif", format = "GTiff", overwrite = TRUE)

#######################################################################################
#######################################################################################
#######################################################################################
# Calculate quantile breaks (3 quantiles)

quantile_breaks <- quantile(values(pred_raster), probs = seq(0, 1, by = 0.33), na.rm = TRUE)
quantile_breaks
#0%    33%    66%    99% 
#  0.0002 0.4003 0.6727 0.8588 

# Assign quantile classes to each pixel
quantile_classes <- cut(values(pred_raster), 
                        breaks = quantile_breaks, 
                        include.lowest = TRUE, 
                        labels = c("Q1", "Q2", "Q3"))

# Create a new raster for quantile classes
quantile_raster <- pred_raster
values(quantile_raster) <- as.integer(quantile_classes)  # Convert factor to numeric


#quantile_output <- "pred_dtm_slp2_plus5_quant.tif"
#writeRaster(quantile_raster, filename = quantile_output, format = "GTiff", overwrite = TRUE)

# Define a focal matrix (e.g., 3x3 moving window)
focal_matrix <- matrix(1, nrow = 21, ncol = 21)

# Apply focal statistics (e.g., mean)
focal <- focal(quantile_raster, w = focal_matrix, fun = modal , na.rm = TRUE)

# Save the output raster
writeRaster(focal, "pred_dtm_slp2_plus5_3quant_focal.tif", format = "GTiff", overwrite = TRUE)

# Save the quantile classes raster to an output file


#######################################################################################
#######################################################################################
#######################################################################################






# Ajustar o modelo GAM considerando apenas a variável slope como quadrática
gam_model_slope <- gam(slide ~ s(slope, k = 2), data = final_train, family = binomial)

# 4. Stack com apenas o raster de slope
rasters_list <- stack("slope.asc")

# Definir o nome dos arquivos de saída
output_filename <- "pred_slope2_only.tif"
temp_output <- "temp_pred_slope_only.tif"  # Arquivo temporário para previsões não arredondadas

# Fazer previsões e salvar em um arquivo temporário
predict(rasters_list, gam_model_slope, filename = temp_output,
        progress = 'text', overwrite = TRUE, type = "response")

# Carregar o arquivo raster temporário
pred_raster <- raster(temp_output)

# Arredondar os valores do raster para 4 casas decimais
values(pred_raster) <- round(values(pred_raster), 4)

# Salvar o raster arredondado no arquivo final de saída
writeRaster(pred_raster, filename = output_filename, format = "GTiff", overwrite = TRUE)

# Opcionalmente, excluir o arquivo temporário
file.remove(temp_output)




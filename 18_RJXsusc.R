library(sf)
library(raster)
library(dplyr)
library(sp)
library(dplyr)

study_area          <- sf::st_read("D:/PROslide_RIO/Rcodes/Shinny_app_RioSlide/StudyArea.shp")

pred                = raster("D:/PROslide_RIO/Susc_heuristic/suscetibilidade_rio.tif")
pred2               = raster("D:/PROslide_RIO/DATA2/pred_megam_class.tif")
# Resample pred2 to match the extent and resolution of pred

# Check resolutions
print(res(pred))
print(res(pred2))

#2. Check and Match Resolutions

# Resample pred2 to match the resolution of pred, if necessary
if (!identical(res(pred), res(pred2))) {
  pred2_resampled <- resample(pred2, pred, method = "ngb")
} else {
  pred2_resampled <- pred2
}

# 3. Define and Verify the Common Extent

# Define the common extent manually
xmin_common <- max(extent(pred)@xmin, extent(pred2_resampled)@xmin)
xmax_common <- min(extent(pred)@xmax, extent(pred2_resampled)@xmax)
ymin_common <- max(extent(pred)@ymin, extent(pred2_resampled)@ymin)
ymax_common <- min(extent(pred)@ymax, extent(pred2_resampled)@ymax)

common_extent <- extent(xmin_common, xmax_common, ymin_common, ymax_common)

# Print the common_extent to verify
print(common_extent)

# 4. Crop the Rasters
# Crop both rasters to the common extent
pred_cropped <- crop(pred, common_extent)
pred2_resampled_cropped <- crop(pred2_resampled, common_extent)


#5. Create the Raster Stack
# Stack the cropped rasters
stacked_rasters <- stack(pred_cropped, pred2_resampled_cropped)


reclass_matrix <- matrix(c(0, NA, 255, NA), ncol = 2, byrow = TRUE)

# Reclassify the 'count_' layer (assuming this is the layer with 0 and 255 values)
count_reclassified <- reclassify(stacked_rasters[['count_']], reclass_matrix)

# Replace the original 'count_' layer in the stack with the reclassified one
stacked_rasters[['count_']] <- count_reclassified

# Check the updated stack
stacked_rasters


# Define factor levels for the new values
rcl <- data.frame(ID = c(1, 2, 3),
                  level = c("Baixo", "Medio", "Alto"))

# Apply factor levels to the raster
pred <- ratify(pred)
pred2<- ratify(pred2)
levels(pred) <- list(rcl)
levels(pred2) <- list(rcl)















































############################################################################
############################################################################
############################################################################
# Define the categories for each layer
categories_count <- c("low", "medium", "high")
categories_pred_megam <- c("low", "medium", "high")

# Create data frames for factor levels
factor_levels_pred_megam <- data.frame(ID = 1:length(categories_pred_megam), 
                                       category = as.factor(categories_pred_megam))

factor_levels_count <- data.frame(ID = 1:length(categories_count), 
                                  category = as.factor(categories_count))

# Set the raster values as factors
stacked_rasters$pred_megam_class <- ratify(stacked_rasters$pred_megam_class)
stacked_rasters$count_ <- ratify(stacked_rasters$count_)

# Assign levels using the data frames
levels(stacked_rasters$pred_megam_class) <- list(factor_levels_pred_megam)
levels(stacked_rasters$count_) <- list(factor_levels_count)



# Extracting cell numbers from predictions
pred <- raster::extract(stacked_rasters, study_area, df = TRUE)
pred <- na.omit(pred)



# Count the number of pixels with different susceptibility levels within each Favela polygon
pixel_counts_pred <- pred %>%
  group_by(count_) %>%
  summarize(Count = n()) %>%
  mutate(Total = sum(Count, na.rm = TRUE),  # Sum excluding NAs
         Percentage = (Count / Total) * 100)  %>%
  rename(Susceptibility = count_)%>%
  mutate(Model = "Oficial LSM")

pixel_counts_pred2 <- pred %>%
  group_by(pred_megam_class) %>%
  summarize(Count = n()) %>%
  mutate(Total = sum(Count, na.rm = TRUE),  # Sum excluding NAs
         Percentage = (Count / Total) * 100)  %>%
  rename(Susceptibility = pred_megam_class)%>%
  mutate(Model = "Data-driven")

# Print the pixel counts
print(pixel_counts_pred)
print(pixel_counts_pred2)


# Now combine the data
combined_fav_data <- rbind(pixel_counts_pred, pixel_counts_pred2)




# ggplot(na.omit(combined_fav_data), aes(x="", y=Percentage, fill=factor(Susceptibility))) +
#   geom_bar(width = 1, stat = "identity") +
#   coord_polar("y", start=0) +
#   geom_label(aes(label=sprintf("%0.0f%%", Percentage)), position=position_stack(vjust=0.5)) +
#   scale_fill_manual(values=c("#008000", "yellow", "red"), 
#                     name="Categories", 
#                     labels=c("Low", "Medium", "High")) +
#   theme_void() +
#   labs(fill="Count") + 
#   guides(fill = guide_legend(override.aes = aes(label = ""))) +
#   facet_grid(~ Model)
# 
# 


gg_Municip2=ggplot(na.omit(pixel_counts_pred), aes(x="", y=Percentage, fill=factor(Susceptibility))) +
  geom_bar(width = 1, stat = "identity",alpha = 7/10) +
  coord_polar("y", start=0) +
  geom_label(aes(label=sprintf("%0.0f%%", Percentage)), position=position_stack(vjust=0.5)) +
  scale_fill_manual(values=c("#008000", "yellow", "red"), 
                    name="Categories", 
                    labels=c("Low", "Medium", "High")) +
  #theme_void() +
  theme(axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank()) +  # Remove panel border
  labs(fill="Count", title="Prediction distribution\ninside the favelas") + 
  guides(fill = guide_legend(override.aes = aes(label = "")))




gg_DD2=ggplot(na.omit(pixel_counts_pred2), aes(x="", y=Percentage, fill=factor(Susceptibility))) +
  geom_bar(width = 1, stat = "identity",alpha = 7/10) +
  coord_polar("y", start=0) +
  geom_label(aes(label=sprintf("%0.0f%%", Percentage)), position=position_stack(vjust=0.5)) +
  scale_fill_manual(values=c("gray", "#987645", "#65828D"), 
                    name="Categories", 
                    labels=c("Low", "Medium", "High")) +
  #theme_void() +
  theme(axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank()) +  # Remove panel border
  labs(fill="Count", title="Prediction distribution\ninside the favelas") +  
  guides(fill = guide_legend(override.aes = aes(label = "")))


################################################################################
################################################################################
################################################################################
################################################################################
# Open a graphics device to save the plot
png("D:/PROslide_RIO/Figs/Susc_official_favelas_1508_930.png", width = 1508, height = 930, units = "px")
# Draw the main plot
print(gg_Municip)

# Define a viewport for the inset plot
vp <- viewport(x = 0, y = 0.9, width = 0.5, height = 0.5, just = c("left", "top"))

# Print the inset plot in the defined viewport
print(gg_Municip2+theme(legend.position="none"), vp = vp)
dev.off()
################################################################################
################################################################################
################################################################################
png("D:/PROslide_RIO/Figs/Susc_DD_favelas_1508_930.png", width = 1508, height = 930, units = "px")
# Draw the main plot
print(gg_DD)

# Define a viewport for the inset plot
vp <- viewport(x = 0, y = 0.9, width = 0.5, height = 0.5, just = c("left", "top"))

# Print the inset plot in the defined viewport
print(gg_DD2+theme(legend.position="none"), vp = vp)
dev.off()











mapview::mapview(Limite_Favelas_2019)

# objectid 	605 
# cod_favela 	43 
# nome 	Rocinha 
# situacao 	2 
# cod_comple 	0 
# complexo 	Isolada 
# data_cadas 	1982-05-26T00:00:00 
# cod_bairro 	154 
# bairro 	Rocinha 
# cod_ra 	27 
# ra 	Rocinha 
# cod_rp 	2.1 
# rp 	Zona Sul 
# cod_ap 	2 
# pop_sabren 	69156 
# dom_sabren 	23347 
# fonte 	IBGE - Censo Demográfico 2010 
# upp 	Rocinha 
# Shape__Are 	844480.6833080013 
# Shape__Len 	9267.06808

library(sf)
library(raster)
library(dplyr)
library(sp)
library(dplyr)

rioslides <- sf::st_read("D:/PROslide_RIO/DATA/landslides_2023.shp")
rioslides <- rioslides %>%
  dplyr::filter(tipologia1 == 1 | tipologia2 == 1 | tipologia3 == 1 | tipologia4 == 1)
study_area          <- st_read("StudyArea.shp")
Limite_Favelas_2019 <- st_read("Limite_Favelas_2019.shp")
Limite_Favelas_2019 <- st_simplify(Limite_Favelas_2019, dTolerance = 1)
#Limite_Favelas_2019 <- sample_n(Limite_Favelas_2019,2)


#1. Load Libraries and Rasters
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
#final_rioslides     <- sf::st_transform(final_rioslides, st_crs(study_area))
#Limite_Favelas_2019 <- sf::st_transform(Limite_Favelas_2019, st_crs(study_area))


################################################################################
# Step 1: Calculate Ratio of Favelas Area to Study Area
area_favelas <- st_area(Limite_Favelas_2019) %>% sum()
area_study <- st_area(study_area) %>% sum()
favelas_ratio <- area_favelas / area_study

# Step 2: Count Landslides in Favelas
landslides_in_favelas <- st_intersection(rioslides, Limite_Favelas_2019)
num_landslides_favelas <- nrow(landslides_in_favelas)

# Step 3: Analyze Landslides Predicted by Susceptibility Maps
# Extract values from pred and pred2 at landslide locations

# Extract the coordinates
coordinates <- sf::st_coordinates(landslides_in_favelas)
x_coords <- coordinates[, "X"]
y_coords <- coordinates[, "Y"]

# Add the coordinates to the original sf object
landslides_in_favelas$X <- x_coords
landslides_in_favelas$Y <- y_coords

# Add an ID
landslides_in_favelas$ID <- seq(nrow(landslides_in_favelas))

# Remove rows with NA in coordinates
landslides_in_favelas <- landslides_in_favelas[!rowSums(is.na(landslides_in_favelas[, c("X", "Y")])),]

# Convert X and Y to numeric if they are not already
landslides_in_favelas$X <- as.numeric(landslides_in_favelas$X)
landslides_in_favelas$Y <- as.numeric(landslides_in_favelas$Y)

# Create a matrix of coordinates
coords_matrix <- cbind(landslides_in_favelas$X, landslides_in_favelas$Y)

# Create SpatialPoints
sp_points <- SpatialPoints(coords = coords_matrix)

# Convert to SpatialPointsDataFrame
landslides_in_favelas_spdf <- SpatialPointsDataFrame(sp_points, data = as.data.frame(landslides_in_favelas))

# Extracting from predictions
#beginCluster()
landslides_in_favelas_spdf_extract  <- raster::extract(pred, landslides_in_favelas_spdf, df = TRUE)
landslides_in_favelas_spdf_extract2 <- raster::extract(pred2, landslides_in_favelas_spdf, df = TRUE)
#endCluster()


landslides_in_favelas   <- merge(landslides_in_favelas_spdf_extract,   landslides_in_favelas,   by.x = 'ID', by.y = 'ID')
landslides_in_favelas   <- merge(landslides_in_favelas_spdf_extract2,   landslides_in_favelas,   by.x = 'ID', by.y = 'ID')

Summary_pred_megam_class <- landslides_in_favelas %>%
  dplyr::group_by(pred_megam_class) %>%
  dplyr::summarise(n_slide = n()) %>%
  filter(!is.na(pred_megam_class))


Summary_count <- landslides_in_favelas %>%
  dplyr::group_by(count_) %>%
  dplyr::summarise(n_slide = n()) %>%
  filter(!is.na(count_))

Summary_pred_megam_class#
Summary_count

# Further analysis can be done based on extracted values
# Obter os números de cada categoria do modelo Data-driven
# Extracting numbers for each category from the Data-driven model (pred_megam_class)
n_low_pred_megam_class <- Summary_pred_megam_class$n_slide[Summary_pred_megam_class$pred_megam_class == 1]
n_medium_pred_megam_class <- Summary_pred_megam_class$n_slide[Summary_pred_megam_class$pred_megam_class == 2]
n_high_pred_megam_class <- Summary_pred_megam_class$n_slide[Summary_pred_megam_class$pred_megam_class == 3]

# Extracting numbers for each category from the municipal model (count_)
n_low_count <- Summary_count$n_slide[Summary_count$count_ == 1]
n_medium_count <- Summary_count$n_slide[Summary_count$count_ == 2]
n_high_count <- Summary_count$n_slide[Summary_count$count_ == 3]

# Step 4: Write Summary
summary_text <- paste("In the study area, approximately", 
                      sprintf("%.2f%%", favelas_ratio * 100), 
                      "is covered by Favelas. There are", num_landslides_favelas, 
                      "landslides within the Favelas. The Data-driven model (pred_megam_class) identified",
                      n_low_pred_megam_class, "low,", 
                      n_medium_pred_megam_class, "medium, and", 
                      n_high_pred_megam_class, "high susceptibility landslides. The municipal model (count_) identified",
                      n_low_count, "low,", 
                      n_medium_count, "medium, and", 
                      n_high_count, "high susceptibility landslides. These findings indicate variation in landslide susceptibility predictions between the two models.")
# Print summary text
print(summary_text)






library(ggplot2)
library(dplyr)

# Combine the datasets and create a new column to distinguish them
Summary_pred_megam_class <- Summary_pred_megam_class %>%
  mutate(Model = "Data-driven")%>%
  rename(Susceptibility = pred_megam_class, Count = n_slide)


Summary_count <- Summary_count %>%
  mutate(Model = "Oficial LSM")%>%
  rename(Susceptibility = count_, Count = n_slide)

# # Adding a model identifier column
# Summary_pred_megam_class$Model <- "Data-driven"
# Summary_count$Model <- "Oficial LSM"

# # Now combine the data
# combined_data <- rbind(Summary_pred_megam_class, Summary_count)
# # Plot using ggplot
# combined_data$Model <- factor(combined_data$Model, levels = c("Oficial LSM", "Data-driven"))
# # Ensure 'Susceptibility' is a factor with the levels labeled as Low, Medium, and High
# combined_data$Susceptibility <- factor(combined_data$Susceptibility, levels = c("1", "2", "3"), labels = c("Low", "Medium", "High"))
# 
# # Define custom colors if desired
# custom_colors <- c("Low" = "gray70", "Medium" = "gray50", "High" = "gray0")
# 
# # Updated ggplot code
# p <- ggplot(combined_data, aes(x = Susceptibility, y = Count, fill = Susceptibility)) +
#   geom_bar(stat = "identity", color="black") +
#   geom_label(aes(label = Count), position = position_stack(vjust = 1), col= "white") +
#   scale_fill_manual(values = custom_colors) +
#   facet_wrap(~Model, scales = "free_x") +
#   labs(title = "Landslides occuring in the Favelas",
#        x = "Susceptibility Category",
#        y = "Number of Landslides",
#        fill = "Categories") +
#   guides(fill = guide_legend(title = "Category")) + 
#   guides(fill = guide_legend(override.aes = aes(label = "")))
# 
# # Save the ggplot to a file
# ggsave("D:/PROslide_RIO/Figs/Slides_favelas_ggplot.png", plot = p, width = 10, height = 6, dpi = 300)


Summary_count$Susceptibility            <-   factor(Summary_count$Susceptibility, levels = c("1", "2", "3"), labels = c("Low", "Medium", "High"))
Summary_pred_megam_class$Susceptibility <-   factor(Summary_pred_megam_class$Susceptibility, levels = c("1", "2", "3"), labels = c("Low", "Medium", "High"))

gg_Municip=ggplot(Summary_count, aes(x = Susceptibility, y = Count, fill = Susceptibility)) +
  geom_bar(stat = "identity", color="black", alpha=0.5) +
  geom_label(aes(label = Count), position = position_stack(vjust = 1.1), col= "black") +
  scale_fill_manual(values = c("#008000", "yellow", "red")) +
  facet_wrap(~Model, scales = "free_x") +
  labs(title = "Landslides occuring in the Favelas",
       x = "Susceptibility Category",
       y = "Number of Landslides",
       fill = "Categories") +
  guides(fill = guide_legend(title = "Category")) + 
  guides(fill = guide_legend(override.aes = aes(label = "")))+
  ylim(0,600)

############################################################################

gg_DD=ggplot(Summary_pred_megam_class, aes(x = Susceptibility, y = Count, fill = Susceptibility)) +
  geom_bar(stat = "identity", color="black", alpha=0.5) +
  geom_label(aes(label = Count), position = position_stack(vjust = 1), col= "black") +
  scale_fill_manual(values = c("gray", "#987645", "#65828D")) +
  facet_wrap(~Model, scales = "free_x") +
  labs(title = "Landslides occuring in the Favelas",
       x = "Susceptibility Category",
       y = "Number of Landslides",
       fill = "Categories") +
  guides(fill = guide_legend(title = "Category")) + 
  guides(fill = guide_legend(override.aes = aes(label = "")))+
  ylim(0,600)
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


# Mask the rasters with Limite_Favelas_2019
masked_rasters <- mask(stacked_rasters, Limite_Favelas_2019)

#mapview::mapview(masked_rasters)+


# Extracting cell numbers from predictions
pred_fav <- raster::extract(stacked_rasters, Limite_Favelas_2019, df = TRUE)
pred_fav <- na.omit(pred_fav)



# Count the number of pixels with different susceptibility levels within each Favela polygon
pixel_counts_pred <- pred_fav %>%
  group_by(count_) %>%
  summarize(Count = n()) %>%
  mutate(Total = sum(Count, na.rm = TRUE),  # Sum excluding NAs
         Percentage = (Count / Total) * 100)  %>%
  rename(Susceptibility = count_)%>%
  mutate(Model = "Oficial LSM")

pixel_counts_pred2 <- pred_fav %>%
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

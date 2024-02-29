library(sf)
library(dplyr)
#library(rgeos)
library(raster)
library(exactextractr)
library(mapview)

dem= raster::raster("D:/PROslide_RIO/DATA/dtm2.tif")
dem
# Print the original raster details
print(dem)
# Calculate the aggregation factor
# Since your original resolution is 5m and you want to change it to 20m,
# you divide the desired resolution by the original resolution.
factor <- 20 / res(dem)[1]
# Create a new raster with the desired extent and resolution
# The resolution is set to 20m x 20m
targetRaster <- raster(extent(dem), res=20, crs=projection(dem))
# Resample using bilinear interpolation
dem_resampled <- resample(dem, targetRaster, method='bilinear')
dem_resampled
# Save the resampled raster
#writeRaster(dem_resampled, "E:/SlopeUnits_RJ/dtm20.tif", format="GTiff", overwrite=TRUE)
# Calculate TPI
# First, calculate the mean elevation around each cell (e.g., using a 3x3 window)
mean_dem <- focal(dem_resampled, w = matrix(1, 15, 15), fun = mean, na.rm = TRUE)
# Then, calculate TPI as the difference between the DEM and its smoothed version
tpi <- dem_resampled - mean_dem
# Classify TPI based on Weiss (2001) or similar classification scheme
# Note: Thresholds are examples; adjust based on your analysis needs and landscape
# Corrected reclassification matrix
reclass_matrix <- matrix(c(-Inf, -1, 1,
                           -1, -0.5, 2,
                           -0.5, 0.5, 3,
                           0.5, 1, 4,
                           1, Inf, 5), byrow = TRUE, ncol = 3)



tpi_classes <- reclassify(tpi, reclass_matrix, right=TRUE)
# Assign names to the classes correctly

levels(tpi_classes) <- list(data.frame(ID = 1:5,
                                       Class = c("Valleys", "Lower_Slopes", "Flat_Areas", "Upper_Slopes", "Ridges")))

tpi_classes <- reclassify(tpi, reclass_matrix, right=TRUE)

# Identify flat areas (e.g., where TPI class equals 3 for flat areas)
flat_areas <- tpi_classes == 3
# Optionally, set values to NA where not flat (if not automatically done)
flat_areas[is.na(flat_areas)] <- 0

majority_fun <- function(x) {
  uniqx <- unique(x)
  uniqx <- uniqx[!is.na(uniqx)]
  if (length(uniqx) == 0) return(NA)
  tab <- tabulate(match(x, uniqx))
  uniqx[which.max(tab)]
}
# Apply the majority filter using a 3x3 window
#flat_areas_majority <- focal(flat_areas, w=matrix(1,9,9), fun=majority_fun, pad=TRUE, padValue=NA)


# Calculate slope from the resampled DEM
slope_dem_resampled <- terrain(dem_resampled, opt = 'slope', unit = 'degrees')

# Identify areas with slope less than 5 degrees
slope_less_than_5 <- slope_dem_resampled < 5
#slope_less_than_2 <- slope_dem_resampled < 2
#slope_less_than_0 <- slope_dem_resampled < 0
# Combine the TPI-based flat areas and slope < 5 degrees areas
# This creates a raster where cells are TRUE if they are either flat according to TPI or have a slope less than 5 degrees
combined_flat_areas5 <- flat_areas | slope_less_than_5
#combined_flat_areas2 <- flat_areas | slope_less_than_2
#combined_flat_areas0 <- flat_areas | slope_less_than_0

# Now, you might want to apply a majority filter on the combined raster to smooth the edges and reduce noise
combined_flat_areas_majority5 <- focal(combined_flat_areas5, w=matrix(1, 3, 3), fun=majority_fun, pad=TRUE, padValue=NA)
#combined_flat_areas_majority2 <- focal(combined_flat_areas2, w=matrix(1, 3, 3), fun=majority_fun, pad=TRUE, padValue=NA)
#combined_flat_areas_majority0 <- focal(combined_flat_areas0, w=matrix(1, 3, 3), fun=majority_fun, pad=TRUE, padValue=NA)
# Update: For larger smoothing, you've already used a 101x101 window
# If you want to maintain that, ensure it aligns with your analysis goals


#mapview::mapview(flat_areas_majority)
# Save only the flat areas raster
#writeRaster(flat_areas_majority, "E:/SlopeUnits_RJ/flat_areas_101.tif", format="GTiff", overwrite=TRUE)

writeRaster(combined_flat_areas_majority5, "E:/SlopeUnits_RJ/combined_flat_areas_majority5b.tif", format="GTiff", overwrite=TRUE)
#writeRaster(combined_flat_areas_majority2, "E:/SlopeUnits_RJ/combined_flat_areas_majority2.tif", format="GTiff", overwrite=TRUE)
#writeRaster(combined_flat_areas_majority0, "E:/SlopeUnits_RJ/combined_flat_areas_majority0.tif", format="GTiff", overwrite=TRUE)

###############################################################################
################################################################################
################################################################################
################################################################################




# This ons was generalized by the "Polygon generalization" tool from SAGA 
slope_units <- st_read("E:/SlopeUnits_RJ/slopeunits3/slusmooth_gen.shp")

# Check for validity and make valid if necessary
slope_units <- st_buffer(slope_units, 0)








#slope_units$flatness <- exactextractr::exact_extract(flat_raster, slope_units, 'majority')
slope_units$flat_frac <- exactextractr::exact_extract(combined_flat_areas_majority5, slope_units, 'frac')

slope_units$nflat_perc = slope_units$flat_frac$frac_0
slope_units$flat_perc = slope_units$flat_frac$frac_1

# Now visualize the slope_units with mapview
mapview(slope_units, zcol = c("nflat_perc", "flat_perc"), legend = TRUE) + mapview(combined_flat_areas_majority5)
mapview(slope_units, zcol = "flat_perc", legend = TRUE) #+ mapview(combined_flat_areas_majority5)

slope_units_subset <- slope_units %>%
  filter(flat_perc < .75)

# Visualize the subset
#mapview(slope_units_subset, zcol = "flat_perc", legend = TRUE)

slope_units_subset$area= st_area(slope_units_subset$geometry)

excluded_cats <- c(3244, 3223, 3248, 3251, 3087, 3187, 3219, 3178, 3285, 3281, 3170, 3212, 3209, 3180, 62, 77, 78,3278)
slope_units_subset <- slope_units_subset %>%
  filter(!cat %in% excluded_cats) %>%
  filter(as.numeric(area) > as.numeric(100000))  # Assuming AREA is the column for area in the same units as the threshold

#st_write(slope_units_subset, "E:/SlopeUnits_RJ/slopeunits3/slusmooth_gen_subset.shp")

# Read the shapefile using sf package
rioslides <- sf::st_read("D:/PROslide_RIO/DATA/landslides_2023.shp")

rioslides <- rioslides %>%
  dplyr::filter(tipologia1 == 1 | tipologia2 == 1 | tipologia3 == 1 | tipologia4 == 1)

# Step 1: Aggregate Landslides by SU
landslides_per_SU <- rioslides  %>%
  st_intersection(slope_units_subset) %>%
  group_by(cat) %>%
  summarise(nslide= n(), .groups = 'drop') 

# Step 2: Merge Count with SU Data
slope_units_subset <- slope_units_subset %>%
  st_join(landslides_per_SU, by = "cat")


sum(slope_units_subset$nslide, na.rm = TRUE)
slope_units_subset$nslide[is.na(slope_units_subset$nslide)] <- 0

st_write(slope_units_subset, "E:/SlopeUnits_RJ/slopeunits3/slusmooth_gen_subset.shp",append=FALSE )

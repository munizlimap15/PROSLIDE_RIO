# Load packages
library(sf)
library(terra)
library(ggplot2)
library(osmdata)
library(dplyr)
library(mapview)
library(leafsync)
library(raster)
library(ggnewscale)

# Read slope units shapefile
slope_units <- st_read("D:/PROslide_RIO/Rcodes/Shinny_app_RioSlide/slusmooth_gen_subset.shp")
slope_units$area_km2 <- st_area(slope_units) / 1e6
slope_units$slide_density <- as.numeric(slope_units$nslide / slope_units$area_km2)

rioslides <- st_read("D:/PROslide_RIO/DATA/landslides_2023.shp")
study_area <- st_read("D:/PROslide_RIO/Rcodes/Shinny_app_RioSlide/StudyArea.shp")

# Read hillshade
hillshade <- rast("D:/PROslide_RIO/SAGA/hillshade.tif")
hillshade_df <- as.data.frame(hillshade, xy = TRUE)

# Transform to WGS84 (required for OSM queries)
study_area_wgs <- st_transform(study_area, 4326)

# Get bounding box
bbox <- st_bbox(study_area_wgs)

# Build OSM query for specific road types
main_roads <- c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential")

q <- opq(bbox = bbox) %>%
  add_osm_feature(key = "highway", value = main_roads)

roads_osm <- osmdata_sf(q)$osm_lines

# Keep only safe fields for Shapefile
roads_export <- roads_osm[, c("osm_id", "name", "highway", "geometry")]
names(roads_export) <- c("osmid", "name", "hwy_type", "geometry")






# Export
output_dir <- "D:/PROslide_RIO/Rcodes/Shinny_app_RioSlide/"
st_write(
  roads_export,
  dsn = output_dir,
  layer = "roads_osm_may",
  driver = "ESRI Shapefile",
  delete_layer = TRUE
)

# Export as GeoJSON (.geojson)
#st_write(roads_osm , file.path(output_dir, "roads_osm.geojson"), delete_dsn = TRUE)
# Convert CRS to match slope units
roads_osm <- st_transform(roads_osm, st_crs(slope_units))

# Intersect to cut road segments by slope unit boundaries
roads_intersect <- st_intersection(roads_osm, slope_units)

# Compute road segment lengths in meters
roads_intersect$length_m <- st_length(roads_intersect)

# Summarize total road length per slope unit (in km)
road_length <- roads_intersect %>%
  group_by(cat_x) %>%
  summarise(road_km = sum(as.numeric(length_m)) / 1000) %>%
  st_drop_geometry()  # drop geometry to allow normal left_join

# Calculate area of each slope unit (in km²)
slope_units$area_km2 <- as.numeric(st_area(slope_units)) / 1e6

# Join road length info to slope units
slope_units_density <- slope_units %>%
  left_join(road_length, by = "cat_x")

# Calculate road density: road length / area (km per km²)
slope_units_density$density <- slope_units_density$road_km / slope_units_density$area_km2

# map_with_su <- 
#   mapview(roads_osm, color = "black", lwd = 2) + 
#   mapview(slope_units_density, color = "white", zcol = "density", alpha = 0.1) +
#   mapview(rioslides, color = "red", col.regions = "red", lwd = 1)
# 
# # Map without slope units
# map_without_su <- 
#   mapview(roads_osm, color = "black", lwd = 2) +
#   mapview(rioslides, color = "red", col.regions = "red", lwd = 1)
# 
# # Sync them side-by-side
# sync(map_with_su, map_without_su)








# Plot A: hillshade + roads
p1 <- ggplot() +
  geom_raster(data = hillshade_df, aes(x = x, y = y, fill = hillshade), show.legend = FALSE) +
  scale_fill_gradient(low = "white", high = "black") +
  
  geom_sf(data = roads_osm, aes(color = "Infrastructure (Road network)"), size = 0.01, alpha = 0.25) +
  scale_color_manual(name = NULL, values = c("Infrastructure (Road network)" = "brown")) +
  geom_sf(data = rioslides, color = "blue", fill = "blue", shape = 17, size = 1/2, alpha = 0.6) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "a) Roads + Landslides")


slope_units <- slope_units %>%
  mutate(
    slide_quartile = factor(ntile(slide_density, 4), labels = c("Q1", "Q2", "Q3", "Q4"))
  )
slope_units$slide_quartile <- factor(slope_units$slide_quartile, 
                                     levels = c("Q1", "Q2", "Q3", "Q4"))

# Plot B: hillshade + slope units colored by landslide density
p2 <- ggplot() +
  geom_raster(data = hillshade_df, aes(x = x, y = y, fill = hillshade), show.legend = FALSE) +
  scale_fill_gradient(low = "white", high = "black") +
  new_scale_fill() +  # Allows a second fill scale after raster
  #geom_sf(data = rioslides, color = "red", fill = "red", shape = 17, size = 1, alpha = 0.7) +
  geom_sf(data = slope_units, aes(fill = slide_quartile), color = NA) +
  scale_fill_viridis_d(option = "mako",name = "Landslide Density (Quartile)",direction=-1, alpha=0.5, na.translate = FALSE) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "b) Slope Units (Landslide Density) + Landslides")


#
# Calcular os quartis com ntile()
slope_units_density <- slope_units_density %>%
  mutate(
    slide_quartile = factor(ntile(density, 4), labels = c("Q1", "Q2", "Q3", "Q4")))

## Plot C: hillshade + slope units colored by road density
p3 <- ggplot() +
  geom_raster(data = hillshade_df, aes(x = x, y = y, fill = hillshade), show.legend = FALSE) +
  scale_fill_gradient(low = "white", high = "black") +
  new_scale_fill() +
  #geom_sf(data = roads_osm, color = "brown", size = 0.005, alpha = 0.1) +
  geom_sf(data = slope_units_density, aes(fill = slide_quartile), color = NA) +
  scale_fill_viridis_d(name = "Road Density (Quartile)", direction=-1, alpha=0.5, na.translate = FALSE ) +
  
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "c) Slope Units (Roads Density) + Landslides")

# Combine your plots into one object
library(patchwork)
combined_plot <- (p1 / p2 / p3)

# Save to file (adjust size and resolution as needed)
ggsave(
  filename = "D:/PROslide_RIO/Figs/multi_panel_figure.png",
  plot = combined_plot,
  width = 8, height = 12, dpi = 300
)





# Load necessary packages
# The raster package is used for reading and manipulating raster data (.tif files in this case)
library(raster)

# Define a function to resample and crop a raster to match a template raster
align_raster_num <- function(raster_to_align, template_raster) {
  raster_to_align <- resample(raster_to_align, template_raster, method = "bilinear")
  raster_to_align <- crop(raster_to_align, extent(template_raster))
  return(raster_to_align)
}


# Define a function to resample and crop a raster to match a template raster
align_raster_categ <- function(raster_to_align, template_raster) {
  raster_to_align <- resample(raster_to_align, template_raster, method = "ngb")
  raster_to_align <- crop(raster_to_align, extent(template_raster))
  return(raster_to_align)
}



# Read in the DTM
dtm <- raster("E:/DTM_2019_RJ_5m/dtm_5m")
crs(dtm) <- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"

# Create a raster mask with zero values
mask <- dtm
mask [] <- 0
crs(mask) <- crs(dtm)

writeRaster(mask, 'C:/Users/pedro/Documents/PROslide_RIO/DATA/mask.tif',overwrite=TRUE)
#writeRaster(mask, 'C:/Users/pedro/Documents/PROslide_RIO/DATA/mask.asc',overwrite=TRUE)
# Create or load a mask raster object (this is the mask)
mask <- raster("C:/Users/pedro/Documents/PROslide_RIO/DATA/mask.tif")

list.files("C:/Users/pedro/Documents/PROslide_RIO/DATA", pattern = "\\.tif$")

dtm <- crop(dtm, extent(mask))
# Perform the mask operation
dtm <- mask(dtm, mask)

#writeRaster(dtm, 'C:/Users/pedro/Documents/PROslide_RIO/DATA/dtm_5m.asc', overwrite=TRUE)
#dtm <- raster("C:/Users/pedro/Documents/PROslide_RIO/DATA/dtm_5m.asc")

# Load .tif files generated using SAGA GIS's "Basic Terrain Analysis" tool
# Each variable will contain the raster data for the corresponding terrain analysis output

# Aspect: Indicates the compass direction that the terrain slope faces at each raster cell
aspect <- raster("C:/Users/pedro/Documents/PROslide_RIO/DATA/aspect.tif")
aspect           <- align_raster_categ(aspect, dtm)
crs(aspect) <- crs(dtm)
# Define reclassification matrix
# The matrix has three columns: from, to, becomes
# The values in radians are just for demonstration; you should adjust them according to your needs
reclass_matrix <- matrix(c(
  0, 0.785398163, 1,  # N
  0.785398163, 1.57079633, 2,  # NE
  1.57079633, 2.35619449, 3,  # E
  2.35619449, 3.14159265, 4,  # SE
  3.14159265, 3.92699082, 5,  # S
  3.92699082, 4.71238898, 6,  # SW
  4.71238898, 5.49778714, 7,  # W
  5.49778714, 7, 8  # NW
), ncol = 3, byrow = TRUE)

# Reclassify the raster
aspect <- reclassify(aspect, reclass_matrix)

# Plan Curvature: Measures the curvature of the terrain in the horizontal plane
plan_curv <- raster("C:/Users/pedro/Documents/PROslide_RIO/DATA/plan_curv.tif")

# Profile Curvature: Measures the curvature of the terrain in the vertical plane
prof_curv <- raster("C:/Users/pedro/Documents/PROslide_RIO/DATA/prof_curv.tif")

# Relative Slope Position: Relative position of each raster cell within the local terrain
rel_slp_position <- raster("C:/Users/pedro/Documents/PROslide_RIO/DATA/rel_slp_pos.tif")

# Slope: Measures the maximum rate of change in elevation at each raster cell
slope <- raster("C:/Users/pedro/Documents/PROslide_RIO/DATA/slope.tif")

slope <- slope * (180 / pi)
# Topographic Wetness Index (TWI): Indicates how wet each area is likely to be based on topography
twi <- raster("C:/Users/pedro/Documents/PROslide_RIO/DATA/twi.tif")

# This layer was generated using the TPI (Topographic Position Index) suggested settings

tpi <- raster("C:/Users/pedro/Documents/PROslide_RIO/DATA/twi.tif")
tpi              <- align_raster_categ(tpi, dtm)
crs(tpi) <- crs(dtm)
##summary(tpi)
#tpi
#Min.    -6.003804e+01
#1st Qu. -1.102186e+00
#Median  -7.266018e-02
#3rd Qu.  6.107567e-01
#Max.     9.566508e+01
#NA's     5.727492e+07
# Use the reclassify function to categorize the TPI values into different classes.
# The vector c(-Inf, -5, 1, -5, -2, 2, -2, 1, 3, 1, 2, 4, 2, 5, 5, 5, Inf, 6) specifies the range and the new value for each class.
# For example, TPI values between -Inf and -5 will be classified as 1 (Valleys).
tpi <- reclassify(tpi, c(-Inf, -5, 1,  # Valleys
                                    -5, -2, 2,    # Lower slope
                                    -2, 1, 3,     # Mid slope
                                    1, 2, 4,      # Flat
                                    2, 5, 5,      # Upper slope
                                    5, Inf, 6))   # Ridge / hilltop


# Note: All these raster layers are intended for terrain analysis and were generated using the "Basic Terrain Analysis" tool from SAGA GIS.

landcover19 <- raster("C:/Users/pedro/Documents/PROslide_RIO/DATA/landcover19")
landcover19      <- align_raster_categ(landcover19, dtm)
crs(landcover19) <- crs(dtm)
unique(landcover19)
# 1  = "Afloramentos rochosos e depósitos sedimentares"  
# 2  = "Cobertura arbórea e arbustiva"
# 3  = "Cobertura gramíneo lenhosa"
# 4  = "Corpos hídricos" 
# 5  = "Favela" 
# 6  = "Áreas agrícolas" 
# 7  = "Áreas de comércio e serviços" 
# 8  = "Áreas de educação e saúde"
# 9  = "Áreas de exploração mineral"   
# 10 = "Áreas de lazer"
# 11 = "Áreas de transporte" 
# 12 = "Áreas industriais"  
# 13 = "Áreas institucionais e de infraestrutura pública" 
# 14 = "Áreas não edificadas"      
# 15 = "Áreas residenciais" 
# 16 = "Áreas sujeitas à inundação"                                


reclass_fun <- function(x) {
  ifelse(x == 1, 1,                                       # Rocky Outcrops and Sedimentary Deposits
  ifelse(x == 2, 2,                                       # Forest and Shrub Cover
  ifelse(x == 3, 3,                                       # Grass and Woody Cover
  ifelse(x == 4 | x == 16, 4,                             # Water Bodies
  ifelse(x == 5, 5,                                       # Favela
  ifelse(x == 15, 6,                                      # Residential Areas
  ifelse(x == 6 | x == 14, 7,                             # Agricultural and Undeveloped Areas
  ifelse(x %in% c(7, 8, 9, 10, 11, 12, 13), 8, NA)))))))) # Industrial, Commercial, and Public Infrastructure
}

# Apply custom reclassification
landcover19 <- calc(landcover19, fun = reclass_fun)

# Check unique values
unique(landcover19)



# Crop landcover19 to the extent of mask
landcover19 <- crop(landcover19, extent(mask))
# Perform the mask operation
landcover19 <- mask(landcover19, mask)



geomorph     <- raster("C:/Users/pedro/Documents/PROslide_RIO/DATA/geomorph.tif")
geomorph          <- align_raster_categ(geomorph, dtm)
crs(geomorph) <- crs(dtm)
#1 = Vertentes Côncavas - Divergentes
#2 = Vertentes Côncavas - Convergentes
#3 = Vertentes Convexas - Divergentes
#4 = Vertentes Convexas - Convergentes
#5 = Área Plana
#6 = Escarpas artificiais
#7 = Topos de elevação
#8 = Escarpas naturais
#9 = Talvegues

# Resample to the same dimensions
geomorph <- resample(geomorph, dtm, method='ngb')
# Crop to the same extent
geomorph <- crop(geomorph, extent(mask))



geol      <- raster("C:/Users/pedro/Documents/PROslide_RIO/DATA/geologico.tif")  # Replace 2 with the actual band number for "value"
geol <- geol + 1
geol[geol > 17] <- NA

geol <- as.integer(geol)
geol             <- align_raster_categ(geol, dtm)
crs(geol) <- crs(dtm)
#1 = Granito Pegmatóide
#2 = Granodiorito, Tonalito e Quartzodiorito
#3 = Gnaisse Migmatitico
#4 = Aluvião
#5 = Granito Cinza Porfirítico (Granito Favela)
#6 = Gnaisse Granitóide
#7 = Aterro
#8 = Sienito e Tinguaito
#9 = Gnaisse Bandado (Archer)
#10 = Biotita Gnaisse
#11 = Kinzigito
#12 = Leptinito
#13 = Gnaisse Facoidal
#14 = Gnaisse Bandado
#15 = Quartzo Gabro e Quartzo Diorito
#16 = Dunas
#17 = 

# # Resample using nearest neighbor method
# geol <- resample(geol, dtm, method='ngb')  # 'ngb' stands for nearest neighbor
# 
# # Crop to the same extent
# geol <- crop(geol, extent(mask))

summary(as.factor(values(geol)))
#2        3        4        5        6        7        8        9       10       11       12       13       14       15 
#53676820  1047474  1032175  4044433 21123555  8090356  1180349   711119   354533  3190988  3361037   409357   367338  1504977 
#16       17       18       19      257     NA's 
#   26292    93892   790575     1220  4849195  6008094 


# Create a list of the raster layers
raster_list <- list(dtm, aspect, plan_curv, prof_curv, rel_slp_position, slope, twi, 
                    tpi, landcover19,geomorph, geol)



# dtm              <- raster("dtm.asc")
# aspect           <- raster("aspect.asc")
# plan_curv        <- raster("plan_curv.asc")
# prof_curv        <- raster("prof_curv.asc")
# rel_slp_position <- raster("rel_slp_position.asc")
# slope            <- raster("slope.asc")
# twi              <- raster("twi.asc")
# tpi              <- raster("tpi.asc")
# landcover        <- raster("landcover19.asc")
# geomorph         <- raster("geomorph.asc")
# geol             <- raster("geol.asc")



# Align all rasters to the extent and resolution of dtm_5m
plan_curv        <- align_raster_num(plan_curv, dtm)
crs(plan_curv) <- crs(dtm)

prof_curv        <- align_raster_num(prof_curv, dtm)
crs(prof_curv) <- crs(dtm)

rel_slp_position <- align_raster_num(rel_slp_position, dtm)
crs(rel_slp_position) <- crs(dtm)

slope            <- align_raster_num(slope, dtm)
crs(slope) <- crs(dtm)

twi              <- align_raster_num(twi, dtm)
crs(twi) <- crs(dtm)



setwd("C:/Users/pedro/Documents/PROslide_RIO/DATA")


########################################
writeRaster(dtm,              filename="dtm.asc",         format="ascii", overwrite=TRUE)
writeRaster(aspect,           filename="aspect.asc",      format="ascii", overwrite=TRUE)
writeRaster(plan_curv,        filename="plan_curv.asc",   format="ascii", overwrite=TRUE)
writeRaster(prof_curv,        filename="prof_curv.asc",   format="ascii", overwrite=TRUE)
writeRaster(rel_slp_position, filename="rel_slp_position",format="ascii", overwrite=TRUE)
writeRaster(slope,            filename="slope.asc",       format="ascii", overwrite=TRUE)
writeRaster(twi,              filename="twi.asc",         format="ascii", overwrite=TRUE)
writeRaster(tpi,              filename="tpi.asc",         format="ascii", overwrite=TRUE)
writeRaster(landcover19,      filename="landcover19.asc", format="ascii", overwrite=TRUE)
writeRaster(geomorph,         filename="geomorph.asc",    format="ascii", overwrite=TRUE)
writeRaster(geol,             filename="geol.asc",        format="ascii", overwrite=TRUE)



# # Check if all raster files have the same CRS, ncol, and nrow
# same_crs  <- all(sapply(raster_list, function(x) identicalCRS(x, aspect)))
# same_ncol <- all(sapply(raster_list, function(x) ncol(x) == ncol(aspect)))
# same_nrow <- all(sapply(raster_list, function(x) nrow(x) == nrow(aspect)))
# 
# if (same_crs && same_ncol && same_nrow) {
#   message("All rasters have the same CRS, number of columns, and number of rows.")
# 
#   # Create a RasterStack
#   raster_stack <- stack(raster_list)
#   message("Created a RasterStack.")
# 
# } else {
#   message("Rasters do not have the same CRS, number of columns, or number of rows.")
#   if (!same_crs) {
#     message("Different CRS detected.")
#   }
#   if (!same_ncol) {
#     message("Different number of columns detected.")
#   }
#   if (!same_nrow) {
#     message("Different number of rows detected.")
#   }
# }

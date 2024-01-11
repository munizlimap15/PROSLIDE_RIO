

library(dplyr)
library(tidyr)
library(raster)
library(RSAGA)

rm(list = ls()) 
setwd("C:/Users/pedro/Documents/PROslide_RIO/DATA")

RJ <- sf::st_read("C:/Users/pedro/Documents/PROslide_RIO/DATA/StudyArea.shp")

rioslides = sf::st_read("C:/Users/pedro/Documents/PROslide_RIO/DATA/landslides_2023.shp")
# Reproject to WGS 84 / UTM zone 23S
rioslides <- sf::st_transform(rioslides, crs = "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs")

summary(as.factor(rioslides$tipologia1))
#0    1    2    3    4    5    6    7    8    9   10   11 
#59 1659   32   25    1  323   55  449   12   71  155  157 

rioslides <- rioslides %>%
  dplyr::filter(tipologia1 == 1 | tipologia2 == 1 | tipologia3 == 1 | tipologia4 == 1)

# Rename the columns in the data frame part of the Simple Features object
names(rioslides)[names(rioslides) == "coorde"] <- "x"
names(rioslides)[names(rioslides) == "coordn"] <- "y"


#Tabelle einlesen / Name der Tabelle ("d")
d = as.data.frame(rioslides)
d$slide <- 1


plot(d$x, d$y, pch=16, cex=0.1)

# set.seed to ensure reproducibility of the "random results"
set.seed(1)

# Define "extent" to sample the non-Landslides as the same extent of the study area (in this case, the ascii-grid "mask" is read out)
mask = raster("C:/Users/pedro/Documents/PROslide_RIO/DATA/mask.asc")
# Write the raster object to an ASCII grid file
#writeRaster(mask, filename="C:/Users/pedro/Documents/PROslide_RIO/DATA/mask.asc", format="ascii",overwrite=TRUE)
extent1 = read.ascii.grid.header("mask")
extent1

#----------------------------------
#--------Sampling on the mask WITH FLAT AREAS
# Distribute N (a number) points randomized within the mask extender defined above (Important: only one bounding rectangle is defined)
# Since the sampling will be done on the bounding rectangle and therefore also outside of the study area, the number of points (N) should be greather than you landslide sample (2x-3x)
N = 5000

#Create a data table (named "noslide") with the attribute "slide" and the coordinates,...
#...where each point is assigned the value "0" (0 means -> is not a landslide point location)
noslide = data.frame(slide = 0, x = sample( extent1$xllcenter + c(0:(extent1$ncols-1)) * extent1$cellsize, 
                                            size = N, replace = TRUE), y = sample( extent1$yllcenter + c(0:extent1$nrows-1) * extent1$cellsize,  size = N, replace = TRUE))

# Pick the values from the mask (Which should have been set as zero on ARCGIS previously), read and add as a variable to the nonlandslide
#This will make "0" to all the N samples located on the mask. The ones located outside will be NA
noslide = pick.from.ascii.grid(noslide, file="mask.asc")

# Keep all points that do not contain a NA
noslide = noslide [ !is.na(noslide$mask) , ]
# Convert the data frame to an sf object
noslide <- sf::st_as_sf(noslide, coords = c("x", "y"), crs = 31983)  # SIRGAS 2000 / UTM zone 23S
noslide <- sf::st_intersection(noslide, RJ)

# Convert to data frame
noslide_df <- as.data.frame(noslide)

coords <- sf::st_coordinates(noslide)
noslide_df$x <- coords[, 1]
noslide_df$y <- coords[, 2]


#noslide = noslide[sample(nrow(noslide), nrslides), ]
#noslide$mask2 = NULL


# First plot
plot(d$x, d$y, pch=16, cex=0.1)
# Overlay the second plot on top of the first
points(noslide_df$x, noslide_df$y, pch=16, cex=0.1, col="red")

noslide= noslide_df
#Merge the table create with landslides (here named "d") and noslide (here named "noslide") into a new table here called "d"
common_cols <- intersect(names(d), names(noslide))

train <- rbind(d[, common_cols], noslide[, common_cols])

summary(as.factor(train$slide))

#define the presence of landslide as a factor "TRUE"
train$slide1[train$slide == 0]=FALSE
train$slide1[train$slide == 1]=TRUE
train$slide = NULL
train$slide = train$slide1
train$slide1=NULL
train$slide = as.factor(train$slide)
summary(train$slide)


plot(train$x, train$y, pch=16, cex=0.1, main="red: TRUE; black: FALSE", col=train$slide)

#setwd("C:/Users/pedro/Documents/PROslide_RIO/DATA")
#Save the data under "train.Rd"
save(train, file = "train.Rd", compress = TRUE)

#Diese Daten koennen beim naechsten mal direkt ueber folgenden Befehl geladen werden (entfernen von #):
(load("train.Rd"))

summary(train)

setwd("E:/PROslide_RIO/DATA2")
dtm              <- raster("dtm.asc")
aspect           <- raster("aspect.asc")
plan_curv        <- raster("plan_curv.asc")
prof_curv        <- raster("prof_curv.asc")
rel_slp_position <- raster("rel_slp_position.asc")
slope            <- raster("slope.asc")
twi              <- raster("twi.asc")
tpi              <- raster("tpi_class.asc")
landcover19      <- raster("landcover19.asc")
geomorph         <- raster("geomorph.asc")
geol             <- raster("geol.asc")

# # Define a function to resample and crop a raster to match a template raster
# align_raster <- function(raster_to_align, template_raster) {
#   raster_to_align <- resample(raster_to_align, template_raster, method = "bilinear")
#   raster_to_align <- crop(raster_to_align, extent(template_raster))
#   return(raster_to_align)
# }
# 
# # Align all rasters to the extent and resolution of dtm_5m
# aspect           <- align_raster(aspect, dtm)
# plan_curv        <- align_raster(plan_curv, dtm)
# prof_curv        <- align_raster(prof_curv, dtm)
# rel_slp_position <- align_raster(rel_slp_position, dtm)
# slope            <- align_raster(slope, dtm)
# twi              <- align_raster(twi, dtm)
# tpi              <- align_raster(tpi, dtm)
# landcover19        <- align_raster(landcover19, dtm)
# geomorph         <- align_raster(geomorph, dtm)
# geol             <- align_raster(geol, dtm)

# Now stack them
layers <- stack(list(
  dtm, aspect, plan_curv, prof_curv, rel_slp_position, slope, twi, tpi, landcover19, geomorph, geol
))


# Add a unique ID to the original 'train' data frame
train$ID <- seq_len(nrow(train))

# Subset the data frame to include only the x, y, and ID columns
train_xy <- train[, c("x", "y", "ID")]

# Run the extract function
train2 <- raster::extract(layers, train_xy[, c("x", "y")], sp = T)

# Convert to data frame and add the ID back
train2 <- as.data.frame(train2)
train2$ID <- train_xy$ID

# Merge the slide field back using the ID
final_train <- merge(train2, train[, c("x", "y", "ID", "slide")], by = "ID")

# Check the summary
summary(final_train)
summary(as.factor(final_train$geol))

# Remove rows with NA values
final_train <- final_train %>% drop_na()
final_train <- final_train[!(final_train$geomorph == 255 | final_train$geol == 255),]

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

# Check the summary again
summary(final_train)
final_train$ID = NULL

save(final_train, file = "final_train.Rd", compress = TRUE)

(load("final_train.Rd"))
summary(final_train)


plot(final_train$slide, final_train$slope)





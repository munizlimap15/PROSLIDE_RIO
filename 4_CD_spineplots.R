# Load necessary libraries
library(ggplot2)
library(gridExtra)
library(grid)

# Load the final_train data
load("D:/PROslide_RIO/DATA/final_train.Rd")

# Ensure the slide variable is a factor
final_train$slide <- as.factor(final_train$slide)
#final_train <- subset(final_train, slope < 75)


summary(final_train)
final_train_true <- subset(final_train, slide == "TRUE")

# Generate summary for the subsetted data
summary(final_train_true)

# Lithology classes with abbreviations
lithology_classes <- c(
  "A" = "Granito Pegmatóide", 
  "B" = "Granodiorito, Tonalito e Quartzodiorito", 
  "C" = "Gnaisse Migmatitico", 
  "D" = "Aluvião", 
  "E" = "Granito Cinza Porfirítico (Granito Favela)", 
  "F" = "Gnaisse Granitóidea", 
  "G" = "Aterro", 
  "H" = "Sienito e Tinguaito", 
  "I" = "Gnaisse Bandado (Archer)", 
  "J" = "Biotita Gnaisse", 
  "K" = "Kinzigito", 
  "L" = "Leptinito", 
  "M" = "Gnaisse Facoidal", 
  "N" = "Gnaisse Bandado", 
  "O" = "Quartzo Gabro e Quartzo Diorito", 
  "P" = "Dunas", 
  "Q" = ""
)

geomorph_classes <- c(
  "1" = "Concave Slopes - Divergent", 
  "2" = "Concave Slopes - Convergent", 
  "3" = "Convex Slopes - Divergent", 
  "4" = "Convex Slopes - Convergent",
  "5" = "Flat Area",
  "6" = "Artificial Scarps",
  "7" = "Elevation Tops",
  "8" = "Natural Scarps",
  "9" = "Thalwegs"
)

# Land cover classes with abbreviations
landcover_classes <- c(
  "A" = "Rocky Outcrops and Sedimentary Deposits", 
  "B" = "Forest and Shrub Cover", 
  "C" = "Grass and Woody Cover", 
  "D" = "Water Bodies", 
  "E" = "Favela", 
  "F" = "Residential Areas", 
  "G" = "Agricultural and Undeveloped Areas", 
  "H" = "Industrial, Commercial, and Public Infrastructure"
)

# Aspect classes with directions
aspect_classes <- c(
  "1" = "N", 
  "2" = "NE", 
  "3" = "E", 
  "4" = "SE", 
  "5" = "S", 
  "6" = "SW", 
  "7" = "W", 
  "8" = "NW"
)
summary(as.factor(final_train$tpi))

# TPI classes with updated reclassification
final_train$tpi <- recode_factor(final_train$tpi,
                                 "3" = "Mid slope",
                                 "4" = "Flat",
                                 "5" = "Upper slope",
                                 "6" = "Ridge / hilltop")

# Filter out unwanted values
final_train <- final_train[!(final_train$geol == "Q" | final_train$geomorph %in% c(0, 255)), ]

# Reclassify lithology and landcover with abbreviations
final_train$geol <- factor(final_train$geol, labels = names(lithology_classes))
final_train$geomorph <- factor(final_train$geomorph, labels = names(geomorph_classes))
final_train$landcover19 <- factor(final_train$landcover19, labels = names(landcover_classes))
final_train$aspect <- factor(final_train$aspect, labels = aspect_classes)

# Create ggplot2 versions of cdplot and spineplot

# Numerical variables: cdplot using ggplot2
cdplot_elevation <- ggplot(final_train, aes(x = dtm)) +
  geom_density(aes(fill = slide), position = "fill", alpha = 0.6,bw = 20, col="white", size=1) +
  labs(title = "Elevation", x = "Elevation", y = "Density") +
  scale_fill_manual(values = c("#81b6d0", "#4f5f95"), name = "Landslide") +
  theme(legend.position = "none")

cdplot_plan_curv <- ggplot(final_train, aes(x = plan_curv)) +
  geom_density(aes(fill = slide), position = "fill", alpha = 0.6) +
  labs(title = "Plan Curvature", x = "Plan Curvature", y = "Density") +
  scale_fill_manual(values = c("#81b6d0", "#4f5f95"), name = "Landslide") +
  theme(legend.position = "none")

cdplot_prof_curv <- ggplot(final_train, aes(x = prof_curv)) +
  geom_density(aes(fill = slide), position = "fill", alpha = 0.6) +
  labs(title = "Profile Curvature", x = "Profile Curvature", y = "Density") +
  scale_fill_manual(values = c("#81b6d0", "#4f5f95"), name = "Landslide") +
  theme(legend.position = "none")

cdplot_rel_slp_position <- ggplot(final_train, aes(x = rel_slp_position)) +
  geom_density(aes(fill = slide), position = "fill", alpha = 0.6) +
  labs(title = "Relative Slope Position", x = "Relative Slope Position", y = "Density") +
  scale_fill_manual(values = c("#81b6d0", "#4f5f95"), name = "Landslide") +
  theme(legend.position = "none")

cdplot_slope <- ggplot(final_train, aes(x = slope)) +
  geom_density(aes(fill = slide), position = "fill", alpha = 0.6,bw = 3.4, col="white", size=1) +
  labs(title = "Slope", x = "Slope", y = "Density") +
  scale_fill_manual(values = c("#81b6d0", "#4f5f95"), name = "Landslide") +
  theme(legend.position = "none")

cdplot_twi <- ggplot(final_train, aes(x = twi)) +
  geom_density(aes(fill = slide), position = "fill", alpha = 0.6) +
  labs(title = "Topographic Wetness Index (TWI)", x = "TWI", y = "Density") +
  scale_fill_manual(values = c("#81b6d0", "#4f5f95"), name = "Landslide") +
  theme(legend.position = "none")

# Categorical variables: spineplot using ggplot2
spineplot_tpi <- ggplot(final_train, aes(x = tpi, fill = slide)) +
  geom_bar(position = "fill", alpha = 0.6) +
  labs(title = "Topographic Position Index (TPI)", x = "TPI", y = "Proportion") +
  scale_fill_manual(values = c("#81b6d0", "#4f5f95"), name = "Landslide") +
  theme(legend.position = "none") 

spineplot_landcover <- ggplot(final_train, aes(x = landcover19, fill = slide)) +
  geom_bar(position = "fill", alpha = 0.6) +
  labs(title = "Landcover", x = "Landcover", y = "Proportion") +
  scale_fill_manual(values = c("#81b6d0", "#4f5f95"), name = "Landslide") +
  theme(axis.text.x = element_text(angle = 0)) +
  theme(legend.position = "none") +
  annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 0, ymax = 1, color = "black", fill = NA, size = .8, linetype = "dashed")


spineplot_geomorph <- ggplot(final_train, aes(x = as.factor(geomorph), fill = slide)) +
  geom_bar(position = "fill", alpha = 0.6) +
  labs(title = "Geomorphology", x = "Geomorphology", y = "Proportion") +
  scale_fill_manual(values = c("#81b6d0", "#4f5f95"), name = "Landslide") +
  theme(legend.position = "none")

spineplot_geol <- ggplot(final_train, aes(x = geol, fill = slide)) +
  geom_bar(position = "fill", alpha = 0.6) +
  labs(title = "Geology", x = "Geology", y = "Proportion") +
  scale_fill_manual(values = c("#81b6d0", "#4f5f95"), name = "Landslide") +
  theme(axis.text.x = element_text(angle = 0)) +
  theme(legend.position = "none")

spineplot_aspect <- ggplot(final_train, aes(x = aspect, fill = slide)) +
  geom_bar(position = "fill", alpha = 0.6) +
  labs(title = "Aspect", x = "Aspect", y = "Proportion") +
  scale_fill_manual(values = c("#81b6d0", "#4f5f95"), name = "Landslide") +
  theme(legend.position = "none")


# # Combine plots in a single layout
# plots <- grid.arrange(
#   cdplot_elevation, spineplot_landcover, 
#   cdplot_slope, spineplot_geol, 
#   spineplot_tpi, 
#   spineplot_geomorph, 
#   ncol = 3,
#   nrow = 2,
#   heights = c(1, 1)
# )

layout_matrix <- rbind(
  c(1, 2, 3),
  c(4, 4, 5)
)

# Arrange the plots
plots <- grid.arrange(
  cdplot_elevation, spineplot_landcover, cdplot_slope, 
  spineplot_geol, #spineplot_tpi, 
  spineplot_geomorph, 
  layout_matrix = layout_matrix,
  heights = c(1, 1)
)


# Define the geology legend text, breaking it into lines
legend_geology <- "  A = Granito Pegmatóide; B = Granodiorito; Tonalito e Quartzodiorito; C = Gnaisse Migmatítico; D = Aluvião; E = Granito Cinza Porfirítico (Granito Favela); F = Gnaisse Granitóide; G = Aterro;
H = Sienito e Tinguaíto; I = Gnaisse Bandado (Archer); J = Biotita Gnaisse; K = Kinzigito; L = Leptinito;  M = Gnaisse Facoidal; N = Gnaisse Bandado; O = Quartzo; P = Sedimentary Deposits; Q = Other Rocks"

# Define the other legends
legend_geomorph <- "  1 = Concave Slopes - Divergent; 2 = Concave Slopes - Convergent; 3 = Convex Slopes - Divergent; 4 = Convex Slopes - Convergent; 5 = Flat Area; 6 = Artificial Scarps; 7 = Elevation Tops;
8 = Natural Scarps; 9 = Thalwegs"

legend_landcover <- "   A = Rocky Outcrops and Sedimentary Deposits; B = Forest and Shrub Cover; C = Grass and Woody Cover; D = Water Bodies; E = Favela; F = Residential Areas;
G = Agricultural and Undeveloped Areas; H = Industrial, Commercial, and Public Infrastructure"

# Create the full legend text
legend_full_text <- paste("Geology: ", legend_geology, "\n\n",
                          "Geomorphology: ", legend_geomorph, "\n\n",
                          "Land cover: ", legend_landcover, sep = "")


# Create the legend plot
legend_plot <- ggplot(final_train, aes(x = geol, fill = slide)) +
  geom_bar(position = "fill", alpha = 0.6) +
  scale_fill_manual(values = c("#81b6d0", "#4f5f95"), name = "Landslide") +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 10))

# Extract the legend as a separate grob
legend_grob <- ggplotGrob(legend_plot)$grobs[[which(sapply(ggplotGrob(legend_plot)$grobs, function(x) x$name) == "guide-box")]]

# Arrange the plots and legends side by side with the note
plots2 = grid.arrange(plots, ncol = 1,
             top = textGrob("Training Data exploratory analysis (overview for a few variables)",
                            x = 0, # starts far left
                            y = 0.5, # vertical placement
                            just = "left", # left-aligned
                            gp = gpar(fontsize = 18)), heights = c(1, .01), 
             bottom = arrangeGrob(legend_grob))

plots3 = grid.arrange(plots2, ncol = 1, heights = c(1, .05), 
             bottom = textGrob(legend_full_text,
                               x = 0,
                               y = 0.5,
                               just = "left",
                               gp = gpar(fontsize = 9) # smaller font for note
             )
)


# Save the one above the other combined plot as an image
ggsave("D:/PROslide_RIO/Figs/test.png", plot = plots3, width = 11.7, height = 8.3, units = "in")




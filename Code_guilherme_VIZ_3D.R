# Clear environment
rm(list = ls())

# Load required packages
library(mgcv)
library(visreg)
library(ggplot2)
library(cowplot)

# Set working directory
setwd("D:/PROslide_RIO/DATA2")

# Load data
load("final_train.Rd")

# Ensure factors are set correctly
final_train$landcover19 <- as.factor(final_train$landcover19)
final_train$geol        <- as.factor(final_train$geol)

# Fit GAM model (or use preloaded one)
gam_model <- gam(slide ~ s(slope, k = 2) +dtm,
                 data = final_train,
                 family = binomial)

# visreg with points (partial residuals)
p1 <- visreg(gam_model, "slope", scale = "response", rug = FALSE,
             line = list(lwd = 1), gg = TRUE) +
  #ggtitle("Effect of Slope on Landslide Probability") +
  ylab("Probability") +
  xlab("Slope")

p2 <- visreg(gam_model, "dtm", scale = "response", rug = FALSE,
             line = list(lwd = 1), gg = TRUE) +
  ylab("Probability") +
  xlab("Elevation (dtm)")

# Combine
combined_plot <- plot_grid(p1, p2, labels = c("a", "b"))

# Save
ggsave("D:/PROslide_RIO/Figs/GAM_visreg_with_points.png", plot = combined_plot,
       width = 30, height = 15, units = "cm")

# Print
print(combined_plot)


####################################################################
####################################################################
####################################################################
####################################################################


vis.gam(gam_model,
        view = c("slope", "dtm"), 
        zlab = "Landslide Probability",
        type = "response",
        plot.type = "persp",
        color = "terrain",            # optional: topo, terrain, heat.colors, etc.
        theta = 120,                  # rotate camera horizontally
        phi = 15,                    # tilt camera downward
        #ticktype = "detailed",        # 3D axes
        n.grid =25,                 # smoother grid
        zlim = c(0, 1),               # keep probabilities clean
        lwd = .02,                     # Line width (default is 1)
        border = "gray75",           # Line color (default is usually black)
        main = NULL)



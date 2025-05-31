library(raster)
library(ggplot2)
library(ggnewscale)
library(scales)
library(dplyr)
library(cowplot)
library(tidyterra)
library(ggspatial)


# Caminho base
base_path <- "D:/PROslide_RIO/DATA2/"

# Carregando os rasters
dtm         <- raster(paste0(base_path, "dtm.asc"))
slope       <- raster(paste0(base_path, "slope.asc"))
#aspect      <- raster(paste0(base_path, "aspect.asc"))
#tpi         <- raster(paste0(base_path, "tpi_class.asc"))
#landcover   <- raster(paste0(base_path, "landcover19.asc"))
#geol        <- raster(paste0(base_path, "geol.asc"))
#geomorph    <- raster(paste0(base_path, "geomorph.asc"))
hillshade <- raster("D:/PROslide_RIO/GUILHERME/hill_mdt_5m")

# Convert hillshade raster to data frame
hillshade_df <- as.data.frame(rasterToPoints(hillshade))
colnames(hillshade_df) <- c("x", "y", "hillshade")
saveRDS(hillshade_df, file = "D:/PROslide_RIO/DATA2/hillshade_df.rds")

# # Criando o stack
# brick <- stack(slope, dtm, aspect, tpi, landcover, geol, geomorph)
# names(brick) <- c("slope", "elevation", "aspect", "tpi", "landcover", "geol", "geomorph")

brick <- stack(slope, dtm)
names(brick) <- c("slope", "elevation")

# Carregando a máscara (trivial terrain = 0)
mask_raster <- raster("D:/PROslide_RIO/GUILHERME/slp_less_5_foc.tif")
mask_raster[mask_raster != 0] <- NA  # mantém apenas os pixels == 0

# Aplicar a máscara e salvar versão mascarada
masked_stack <- mask(brick, mask_raster)
#writeRaster(masked_stack, filename = "D:/PROslide_RIO/DATA2/masked_stack.tif", format = "GTiff", overwrite = TRUE)

# Criar data.frame da versão mascarada
brick_df_masked <- data.frame(rasterToPoints(masked_stack, na.rm = TRUE))
saveRDS(brick_df_masked, file = "D:/PROslide_RIO/DATA2/brick_df_masked.rds")
#brick_df <- readRDS("D:/PROslide_RIO/DATA2/brick_df_masked.rds")
# --------------------------------------------------------
# Também criar a versão completa com "tag" para o trivial terrain

# Versão sem aplicar mask
brick_df <- data.frame(rasterToPoints(brick, na.rm = TRUE))

# Carregar raster trivial terrain e converter
trivial_df <- data.frame(rasterToPoints(mask_raster))
colnames(trivial_df) <- c("x", "y", "trivial")

# Juntar e criar coluna de identificação
brick_df <- left_join(brick_df, trivial_df, by = c("x", "y"))
brick_df$terrain_type <- ifelse(!is.na(brick_df$trivial), "Trivial Terrain", "Rest of Area")

# Salvar versão com tagging
saveRDS(brick_df, file = "D:/PROslide_RIO/DATA2/brick_df_tagged.rds")
#brick_df <- readRDS("D:/PROslide_RIO/DATA2/brick_df_tagged.rds")


























###########################################################
###########################################################
###########################################################
#brick_df <- readRDS("D:/PROslide_RIO/DATA2/brick_df_masked.rds") %>% sample_n(1000)
brick_df <- readRDS("D:/PROslide_RIO/DATA2/brick_df_tagged.rds") #%>% sample_n(10000)
hillshade_df <- readRDS("D:/PROslide_RIO/DATA2/hillshade_df.rds") #%>% sample_n(10000)
colnames(hillshade_df) <- c("x", "y", "hillshade")


# # Define a small bounding box (adjust if needed)
# xmin <- 660000
# xmax <- 662000
# ymin <- 7460000
# ymax <- 7462000
# 
# # Subset hillshade and brick_df
# hillshade_tiny <- hillshade_df %>%
#   filter(x >= xmin, x <= xmax, y >= ymin, y <= ymax)
# 
# brick_df_tiny <- brick_df %>%
#   filter(x >= xmin, x <= xmax, y >= ymin, y <= ymax)


#head(brick_df)
###########################################################
###########################################################
###########################################################

# Raster maps
p_raster_slope <- ggplot() +
  # Hillshade as grayscale background
  geom_raster(data = hillshade_df, aes(x = x, y = y, fill = hillshade)) +
  scale_fill_gradient(low = "black", high = "white", guide = FALSE) +
  
  # New fill scale for slope
  ggnewscale::new_scale_fill() +
  
  # Slope on top
  geom_raster(data = brick_df, aes(x = x, y = y, fill = slope), alpha = 0.6) +
  scale_fill_viridis_c(name = "Slope (°)", direction=-1,
                       guide = guide_colorbar(barwidth = 15))+ #https://dieghernan.github.io/tidyterra/reference/scale_hypso.html
  #scale_fill_gradient2(name = "Slope (°)",low = 'blue', mid = 'white', high = 'red', guide = guide_colorbar(barwidth = 15)) +
  
  labs(title = "C) Slope Map")+
  coord_equal() +
  # Add scale and north arrow
  annotation_scale(location = "bl", width_hint = 0.2, line_col = "gray30",  text_col = "gray30" )+
  annotation_north_arrow( location = "bl",which_north = "true",pad_y = unit(0.2, "in"),
                          style = north_arrow_fancy_orienteering(
                            fill = c("gray30", "gray70"),
                            line_col = "gray30" )) +
  #theme_void() +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_blank())





# 1. Get elevation range directly from brick_df
r_limits <- range(brick_df$elevation, na.rm = TRUE)
# 2. Round DOWN the min to nearest 100 and UP the max to nearest 100
r_limits <- c(0, ceiling(r_limits[2] / 100)) * 100
# 3. Optional: enforce maximum limit (e.g., cap at 1200m if your data stops at ~1100)
r_limits[2] <- min(r_limits[2], 1200)

# 2. Build hypsometric palette
grad_hypso <- c(
  "#006837",  # deep green
  "#78C679",  # light green
  #"#FEE08B",  # yellow
  "#F46D43",  # orange-red
  "#A50026",  # dark red
  "#FFFFFF"   # white
)

p_raster_dtm <- ggplot() +
  # Hillshade as grayscale background
  geom_raster(data = hillshade_df, aes(x = x, y = y, fill = hillshade)) +
  scale_fill_gradient(low = "black", high = "white", guide = FALSE) +
  
  # New fill scale for slope
  ggnewscale::new_scale_fill() +
  
  # DTM
  geom_raster(data = brick_df, aes(x = x, y = y, fill = elevation), alpha = 0.6) +
  scale_fill_gradientn(
    colours = grad_hypso,
    values = scales::rescale(c(0, 100, 200, 300, 400, 500, 600, 1000)),
    limits = r_limits,
    name = "Elevation (m)",
    guide = guide_colorbar(barwidth = 15),
    na.value = NA
  ) +
  labs(title = "F) Elevation Map")+
  
  # tidyterra::scale_fill_hypso_b(breaks = seq(0, 1250, 200), palette = "colombia_hypso",
  #                               guide = guide_colorbar(barwidth = 15),
  #                               name = "Elevation (m)")+
  # #scale_fill_viridis_c(name = "Elevation (m)")+
  
  
  coord_equal() +
  # Add scale and north arrow
  annotation_scale(location = "bl", width_hint = 0.2, line_col = "gray30",  text_col = "gray30" )+
  annotation_north_arrow( location = "bl",which_north = "true", pad_y = unit(0.2, "in"),
                          style = north_arrow_fancy_orienteering(
    fill = c("gray30", "gray70"),
    line_col = "gray30" )) +
  #theme_void() +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_blank())


###########################################################
###########################################################

# Histograma da declividade
# Histogram slope
p_hist_slope <- ggplot(brick_df, aes(x = slope)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 90, fill = "gray70", color = "black",
                  linewidth=0.2) +
  scale_x_continuous(name = "Slope (°)", limits = c(0, 90)) +
  labs(title = "A) Frequency distribution of slope angles")+
  
  scale_y_continuous(name = "% of pixels in the study area", labels = percent_format(), limits = c(0, .15)) 

# Histogram elevation
p_hist_dtm <- ggplot(brick_df, aes(x = elevation)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 100, fill = "gray70", color = "black",
                 linewidth=0.2) +
  scale_x_continuous(name = "Elevation (m)", limits = c(0, 1000)) +
  labs(title = "D) Frequency distribution of elevation") +
  scale_y_continuous(name = "% of pixels in the study area", labels = percent_format(), limits = c(0, .15)) 













###########################################################
###########################################################
#################################################
load("D:/PROslide_RIO/DATA2/final_train.Rd")

# ---- Slope proportional density (Landslide on bottom) ----
p_stack_slope <- ggplot(final_train, aes(x = slope, fill = slide)) +
  geom_density(position = "fill", alpha = 0.75, color = NA) +
  scale_fill_manual(values = c("FALSE" = "#00bfc4", "TRUE" = "#f8766d"),labels = c("no-slide", "slide"))+
  scale_y_continuous(labels = percent_format(), name = "Proportion of the training samples") +
  scale_x_continuous(name = "Slope (°)", limits = c(0, 90)) +
  labs(title = "B) Training sample density by slope") +
  theme(#plot.title = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.7, 0.9),            # inside top-center
        legend.justification = c(0.5, 1),
        legend.direction = "horizontal")

# ---- Elevation proportional density ----
p_stack_dtm <- ggplot(final_train, aes(x = dtm, fill = slide)) +
  geom_density(position = "fill", alpha = 0.75, color = NA) +
  scale_fill_manual(values = c("FALSE" = "#00bfc4", "TRUE" = "#f8766d"),labels = c("no-slide", "slide"))+
  scale_y_continuous(labels = percent_format(), name = "Proportion of the training samples") +
  scale_x_continuous(name = "Elevation (m)", limits = c(0, 1000)) +
  labs(title = "E) Training sample density by elevation") +
  theme(#plot.title = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.7, 0.9),            # inside top-center
        legend.justification = c(0.5, 1),
        legend.direction = "horizontal")


# # Compose figure
# row_slope <- plot_grid(p_raster_slope, p_hist_slope, p_stack_slope, nrow = 1, rel_widths = c(1.8, 1, 1))
# row_dtm   <- plot_grid(p_raster_dtm, p_hist_dtm, p_stack_dtm,     nrow = 1, rel_widths = c(1.8, 1, 1))
# 
# final_plot <- plot_grid(row_slope, row_dtm, ncol = 1, rel_heights = c(1, 1))
# 
# # Save
# ggsave("D:/PROslide_RIO/Figs/slope_dtm_composite2.png", final_plot, width = 16, height = 9, dpi = 300)

############################################################
# Arrange slope plots in a column: top = hist + stack, bottom = map
# Slope column (top row of hist + stack, bottom row = map)
col_slope <- plot_grid(
  plot_grid(p_hist_slope, p_stack_slope, ncol = 2),
  p_raster_slope,
  ncol = 1,
  rel_heights = c(1, 1.3),
  
  label_y = c(1, 0.9)
)

# Elevation column
col_dtm <- plot_grid(
  plot_grid(p_hist_dtm, p_stack_dtm, ncol = 2),
  p_raster_dtm,
  ncol = 1,
  rel_heights = c(1, 1.3),
  
  label_y = c(1, 0.9)
)

# Final layout: slope on the left, dtm on the right
final_plot <- plot_grid(col_slope, col_dtm, ncol = 2, labels = NULL, label_size = .8, rel_widths = c(1, 1))

ggsave("D:/PROslide_RIO/Figs/slope_dtm_composite_new3.png", final_plot, width = 16, height = 9, dpi = 300)


# ## Aspect
# Rótulos cardeais completos
# aspect_labels <- c(
#   "1" = "Norte (N)",
#   "2" = "Nordeste (NE)",
#   "3" = "Leste (E)",
#   "4" = "Sudeste (SE)",
#   "5" = "Sul (S)",
#   "6" = "Sudoeste (SW)",
#   "7" = "Oeste (W)",
#   "8" = "Noroeste (NW)"
# )
# 
# # Aplicando ao dataframe
# brick_df$aspect_label <- aspect_labels[as.character(brick_df$aspect)]
# 
# # Gráfico com rótulos descritivos
# p_aspect <- ggplot(brick_df, aes(x = aspect_label)) +
#   geom_bar(aes(y = ..count../sum(..count..)), fill = "skyblue") +
#   scale_y_continuous(labels = percent_format()) +
#   labs(title = "C - Orientação (Aspect)", x = "Orientação cardeal", y = "") +
#   theme_minimal(base_size = 14) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# p_aspect


# ## TPI
# # Legendas descritivas para TPI
# tpi_labels <- c(
#   "1" = "Vale (Valley)",
#   "2" = "Encosta inferior (Lower slope)",
#   "3" = "Encosta média (Mid slope)",
#   "4" = "Área plana (Flat)",
#   "5" = "Encosta superior (Upper slope)",
#   "6" = "Topo / crista (Ridge)"
# )
# 
# # Aplicando ao dataframe
# brick_df$tpi_label <- tpi_labels[as.character(brick_df$tpi)]
# 
# # Gráfico com rótulos descritivos
# p_tpi <- ggplot(brick_df, aes(x = tpi_label)) +
#   geom_bar(aes(y = ..count../sum(..count..)), fill = "orange") +
#   scale_y_continuous(labels = percent_format()) +
#   labs(title = "D - Posição Topográfica (TPI)", x = "Classe TPI", y = "") +
#   theme_minimal(base_size = 14) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# p_tpi



# 
# 
# # Landcover legendas
# landcover_labels <- c(
# 1 = "Afloramentos rochosos e depósitos sedimentares"
# 2 = "Cobertura arbórea e arbustiva"
# 3 = "Cobertura gramíneo-lenhosa"
# 4 = "Corpos hídricos e áreas sujeitas à inundação"        (4, 16)
# 5 = "Favela"
# 6 = "Áreas residenciais"
# 7 = "Áreas agrícolas e não edificadas"                    (6, 14)
# 8 = "Infraestrutura pública, comercial e industrial"      (7, 8, 9, 10, 11, 12, 13)
# )
# 
# brick_df$landcover_label <- landcover_labels[as.character(brick_df$landcover)]
# 
# # Plot atualizado
# p_lc <- ggplot(brick_df, aes(x = landcover_label)) +
#   geom_bar(aes(y = ..count../sum(..count..)), fill = "forestgreen") +
#   scale_y_continuous(labels = percent_format()) +
#   labs(title = "E - Landcover", x = "Uso da Terra", y = "") +
#   theme_minimal(base_size = 14) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# p_lc




# 
# ## Geology
# # Geology legendas
# geol_labels <- c(
#   "1"  = "Granito Pegmatóide",
#   "2"  = "Granodiorito/Tonalito/Quartzodiorito",
#   "3"  = "Gnaisse Migmatítico",
#   "4"  = "Aluvião",
#   "5"  = "Granito Cinza Porfirítico",
#   "6"  = "Gnaisse Granitóide",
#   "7"  = "Aterro",
#   "8"  = "Sienito/Tinguaito",
#   "9"  = "Gnaisse Bandado (Archer)",
#   "10" = "Biotita Gnaisse",
#   "11" = "Kinzigito",
#   "12" = "Leptinito",
#   "13" = "Gnaisse Facoidal",
#   "14" = "Gnaisse Bandado",
#   "15" = "Quartzo Gabro/Diorito",
#   "16" = "Dunas",
#   "17" = "Outro"
# )
# 
# brick_df$geol_label <- geol_labels[as.character(brick_df$geol)]
# 
# # Plot atualizado
# p_geol <- ggplot(brick_df, aes(x = geol_label)) +
#   geom_bar(aes(y = ..count../sum(..count..)), fill = "tan") +
#   scale_y_continuous(labels = percent_format()) +
#   labs(title = "F - Litologia", x = "Unidade Litológica", y = "") +
#   theme_minimal(base_size = 14) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# p_geol
# # 





# ## Geomorphology
# Legendas descritivas da geomorfologia
# geomorph_labels <- c(
#   "1" = "Vertentes Côncavas - Divergentes",
#   "2" = "Vertentes Côncavas - Convergentes",
#   "3" = "Vertentes Convexas - Divergentes",
#   "4" = "Vertentes Convexas - Convergentes",
#   "5" = "Área Plana",
#   "6" = "Escarpas artificiais",
#   "7" = "Topos de elevação",
#   "8" = "Escarpas naturais",
#   "9" = "Talvegues"
# )
# 
# # Aplicando ao dataframe
# brick_df$geomorph_label <- geomorph_labels[as.character(brick_df$geomorph)]
# 
# # Gráfico atualizado
# p_geomorph <- ggplot(brick_df, aes(x = geomorph_label)) +
#   geom_bar(aes(y = ..count../sum(..count..)), fill = "steelblue") +
#   scale_y_continuous(labels = percent_format()) +
#   labs(title = "G - Geomorfologia", x = "Classe Geomorfológica", y = "") +
#   theme_minimal(base_size = 14) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# p_geomorph



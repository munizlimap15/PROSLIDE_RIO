library(sf)
library(raster)
library(dplyr)
library(sp)
library(dplyr)

pred                = raster("D:/PROslide_RIO/Susc_heuristic/suscetibilidade_rio.tif")
# Assuming 'pred' is your raster object
# 1. Get the frequency of each unique value
frequency <- freq(pred)

# 2. Calculate the resolution of the raster cells
res_values <- res(pred)
cell_area <- res_values[1] * res_values[2] # Cell area = resolution x * resolution y

# 3. Calculate the area for each unique value
# frequency[,1] contains the unique values, frequency[,2] contains their counts
areas <- frequency[,2] * cell_area

# Combine the unique values with their corresponding areas into a data frame
area_df <- data.frame(unique_value = frequency[,1], area = areas)

# Display the resulting dataframe
print(area_df)

# Exclude the NA values (0 and 255)
area_df <- subset(area_df, unique_value != 0 & unique_value != 255)

# Calculate the total area of the remaining values
total_area <- sum(area_df$area)

# Calculate the percentages for each unique value
area_df$percentage <- (area_df$area / total_area) * 100

# Display the updated dataframe with percentages
print(area_df)
#unique_value      area percentage
#2            1 694468362   58.79
#3            2 341647868   28.92
#4            3 145003905   12.27



################################################################################

Limite_Favelas_2019 <- st_read("D:/PROslide_RIO/Rcodes/Shinny_app_RioSlide/Limite_Favelas_2019.shp")
Limite_Favelas_2019 <- st_simplify(Limite_Favelas_2019, dTolerance = 1)
Limite_Favelas_2019 <- Limite_Favelas_2019 %>%
  filter(pop_sabren > 100)

# Transforme o sistema de coordenadas das Limite_Favelas_2019 para corresponder ao do raster
Limite_Favelas_2019 <- st_transform(Limite_Favelas_2019, crs = crs(pred))

# Aplique a máscara do raster usando as geometrias das Limite_Favelas_2019
pred_Limite_Favelas_2019 <- mask(pred, Limite_Favelas_2019)

# Agora, repita o processo de cálculo de área e porcentagem para o raster mascarado
frequency_Limite_Favelas_2019 <- freq(pred_Limite_Favelas_2019, useNA="no")

# Cálculo da área de cada valor único
res_values <- res(pred_Limite_Favelas_2019)
cell_area <- res_values[1] * res_values[2]
areas_Limite_Favelas_2019 <- frequency_Limite_Favelas_2019[,2] * cell_area

# Combine os valores únicos com suas áreas correspondentes em um data frame
area_df_Limite_Favelas_2019 <- data.frame(unique_value = frequency_Limite_Favelas_2019[,1], area = areas_Limite_Favelas_2019)

# Calcule a área total e as porcentagens como antes
total_area_Limite_Favelas_2019 <- sum(area_df_Limite_Favelas_2019$area)
area_df_Limite_Favelas_2019$percentage <- (area_df_Limite_Favelas_2019$area / total_area_Limite_Favelas_2019) * 100

# Exibir o data frame atualizado
print(area_df_Limite_Favelas_2019)

#unique_value        area  percentage
#1            1 21170466.87 44.8
#2            2 16271405.49 34.4
#3            3  9793164.18 20.8



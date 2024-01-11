stations_df <- data.frame(
  Estação = c("Vidigal", "Urca", "Rocinha", "Tijuca", "Santa Teresa", 
              "Copacabana", "Grajaú", "Ilha do Governador", "Penha", "Madureira",
              "Irajá", "Bangu", "Piedade", "Jacarepaguá/Tanque", "Saúde",
              "Jardim Botânico", "Barra/Barrinha", "Jacarepaguá/Cidade de Deus", 
              "Barra/Riocentro", "Guaratiba", "Est. Grajaú/Jacarepaguá", 
              "Santa Cruz", "Grande Méier", "Anchieta", "Grota Funda", 
              "Campo Grande", "Sepetiba", "Alto da Boa Vista", "Av. Brasil/Mendanha", 
              "Recreio dos Bandeirantes", "Laranjeiras", "São Cristóvão", "Tijuca/Muda"),
  Cota_m = c(85, 90, 160, 340, 170, 90, 80, 0, 111, 45, 20, 15, 50, 73, 15, 0, 7, 15, 
             0, 0, 105, 15, 25, 50, 11, 30, 62, 355, 30, 10, 60, 25, 31),
  X = c(681138.532, 688004.213, 679831.802, 682358.108, 684951.792, 685675.030, 
        677639.269, 683708.659, 677059.917, 670409.679, 670692.602, 657403.761, 
        673344.642, 667541.219, 685875.072, 682133.530, 674262.081, 667928.198, 
        664879.400, 643972.241, 672722.551, 634915.936, 676628.743, 663886.431, 
        651526.394, 647538.728, 632068.601, 676494.085, 649669.378, 659816.802, 
        685883.625, 682404.960, 680136.137),
  Y = c(7456241.298, 7460236.157, 7457041.035, 7462941.416, 7462971.838, 
        7456902.449, 7463809.403, 7475959.609, 7472757.104, 7469665.020, 
        7474733.927, 7468956.662, 7467452.646, 7465482.186, 7466833.239, 
        7458453.116, 7454520.709, 7461632.847, 7458100.000, 7450214.260, 
        7463726.025, 7465594.315, 7467665.546, 7474809.856, 7454108.412, 
        7466493.819, 7459406.612, 7459222.483, 7471561.078, 7454514.151, 
        7462252.656, 7466817.174, 7462851.843)
)

stations_df

library(sp)
library(gstat)

# Convert to SpatialPointsDataFrame
coordinates(stations_df) <- ~X+Y
# Assuming your original coordinates are in SAD69 / UTM Zone 23S (as an example)
proj4string(stations_df) <- CRS("+proj=utm +zone=23 +south +ellps=aust_SA +units=m +no_defs")

# Export as shapefile
shapefile_name <- "stations"
writeOGR(stations_df, "E:/PROslide_RIO/DATA", shapefile_name, driver="ESRI Shapefile", layer=shapefile_name)
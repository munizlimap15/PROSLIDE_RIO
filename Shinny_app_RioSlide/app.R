library(sf)
library(leaflet)
library(dplyr) 


# source_dir <- "D:/PROslide_RIO/DATA"
# dest_dir <- getwd()
# 
# # Base names of the shapefiles without extensions
# base_names <- c("landslides_2023", "StudyArea")
# 
# # Extensions of the shapefile components
# extensions <- c(".shp", ".shx", ".dbf", ".prj", ".sbn", ".sbx")
# 
# # Copy each component of each shapefile
# for (base_name in base_names) {
#   for (extension in extensions) {
#     file_name <- paste0(base_name, extension)
#     source_file_path <- file.path(source_dir, file_name)
#     dest_file_path <- file.path(dest_dir, file_name)
#     
#     # Check if the source file exists before attempting to copy
#     if (file.exists(source_file_path)) {
#       file.copy(source_file_path, dest_file_path)
#     }
#   }
# }
# 

final_rioslides <-sf::st_read("landslides_2023.shp")
study_area <- st_read("StudyArea.shp")

# Ensure LA and MoNEW are in the same CRS
final_rioslides <- sf::st_transform(final_rioslides, st_crs(study_area))

# Transform both to WGS84
study_area_wgs84 <- st_transform(study_area, 4326)
final_rioslides_wgs84 <- st_transform(final_rioslides, 4326)
# Removing rows with NA coordinates
coords <- st_coordinates(final_rioslides_wgs84)
final_rioslides_wgs84 <- final_rioslides_wgs84[!is.na(coords[, 'X']) & !is.na(coords[, 'Y']), ]


# # Your leaflet code
# m <- leaflet(options = leafletOptions(minZoom = 9, maxZoom = 12)) %>%
#   addTiles() %>% leafem:: addMouseCoordinates()%>%
#   leaflet.extras::addHeatmap(data = st_coordinates(final_rioslides_wgs84), max = 30, radius = 10, blur = 10) %>%
#   addCircleMarkers(data = final_rioslides_wgs84, 
#                    radius = 1/2,      # adjust as desired
#                    color = "blue",  # adjust as desired
#                    stroke = FALSE,  # to remove the border
#                    fillOpacity = .9) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = study_area_wgs84, color = "black", fillOpacity = 0, weight = 3) %>%
#   
#   addMiniMap(position = "topleft", tiles = providers$OpenStreetMap.Mapnik, toggleDisplay = TRUE) %>%
#   addScaleBar()
# 
# # Save the map and screenshot
# mapview::mapshot(m, url = "D:/PROslide_RIO/Figs/invent_heatmap_pts.html")
# webshot::webshot("D:/PROslide_RIO/Figs/invent_heatmap.png")

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


library(shiny)
library(leaflet)
library(sf)

ui <- fluidPage(
  titlePanel("RioSlide"),
  div(style = "text-align: center;", img(src = "Proj_partners.jpg", height = "100px")),  
  h3("Landslide distribution RJ"),
  
  sidebarLayout(
    sidebarPanel(width = "0%", height = "0px"
      # Other sidebar elements if needed
    ),
    mainPanel(
      leafletOutput("map", width = "150%", height = "600px"),
      
      # Using absolutePanel to position the slider over the map
      absolutePanel(bottom = 750, left = 10, # Adjust position as needed
                    style = "background-color: white; padding: 10px; border-radius: 5px;",
                    sliderInput("yearSlider", "Year:",
                                min = 2009,
                                max = max(final_rioslides_wgs84$anolaudo),
                                value = range(final_rioslides_wgs84$anolaudo),
                                step = 1)
      ),
            h6("Data Protection and Map Usage Disclaimer: This map's zoom level is controled to balance detail with data protection. 
      Precise locations are hidden to protect privacy and comply with data protection regulations. For detailed inquiries or data concerns, 
      contact the appropriate authority.", a(href = "https://www.rio.rj.gov.br/web/georio/quem-somos", target="_blank", strong("Source: GeoRIO"))),
            
      h5("Code& Data Availability:"),
      p("The data and code for this study are accessible via the following links: ",
         a(href = "http://alertario.rio.rj.gov.br/download/dados-pluviometricos/", target = "_blank", em("Rainfall dataset")),
         " and ",
         a(href = "https://github.com/munizlimap15/PROSLIDE_RIO", target = "_blank", em("Code")),
         ". Data analysis was conducted in the R environment, with the software available for free at ",
        a(href = "https://www.r-project.org/", target = "_blank", "R Project"),
        ".", "Additional datasets, including landslide occurrences and predictive variables, may be made available upon formal request to the owning organization. Interested parties should contact the relevant authority to inquire about data access conditions and potential collaboration opportunities."
      ),
      
      
      # Project details and image at the top
            h5("Project Details"),
            p("This project is conducted by the following members:"),
            
            p(strong("Pedro Henrique Muniz Lima"), " (main responsible) - University of Vienna, Institute of Geography and Regional Research, Wien, Austria. ", 
            strong("Email:"), " ", a(href="mailto:pedro.lima@univie.ac.at", "pedro.lima@univie.ac.at"), ". ", strong("Webpage:"), " ", 
            a(href="https://munizlimap15.github.io/Pedrolima/", target="_blank", "Personal webpage")),
            
            p("Luiz Carlos Teixeira Coelho - Instituto Municipal de Urbanismo Pereira Passos - IPP, Rio de Janeiro, Brazil."),
            p("Mateo Moreno Zapata - Eurac Research, Institute for Earth Observation, Bozen, Italy."),
            p("Stefan Steger - Geosphere, Vienna, Austria."),
            p("Pedro Ivo Camarinha - Centro Nacional de Monitoramento e Alertas de Desastres Naturais, Ministry of Science, Technology and Innovation of Brazil, São José dos Campos, Brazil."),
            p("Felipe Cerbella Mandarino - Instituto Municipal de Urbanismo Pereira Passos - IPP, Rio de Janeiro, Brazil."),
            p("Raquel Batista Medeiros da Fonseca - Fundação Geo - Rio, Prefeitura do Rio de Janeiro, Rio de Janeiro, Brazil."),
            p("Guilherme Damasceno Raposo - Universidade do Estado do Rio de Janeiro, Department of Cartographic Engineering, Rio de Janeiro, Brazil."),
            p("Thomas Glade - University of Vienna, Institute of Geography and Regional Research, Wien, Austria."),
  
            div(style = "text-align: center;", img(src = "Proj_partners.jpg", height = "100px")),
            
            h3("Research outcomes:"),
      p(strong("Pedro Lima"), " Moreno M, Steger S, Camarinha PI, Coelho LCT, Mandarino FC and Glade T (2023a) ", 
        strong("DEVELOPING A SPATIOTEMPORAL MODEL TO INTEGRATE LANDSLIDE SUSCEPTIBILITY AND CRITICAL RAINFALL CONDITIONS. A PRACTICAL MODEL APPLIED TO RIO DE JANEIRO MUNICIPALITY."),
        " In: Tofani V, Casagli N, Bandecchi E, Gargini E and Armignacco D (eds.) Landslide Science for Sustainable Development. Proceedings of the 6th World Landslide Forum. Firenze, Italy: OIC S.r.l. ISBN 9791221048063. ", 
        a(href="https://wlf6.org/wp-content/uploads/2023/11/WLF6_ABSTRACT-BOOK.pdf", target="_blank", "See here")),
      
    )
  )
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    
    # Filter the data based on the selected year
    filtered_data <- final_rioslides_wgs84 %>%
      filter(anolaudo >= input$yearSlider[1], anolaudo <= input$yearSlider[2])
    
    # Your leaflet code, but use 'filtered_data' for the heatmap and markers
    m <- leaflet(options = leafletOptions(minZoom = 10, maxZoom = 14)) %>%
      addTiles() %>%
      leafem::addMouseCoordinates() %>%
      leaflet.extras::addHeatmap(data = st_coordinates(filtered_data), max = 30, radius = 10, blur = 10) %>%
      addCircleMarkers(data = filtered_data, 
                       radius = 1,      # adjust as desired
                       color = "blue",  # adjust as desired
                       stroke = FALSE,  # to remove the border
                       fillOpacity = .6) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = study_area_wgs84, color = "black", fillOpacity = 0, weight = 3) %>%
      
      addMiniMap(position = "topleft", tiles = providers$OpenStreetMap.Mapnik, toggleDisplay = TRUE) %>%
      addScaleBar()
    m  # Return the leaflet map
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


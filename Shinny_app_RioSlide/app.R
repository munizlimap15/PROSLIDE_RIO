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
Limite_Favelas_2019 <- st_read("Limite_Favelas_2019.shp")

# Ensure LA and MoNEW are in the same CRS
final_rioslides <- sf::st_transform(final_rioslides, st_crs(study_area))
Limite_Favelas_2019 <- sf::st_transform(Limite_Favelas_2019, st_crs(study_area))

# Transform both to WGS84
study_area_wgs84 <- st_transform(study_area, 4326)
final_rioslides_wgs84 <- st_transform(final_rioslides, 4326)
Limite_Favelas_2019 <- st_transform(Limite_Favelas_2019, 4326)
# Removing rows with NA coordinates
coords <- st_coordinates(final_rioslides_wgs84)
final_rioslides_wgs84 <- final_rioslides_wgs84[!is.na(coords[, 'X']) & !is.na(coords[, 'Y']), ]




library(shiny)
library(leaflet)
library(sf)

ui <- fluidPage(
  #titlePanel("RioSlide"),
  #div(style = "text-align: left;", img(src = "rioSlide_logo.jpg", height = "200px")),
  div(style = "text-align: left;", img(src = "Untitled.jpg", height = "200px")),
  #p(readLines("abstract.txt")),
    # Using tabsetPanel to organize content into tabs
  tabsetPanel(
    # Map and Data Visualization Tab
    tabPanel("Map and Data",
             sidebarLayout(
               sidebarPanel(width = "0%", height = "0px"),
               mainPanel(
                 h3("Landslide Distribution in RJ"),
                 leafletOutput("map", width = "100%", height = "600px"),
                 #div(
                   absolutePanel(
                     id = "controls", class = "panel panel-default",
                     top = "500px", left = "10px", # Adjust the top and left values as needed
                     width = 400, height = "auto",
                     style = "z-index: 400; background-color: rgba(255, 255, 255, 0.8); padding: 10px; border-radius: 5px;",
                     sliderInput("yearSlider", "Year:",
                                 min = 2009,
                                 max = max(final_rioslides_wgs84$anolaudo),
                                 value = range(final_rioslides_wgs84$anolaudo),
                                 step = 1)
                   #),
                   #style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%;"
                 ),
                 h6("Data Protection and Map Usage Disclaimer: This map's zoom level is controlled to balance detail with data protection. 
                    Precise locations are hidden to protect privacy and comply with data protection regulations. For detailed inquiries or data concerns, 
                    contact the appropriate authority.", 
                    a(href = "https://www.rio.rj.gov.br/web/georio/quem-somos", target = "_blank", strong("Source: GeoRIO"))),
                 #div(style = "text-align: center;", img(src = "Proj_partners.jpg", height = "100px")), 
               )
             )
    ),
    
    # Description Tab
    tabPanel("Description",
             p(readLines("abstract.txt")),
             p("This project is conducted by an international team of experts in landslides, spatial analysis, and disaster management. Our initiative is centered around the development and implementation of innovative approaches to address the complex challenges posed by landslides. The project distinguishes itself through its multifaceted objectives, which include:"),
             
             # Detailed project description in bullet points
             tags$ul(
               tags$li(strong("Scientific Impact:"),"Advancing the understanding of landslide dynamics and fostering innovation in geosciences."),
               tags$li(strong("Technological Advancement:"),"Developing robust tools for landslide prediction."),
               tags$li(strong("Societal Benefit:"),"Reducing susceptibility to landslides, enhancing preparedness, and promoting informed decision-making."),
               tags$li(strong("Blueprint for Broader Application:"),"Establishing a comprehensive model for other Brazilian municipalities to adapt, utilize and develop further."),
               tags$li(strong("Collaborative Spirit:")," Inviting partnerships and contributions to enrich and expand the project."),
               tags$li(strong("Commitment to Ethical Standards:"),"Upholding integrity and transparency, devoid of any financial interests."),
               tags$li(strong("Data Protection and Privacy:"),"Adhering to data protection regulations with a commitment to privacy. Contact ",
                       a(href="mailto:pedro.lima@univie.ac.at", "Pedro Lima"),
                       " for data-related inquiries."
               )
             ),
             #div(style = "text-align: center;", img(src = "Proj_partners.jpg", height = "100px")),
             p(strong("Acknowledgement:")),
             p("While this project is currently self-funded and does not receive financial support, it is designed in alignment with the guidelines of the Brazilian National Council for Scientific and Technological Development (CNPq) and relates to the process number 234815/2014-0. The project team recognizes the vital role of CNPq in promoting scientific and technological development in Brazil and appreciates the framework it provides for research initiatives."),
             div(style = "text-align: center;", img(src = "CNPq.png", height = "100px")),
    ),
    
    # Collaborators Tab
    tabPanel("Collaborators",
             
             p("This project is conducted by the following members:"),
             
             tags$ul(
               tags$li(
                 div(
                   strong("Pedro Lima"), " (Main responsible) - University of Vienna, Institute of Geography and Regional Research, Wien, Austria. ",
                   div(
                     style = "margin-left: 20px;",  # Adjust the value as needed to align to the right
                     tags$div(
                       "Email:", " ", a(href="mailto:pedro.lima@univie.ac.at", "pedro.lima@univie.ac.at"),
                       tags$br(),
                       "Webpage:", " ", a(href="https://munizlimap15.github.io/Pedrolima/", target="_blank", "Personal webpage"),
                       tags$br(),
                       "Professional Website:", " ", a(href="http://geomorph.univie.ac.at/", target="_blank", "geomorph.univie.ac.at"),
                       tags$br(),
                       "ResearchGate:", " ", a(href="https://www.researchgate.net/profile/Pedro-Lima-2/", target="_blank", "Pedro Lima - ResearchGate"),
                       tags$br(),
                       "ORCID:", " ", a(href="https://orcid.org/0000-0003-2429-3752", target="_blank", "0000-0003-2429-3752"),
                       tags$br(),
                       "Lattes:", " ", a(href="https://munizlimap15.github.io/Pedrolima/", target="_blank", "Lattes - Pedro Lima")
                     )
                   )
                 )
               ),
               
               tags$li(
                 div(
                   strong("Luiz Carlos Teixeira Coelho"), " - Instituto Municipal de Urbanismo Pereira Passos - IPP, Rio de Janeiro, Brazil.",
                   div(
                     style = "margin-left: 20px;",
                     tags$div(
                       "Email:", " ", a(href="mailto:luiz.coelho@eng.uerj.br", "luiz.coelho@eng.uerj.br"),  # Replace with actual email
                       tags$br(),
                       "Webpage:", " ", a(href="https://example.com", target="_blank", "Example Webpage"),  # Replace with actual webpage
                       tags$br(),
                       "ORCID:", " ", a(href="https://orcid.org/0000-0002-4466-9772", target="_blank", "0000-0002-4466-9772")  # Replace with actual ORCID
                     )
                   )
                 )
               ),
               
               tags$li(
                       div(
                         strong("Mateo Moreno Zapata"), " - Eurac Research, Institute for Earth Observation, Bozen, Italy.",
                         div(
                           style = "margin-left: 20px;",
                           tags$div(
                             "Email:", " ", a(href="mailto:mateo.morenozapata@eurac.edu", "mateo.morenozapata@eurac.edu"),  # Replace with actual email
                             tags$br(),
                             "Webpage:", " ", a(href="https://example.com", target="_blank", "Example Webpage"),  # Replace with actual webpage
                             tags$br(),
                             "ORCID:", " ", a(href="https://orcid.org/0000-0002-9530-3076", target="_blank", "0000-0002-9530-3076")  # Replace with actual ORCID
                             
                           )))),
               
               tags$li(
                 div(
                   strong("Stefan Steger"), " - GeoSphere Austria – Austria ́s national geological, geophysical, climatological and meteorological service - RiskLab – Weather, Climate & Natural Hazards.",
                   div(
                     style = "margin-left: 20px;",
                     tags$div(
                       "Email:", " ", a(href="mailto:stefan.steger@geosphere.at", "stefan.steger@geosphere.at"),  # Replace with actual email
                       tags$br(),
                       "Webpage:", " ", a(href="https://example.com", target="_blank", "Example Webpage"),  # Replace with actual webpage
                       tags$br(),
                       "ORCID:", " ", a(href="https://orcid.org/0000-0003-0886-5191", target="_blank", "0000-0003-0886-5191")  # Replace with actual ORCID
                       
                     )))),
               tags$li(
                 div(
                   strong("Pedro Ivo Camarinha"), " - Centro Nacional de Monitoramento e Alertas de Desastres Naturais, Ministry of Science, Technology and Innovation of Brazil, São José dos Campos, Brazil.",
                   div(
                     style = "margin-left: 20px;",
                     tags$div(
                       "Email:", " ", a(href="mailto:pedro.camarinha@cemaden.gov.br", "pedro.camarinha@cemaden.gov.br"),  # Replace with actual email
                       tags$br(),
                       "Webpage:", " ", a(href="www.cemaden.gov.br", target="_blank", "www.cemaden.gov.br"),  # Replace with actual webpage
                       tags$br(),
                       "ORCID:", " ", a(href="https://orcid.org/0000-0002-1316-3066", target="_blank", "0000-0002-1316-3066")  # Replace with actual ORCID
                       
                     )))),
               
               tags$li(
                 div(
                   strong("Felipe Cerbella Mandarino"), " - Instituto Municipal de Urbanismo Pereira Passos - IPP, Rio de Janeiro, Brazil.",
                   div(
                     style = "margin-left: 20px;",
                     tags$div(
                       "Email:", " ", a(href="mailto:felipe.mandarino@rio.rj.gov.br", "felipe.mandarino@rio.rj.gov.br"),  # Replace with actual email
                       tags$br(),
                       "Webpage:", " ", a(href="https://www.rio.rj.gov.br/web/ipp/who-we-are", target="_blank", "https://www.rio.rj.gov.br/web/ipp/who-we-are"),  # Replace with actual webpage
                       tags$br(),
                       "ORCID:", " ", a(href="https://orcid.org/0000-0001-9576-5257", target="_blank", "0000-0001-9576-5257")  # Replace with actual ORCID
                       
                     )))),
               
               tags$li(
                 div(
                   strong("Raquel Batista Medeiros da Fonseca"), " - Fundação Geo - Rio, Prefeitura do Rio de Janeiro, Rio de Janeiro, Brazil.",
                   div(
                     style = "margin-left: 20px;",
                     tags$div(
                       "Email:", " ", a(href="mailto:raquelbmfonseca@gmail.com", "raquelbmfonseca@gmail.com"),  # Replace with actual email
                       #tags$br(),
                       #"Webpage:", " ", a(href="https://www.rio.rj.gov.br/web/ipp/who-we-are", target="_blank", "https://www.rio.rj.gov.br/web/ipp/who-we-are"),  # Replace with actual webpage
                       tags$br(),
                       #"ORCID:", " ", a(href="https://orcid.org/0000-0001-9576-5257", target="_blank", "0000-0001-9576-5257")  # Replace with actual ORCID
                       
                     )))),
               
               tags$li(
                 div(
                   strong("Guilherme Damasceno Raposo"), " - Universidade do Estado do Rio de Janeiro, Department of Cartographic Engineering, Rio de Janeiro, Brazil.",
                   div(
                     style = "margin-left: 20px;",
                     tags$div(
                       "Email:", " ", a(href="mailto:guiraposoo@gmail.com", "guiraposoo@gmail.com"),  # Replace with actual email
                       #tags$br(),
                       #"Webpage:", " ", a(href="https://www.rio.rj.gov.br/web/ipp/who-we-are", target="_blank", "https://www.rio.rj.gov.br/web/ipp/who-we-are"),  # Replace with actual webpage
                       tags$br(),
                       #"ORCID:", " ", a(href="https://orcid.org/0000-0001-9576-5257", target="_blank", "0000-0001-9576-5257")  # Replace with actual ORCID
                       
                     )))),
               
               tags$li(
                 div(
                   strong("Thomas Glade"), " - University of Vienna, Institute of Geography and Regional Research, Wien, Austria.",
                   div(
                     style = "margin-left: 20px;",
                     tags$div(
                       "Email:", " ", a(href="mailto:thomas.glade@univie.ac.at", "thomas.glade@univie.ac.at"),  # Replace with actual email
                       tags$br(),
                       "Webpage:", " ", a(href="https://homepage.univie.ac.at/thomas.glade/", target="_blank", "https://homepage.univie.ac.at/thomas.glade/"),  # Replace with actual webpage
                       tags$br(),
                       "ORCID:", " ", a(href="https://orcid.org/0000-0001-9576-5257", target="_blank", "0000-0001-9576-5257")  # Replace with actual ORCID
                       
                     )))),
               #tags$li("Thomas Glade - University of Vienna, Institute of Geography and Regional Research, Wien, Austria.")
             ),
             tags$iframe(
               src = "coolab.pdf",  # Replace with the path to your PDF file
               width = "100%", height = "600px",
               style = "border: none;"
             )
             
    ),
        
    
    tabPanel("Input data overview",
             # Content for the Input data overview tab
    ),
    
    tabPanel("A few results",
             # Content for the Input data overview tab
             div(style = "text-align: center;", img(src = "Animation_B.gif", height = "800px")),
    ),
    
    tabPanel("Cronograma & Development plan",
             # Content for the Input data overview tab
             #div(style = "text-align: center;", img(src = "SMART.jpg", height = "700px")),
    ),
    
    
    # Outreach Panel
    tabPanel("Outreach",
             #h4("Connect with Us"),
             
             # Repository 1
             h5("Repository 1"),
             p(a("Link to Repository 1", href = "https://www.example.com/repository1", target = "_blank")),
             
             # Repository 2
             h5("Repository 2"),
             p(a("Link to Repository 2", href = "https://www.example.com/repository2", target = "_blank")),
             
             # Twitter
             h5("Twitter"),
             p(a("Follow us on Twitter", href = "https://twitter.com/example_twitter", target = "_blank")),
             
             # YouTube Video
             h5("YouTube Video"),
             tags$iframe(src = "https://www.youtube.com/embed/QbziLSNcc1g", width = "560", height = "315")
    ),
    
    # Tab for Interesting Links
    tabPanel("Interesting Links",
             h4("Explore these valuable resources:"),
             
             # Link 1
             h5("Landslide Susceptibility - Data.Rio"),
             tags$ul(
               tags$li(a("This resource provides information about the existing (official) landslide susceptibility map for Rio de Janeiro.", href = "https://www.data.rio/apps/PCRJ::suscetibilidade-a-deslizamentos/about", target = "_blank"))
             ),
             
             # Link 2
             h5("Risk of Mass Movement Accidents (PDF)"),
             tags$ul(
               tags$li(a("This PDF document contains information about the risk of mass movement accidents.", href = "https://www.rio.rj.gov.br/documents/11235825/11236874/RISCO_DE_ACIDENTES_ASSOCIADOS_A_CORRIDAS_DE_MASSA.pdf", target = "_blank"))
             ),
             
             # Link 3
             h5("Favela Boundaries 2019 - Data.Rio"),
             tags$ul(
               tags$li(a("Explore data on the boundaries of favelas in Rio de Janeiro.", href = "https://www.data.rio/datasets/PCRJ::limite-favelas-2019/about", target = "_blank"))
             ),
             
             # Link 4
             h5("Land Use 2019 - Data.Rio"),
             tags$ul(
               tags$li(a("This resource provides information on land use in Rio de Janeiro for 2019.", href = "https://www.data.rio/datasets/PCRJ::uso-do-solo-2019/about", target = "_blank"))
             ),
             
             # Link 5
             h5("50 most representative landslides in Rio (PDF)"),
             tags$ul(
               tags$li(a("This PDF document contains information about the 50 largest accidents.", href = "http://www.sistema-alerta-rio.com.br/wp-content/uploads/2016/12/PDF_ESTRUTURA-DO-LIVRETO_50-MAIORES-ACIDENTES-A5.pdf", target = "_blank"))
             ),
             
             # Link 6
             h5("Alerta Rio"),
             tags$ul(
               tags$li(a("Visit the Alerta Rio website for real-time information on weather and natural disasters in Rio de Janeiro.", href = "http://alertario.rio.rj.gov.br/", target = "_blank"))
             )
    ),
    
    # Student(s) supervision",
    tabPanel("Student(s) supervision",
             # Fake student 1
             h4("Student 1"),
             p("Name: Guilherme Damasceno Raposo"),
             p("Institution: Universidade do Estado do Rio de Janeiro, Department of Cartographic Engineering."),
             p("Supervisors: Prof. Dr. Luiz Carlos Teixeira Coelho; Dr. Pedro Lima"),
             p("Working Title: TBA"),
             
             # Fake student 2
             h4("Student 2"),
             p("Name: --"),
             p("Institution: ---"),
             p("Supervisors: Prof. Dr. Luiz Carlos Teixeira Coelho; Dr. Pedro Lima"),
             p("Working Title: TBA"),
             
             # Fake student 3
             h4("Student 3"),
             p("Name: Alejandro Serrano Acevedo"),
             p("Institution: Universidad de Chile"),
             p("Supervisors: Prof. Dr. Luiz Carlos Teixeira Coelho; Dr. Pedro Lima"),
             p("Working Title: TBA")
    ),
    
    # Research Outcomes Tab
    tabPanel("Research Outcomes",
             p(""),
             
             # Detailed project description in bullet points
             tags$ul(
               tags$li(strong("Pedro Lima,"), " Moreno M, Steger S, Camarinha PI, Coelho LCT, Mandarino FC and Glade T (2023a) ",
                       strong("DEVELOPING A SPATIOTEMPORAL MODEL TO INTEGRATE LANDSLIDE SUSCEPTIBILITY AND CRITICAL RAINFALL CONDITIONS. A PRACTICAL MODEL APPLIED TO RIO DE JANEIRO MUNICIPALITY."),
                       " In: Tofani V, Casagli N, Bandecchi E, Gargini E and Armignacco D (eds.) Landslide Science for Sustainable Development. Proceedings of the 6th World Landslide Forum. Firenze, Italy: OIC S.r.l. ISBN 9791221048063. ",
                       a(href="https://wlf6.org/wp-content/uploads/2023/11/WLF6_ABSTRACT-BOOK.pdf", target="_blank", "See here"),
                tags$iframe(
                         src = "WLF_2023_RIO_PedroLima.pdf",  # Replace with the path to your PDF file
                         width = "80%", height = "500px",
                         style = "border: none;"
                       ),
               ))),
             # Further Reading
             tabPanel("Further Reading",
                      p("Further Reading"),
                      
                      tags$ul(
                        tags$li(
                          HTML('<a href="https://nhess.copernicus.org/articles/23/1483/2023/nhess-23-1483-2023-assets.html" target="_blank">Steger, S., Moreno, M., Crespi, A., Zellner, P. J., Gariano, S. L., Brunetti, M. T., Melillo, M., Peruccacci, S., Marra, F., Kohrs, R., Goetz, J., Mair, V., Pittore, M. (2023). "Deciphering seasonal effects of triggering and preparatory precipitation for improved shallow landslide prediction using generalized additive mixed models." Natural Hazards and Earth System Sciences, 23(4), 1483–1506. DOI: 10.5194/nhess-23-1483-2023.</a>')
                        ),
                        tags$li(
                          HTML('<a href="https://www.sciencedirect.com/science/article/pii/S0048969723077963?via%3Dihub" target="_blank">Moreno, M., Lombardo, L., Crespi, A., Zellner, P.J., Mair, V., Pittore, M., van Westen, C., Steger, S. (2024). "Space-time data-driven modeling of precipitation-induced shallow landslides in South Tyrol, Italy." Science of The Total Environment, Volume 912, Pages 169166. DOI: https://doi.org/10.1016/j.scitotenv.2023.169166.</a>')
                        ),
                        tags$li(
                          HTML('<a href="https://meetingorganizer.copernicus.org/EGU23/EGU23-9538.html" target="_blank">Moreno, M., Steger, S., Lombardo, L., Opitz, T., Crespi, A., Marra, F., de Vugt, L., Zieher, T., Rutzinger, M., Mair, V., Pittore, M., and van Westen, C. (2023). "Functional regression for space-time prediction of precipitation-induced shallow landslides in South Tyrol, Italy." Presented at EGU General Assembly 2023, Vienna, Austria. Abstract ID: EGU23-9538. DOI: https://doi.org/10.5194/egusphere-egu23-9538.</a>')
                       
             )))))
             


server <- function(input, output, session) {
  output$map <- renderLeaflet({
    
    # Filter the data based on the selected year
    filtered_data <- final_rioslides_wgs84 %>%
      filter(anolaudo >= input$yearSlider[1], anolaudo <= input$yearSlider[2])
    
    # Your leaflet code, but use 'filtered_data' for the heatmap and markers
    m <- leaflet(options = leafletOptions(minZoom = 10, maxZoom = 14)) %>%
      addTiles() %>%
      leafem::addMouseCoordinates() %>%
      leaflet.extras::addHeatmap(data = st_coordinates(filtered_data), max = 30, radius = 15, blur = 15) %>%
      addCircleMarkers(data = filtered_data, 
                       radius = 1,      # adjust as desired
                       color = "blue",  # adjust as desired
                       stroke = FALSE,  # to remove the border
                       fillOpacity = .6) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      #addProviderTiles(providers$Stadia.StamenToner) %>%
      addPolygons(data = study_area_wgs84, color = "black", fillOpacity = 0, weight = 3) %>%
      addPolygons(data = Limite_Favelas_2019, color = "red", fillOpacity = 0.1, weight = 1) %>%
      
      addMiniMap(position = "topleft", tiles = providers$OpenStreetMap.Mapnik, toggleDisplay = TRUE) %>%
      addScaleBar() %>%
      addLegend(position = "bottomright",  # Adjust position as desired
              colors = "red",            # Color to represent in the legend
              labels = "Favelas (# 1074)", # Legend label
              opacity = 1,                # Adjust opacity if needed
              title = ""      # Legend title
    )%>%
      
      addLegend(position = "bottomright",  # Legend for filtered_data layer
                colors = "blue",        # Color to represent in the legend
                labels = "Landslides (# 2998)",  # Legend label
                opacity = 1,            # Adjust opacity if needed
                title = "",
      )
    
    m  # Return the leaflet map
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




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

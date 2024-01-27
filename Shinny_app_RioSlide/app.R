library(sf)
library(leaflet)
library(dplyr) 
library(fontawesome)
library(shiny)
library(raster)


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

final_rioslides     <- st_read("landslides_2023_with_pred.shp")#landslides_2023.shp
study_area          <- st_read("StudyArea.shp")
Limite_Favelas_2019 <- st_read("Limite_Favelas_2019.shp")
stations            <- st_read("stations.shp")
pred                <- raster("suscetibilidade_rio.tif")

# Generate a custom color palette including a color for NA values
customPalette <- colorFactor(palette = c("red", "#008000", "yellow"), 
                             domain = c(1, 2, 3), 
                             na.color = "transparent")


# Ensure LA and MoNEW are in the same CRS
final_rioslides     <- sf::st_transform(final_rioslides, st_crs(study_area))
Limite_Favelas_2019 <- sf::st_transform(Limite_Favelas_2019, st_crs(study_area))
stations            <- sf::st_transform(stations, st_crs(study_area))


# Transform both to WGS84
study_area_wgs84      <- st_transform(study_area, 4326)
final_rioslides_wgs84 <- st_transform(final_rioslides, 4326)
Limite_Favelas_2019   <- st_transform(Limite_Favelas_2019, 4326)
stations              <- st_transform(stations, 4326)
# Removing rows with NA coordinates
coords <- st_coordinates(final_rioslides_wgs84)
final_rioslides_wgs84 <- final_rioslides_wgs84[!is.na(coords[, 'X']) & !is.na(coords[, 'Y']), ]






total_landslides <- 1660
low_percentage <- (242 / total_landslides) * 100
medium_percentage <- (878 / total_landslides) * 100
high_percentage <- (540 / total_landslides) * 100

# Update the legend text with calculated percentages
legend_text <- paste(
  "Low (Green): From the available 1660 landslides, 242 (", sprintf("%.2f", low_percentage), "%) are located within the 'low' susceptibility zones. ",
  "Medium (Yellow): 878 landslides (", sprintf("%.2f", medium_percentage), "%) are located in moderate ('medium') susceptibility zones. ",
  "High (Red): 540 landslides (", sprintf("%.2f", high_percentage), "%) are located in the most susceptible zones ('high'). ",
  "These figures represent the distribution of 1660 observed landslides across different susceptibility categories.",
  sep = ""
)

# Summary<- final_rioslides %>%
#     dplyr::group_by(ssctbl_)%>%
#     dplyr::summarise(n_slide = n())

#https://bootswatch.com/
# bslib::bootswatch_themes(5)
# [1] "cerulean"  "cosmo"     "cyborg"    "darkly"    "flatly"    "journal"   "litera"    "lumen"     "lux"       "materia"   "minty"    
# [12] "morph"     "pulse"     "quartz"    "sandstone" "simplex"   "sketchy"   "slate"     "solar"     "spacelab"  "superhero" "united"   
# [23] "vapor"     "yeti"      "zephyr"   

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "sandstone"),
  titlePanel("RioSlide - Landslide Susceptibility Map"),
  div(style = "text-align: left;", img(src = "Untitled.jpg", height = "200px")), # Update the image path as needed
  tabsetPanel(
    tabPanel("Map and Data",
             sidebarLayout(
               sidebarPanel(width = "0%", height = "0px"),
               mainPanel(
                 h4("Landslide Distribution in the study area (approx. 1,200km²)"),
                 leafletOutput("map", width = "100%", height = "600px"),
                 # absolutePanel(
                 #   id = "controls", class = "panel panel-default",
                 #   top = "800px", left = "10px",
                 #   width = 400, height = "auto",
                 #   style = "z-index: 400; background-color: rgba(255, 255, 255, 0.8); padding: 10px; border-radius: 5px;",
                 #   sliderInput("yearSlider", "Year:",
                 #               min = 2009,
                 #               max = max(final_rioslides_wgs84$anolaudo),
                 #               value = range(final_rioslides_wgs84$anolaudo),
                 #               step = 1)
                 # ),
                 p(style = "color: grey; font-size: 80%; text-align: justify;",
                 "Data Protection and Map Usage Disclaimer: This map's zoom level is controlled to balance detail with data protection. 
              Precise locations are hidden to protect privacy and comply with data protection regulations. The map utilizes the dataset on susceptibility to landslides provided by 
              the Prefeitura da Cidade do Rio de Janeiro. The original color scheme has been preserved to maintain the visual consistency and interpretive framework established by 
              the Prefeitura's study. The dataset has been optimized for rendering performance to enhance user experience, under a scientific partnership agreement and in alignment with the 
              data sharing and use policies provided by the Prefeitura. This dissemination is compliant with the Creative Commons Attribution 4.0 International License (CC BY 4.0), which 
              allows for sharing and adaptation, provided that appropriate credit is given, and any changes are indicated. The data is provided 'as is' without warranty. For detailed 
              inquiries or data concerns, please contact the appropriate authority.", 
                    a(href = "https://creativecommons.org/licenses/by/4.0/", target = "_blank", "CC BY 4.0 License"),
                    " | ",
                    a(href = "https://www.rio.rj.gov.br/web/georio/quem-somos", target = "_blank", strong("Source: GeoRIO")))
               )
             )
    ),
    
    # Map and Data Visualization Tab
    tabPanel("The existing susceptibility map",
             mainPanel(
               h4("Landslide and the Available Susceptibility Map for Rio de Janeiro"),
               leafletOutput("map2", width = "100%", height = "600px"),
               p(style = "color: grey; font-size: 80%; text-align: justify;",
                 "This map showcases the dataset on susceptibility to landslides provided by the Prefeitura da Cidade do Rio de Janeiro...",
                 a(href = "https://creativecommons.org/licenses/by/4.0/", target = "_blank", "CC BY 4.0 License"),
                 " | ",
                 a(href = "https://www.rio.rj.gov.br/web/georio/quem-somos", target = "_blank", strong("Source: GeoRIO"))
               ),
               div(style = "text-align: center;", img(src = "plot2.png", height = "500px")), # Image
               div(style = "text-align: left; padding-top: 20px;",
                   p(style = "color: grey; font-size: 80%; text-align: justify;",
                     strong("Legend for the plot:"), " ",
                     HTML(legend_text) # Insert the updated legend text
                   )
               )
             )
    ),
    
    
        
    
    tabPanel("Input data overview",
             # Content for the Input data overview tab
             div(style = "text-align: center;", img(src = "dados.png", height = "700px")),
    ),
    
    tabPanel("Conceptual Projections",
             # Content for the Input data overview tab
             div(style = "text-align: center; width: 800px; margin: auto;",  # Set the width to match the image and center the container
                 img(src = "Animation_B.gif", height = "800px"),
                 # Adding credits for the image with a hyperlink
                 p(style = "color: grey; font-size: 80%; text-align: justify;",  # Justify the text to make it evenly spread across the line
                   strong("Image credits:"), " Steger, S.(2023). This image showcases an interactive landslide susceptibility map, dynamically adjusting to reflect changes based on different rainfall scenarios. The predictions are for a study site located in Northern Italy, region of Bolzano, IT, and is derived from the analysis detailed in Steger et al. (2023). For more details, please check: ",
                   a(href = "https://nhess.copernicus.org/articles/23/1483/2023/nhess-23-1483-2023-assets.html", 
                     "https://nhess.copernicus.org/articles/23/1483/2023/nhess-23-1483-2023-assets.html", 
                     target = "_blank"), "and", 
                   a(href = "https://www.mountainresearch.at/proslide/", 
                     "https://www.mountainresearch.at/proslide/", 
                     target = "_blank")
                 )
             ),
    ),
    
    
  
    
    navbarMenu("Project Information",
               
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
                        
                        tags$hr(),
                        
                        h3(strong("SDGs: Project potential contributions to the Sustainable Development Goals (SDGs). ")),
                        
                        p(strong("Goal 4: Quality Education:"), 
                          "Enhancing public understanding and integrating landslide susceptibility knowledge into educational programs fosters preparedness and resilience. Additionally, the project is deeply invested in technology and knowlwdge transfer through the active supervision of students. By mentoring the next generation of scientists, we ensure that the state of teh art tools and methodologies developed through our research are passed on effectively."),
                        p(strong("Goal 9: Industry, Innovation, and Infrastructure:"), 
                          "The project plays a pivotal role in safeguarding infrastructure, which is a cornerstone for sustainable development. By protecting essential infrastructure from landslide damage, we conserve valuable resources that can then be channeled into other critical services such as education and healthcare. This not only fortifies the physical fabric of our communities but also supports broader societal welfare. Innovation is at the core of our approach; we maintain open repositories and actively publish in journals to ensure that our findings and technological advancements are accessible and can be built upon. This transparency and knowledge sharing are vital for continuous improvement in our field and for inspiring new, cost-effective solutions in disaster risk management and infrastructure resilience."),
                        p(strong("Goal 10: Reduce inequalities:"), 
                          "In the Rioslide project, we are acutely aware of the disproportionate impact landslides have on the most vulnerable populations, who are often situated in high-risk areas without the means to safeguard their communities. By identifying these high-susceptible  zones and implementing comprehensive susceptibility maps, we aim to inform urban development and disaster preparedness in a way that prioritizes these communities. Our initiative is not just about reducing the risk of landslides; it's about leveling the playing field. By making the entire city safer, we are actively working to reduce social and economic disparities, ensuring that safety and resilience are not privileges but basic rights for all citizens."),
                        p(strong("Goal 11: Sustainable Cities and Communities:"), 
                          "The project significantly enhances urban planning by providing detailed landslide susceptibility maps. These maps are crucial for urban planners and local authorities as they navigate the complexities of urban expansion and development strategies. By identifying areas at high risk for landslides, we enable city planners to make informed decisions about where to build and how to design communities that are both safe and sustainable. Our work ensures that urban growth is managed in harmony with the natural environment, minimizing risks to human life and property. This proactive approach to urban development supports the creation of resilient communities that can thrive for generations to come."),
                        p(strong("Goal 13: Climate Action:"), 
                          "In the face of escalating climate-related challenges, the Rioslide project is at the forefront of climate action. Our comprehensive mapping of landslide susceptibility is integral to developing robust climate change adaptation strategies. By understanding the intricate dynamics of extreme weather events and their impact on landslide patterns, we are paving the way for communities to be better prepared for the future. Our work not only contributes to immediate mitigation efforts but also enhances our collective understanding of how climate change influences geological phenomena. This knowledge is crucial for shaping policies and practices that ensure long-term resilience against the unpredictable nature of our changing climate, safeguarding communities and ecosystems alike."),
                        p(strong("Goal 15: Life on Land:"), 
                          "In the densely populated urban landscape of Rio, the Rioslide project plays a vital role in harmonizing societal and environmental well-being. By pinpointing areas prone to landslides, our project not only aims to protect human communities but also to minimize disruptions to the local ecosystems. Recognizing the interconnectedness of urban and natural environments, especially in a bustling city like Rio, we are committed to informing land management strategies that respect and preserve the urban biodiversity. This mindful approach seeks to ensure that as the city grows and develops, it does so in a way that maintains the ecological harmony essential for the flourishing of both human and natural communities."),
                        p(strong("Goal 16: Peace, Justice and strong institutions:"), 
                          "The Rioslide project is acutely aware of the socio-economic disparities that make the most vulnerable communities disproportionately affected by landslides. Our initiative goes beyond technical mapping; it seeks to address these inequalities by providing equitable solutions in disaster risk management. By enhancing the safety of all sectors within urban areas, particularly those historically underserved, we contribute to reducing inequalities. Ensuring that every community has access to the same level of protection and resources for landslide preparedness not only makes our cities safer but also fosters a more inclusive environment where everyone has the opportunity to thrive."),
                        p(strong("Goal 17: Partnerships for the Goals:"), 
                          "Fostering partnerships to unify researchers, policymakers (from the municipality to the national level), and communities in landslide susceptibility work."),
                        div(style = "text-align: center;", img(src = "SDGs.jpg", height = "500px")),
                        
                        tags$hr(),
                        #div(style = "text-align: center;", img(src = "Proj_partners.jpg", height = "100px")),
                        h3(strong("Acknowledgement:")),
                        p("While this project is currently self-funded and does not receive financial support, it is designed in alignment with the guidelines of the Brazilian National Council for Scientific and Technological Development (CNPq) and relates to the process number 234815/2014-0. The project team recognizes the vital role of CNPq in promoting scientific and technological development in Brazil and appreciates the framework it provides for research initiatives."),
                        div(style = "text-align: center;", img(src = "CNPq.png", height = "200px")),
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
                          width = "60%", height = "1000px",
                          style = "border: none;"
                        )
                        
               ),
               
    ),
    
    
    
    
    tabPanel("Cronograma & Development plan",
             # Content for the Input data overview tab
             div(style = "text-align: center;", img(src = "methods.jpg", height = "700px")),
    ),
    
    navbarMenu("Outreach and Resources",
               
               # Outreach Panel
               tabPanel("Outreach, transparency",
                        #h4("Connect with Us"),
                        
                        # Repository 1
                        h5("Repository 1"),
                        p(a("Oficial project Repository", href = "https://github.com/munizlimap15/PROSLIDE_RIO", target = "_blank")),
                        
                        # Repository 2
                        h5("Repository 2"),
                        p(a("Downloading the Rainfall data", href = "https://github.com/munizlimap15/PluvioDataRio", target = "_blank")),
                        
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
                                  # Adding a space between the text and the PDF
                                  div(style = "height: 20px;"),  # Adjust the height as needed for desired space
                                  tags$iframe(
                                    src = "WLF_2023_RIO_PedroLima.pdf#toolbar=0&navpanes=0&scrollbar=0",  # Append these parameters to the URL
                                    width = "50%", height = "1000px",
                                    style = "border: none;"
                                  ),
                          ))),
               
               
               
               
               
               
               
               # Further Reading
               tabPanel("Further Reading",
                        p("Further Reading"),
                        
                        tags$ul(
                          tags$li(
                            HTML('<a href="https://doi.org/10.1016/j.geomorph.2006.03.041" target="_blank">Coelho-Netto, Ana Luiza, André S. Avelar, Manoel C. Fernandes, and Willy A. Lacerda. "Landslide Susceptibility in a Mountainous Geoecosystem, Tijuca Massif, Rio de Janeiro: The Role of Morphometric Subdivision of the Terrain." Geomorphology 87, no. 3 (2007): 120–131. https://doi.org/10.1016/j.geomorph.2006.03.041.</a>')
                          ),
                          tags$li(
                            HTML('<a href="https://doi.org/10.3390/geosciences11100425" target="_blank">Dias, Helen Cristina, Daniel Hölbling, and Carlos Henrique Grohmann. (2021). "Landslide Susceptibility Mapping in Brazil: A Review" Geosciences 11, no. 10: 425. https://doi.org/10.3390/geosciences11100425.</a>')
                          ),
                          tags$li(
                            HTML('<a href="https://climacom.mudancasclimaticas.net.br/o-que-sao-eventos-extremos/">MARCHEZINI, Victor; CUNNINGHAM, Cristopher; DOLIF, Giovanni; CAMARINHA, Pedro Ivo; ODA, Paula; LACERDA, Renato (2023). O que são eventos extremos? Uma reflexão sobre as diferentes perspectivas do termo. ClimaCom – Desastres [online], Campinas, ano 10, nº. 25. nov. 2023.</a>')
                          ),
                          tags$li(
                            HTML('<a href="https://www.sciencedirect.com/science/article/pii/S0048969723077963?via%3Dihub" target="_blank">Moreno, M., Lombardo, L., Crespi, A., Zellner, P.J., Mair, V., Pittore, M., van Westen, C., Steger, S. (2024). "Space-time data-driven modeling of precipitation-induced shallow landslides in South Tyrol, Italy." Science of The Total Environment, Volume 912, Pages 169166. DOI: https://doi.org/10.1016/j.scitotenv.2023.169166.</a>')
                          ),
                          tags$li(
                            HTML('<a href="https://meetingorganizer.copernicus.org/EGU23/EGU23-9538.html" target="_blank">Moreno, M., Steger, S., Lombardo, L., Opitz, T., Crespi, A., Marra, F., de Vugt, L., Zieher, T., Rutzinger, M., Mair, V., Pittore, M., and van Westen, C. (2023). "Functional regression for space-time prediction of precipitation-induced shallow landslides in South Tyrol, Italy." Presented at EGU General Assembly 2023, Vienna, Austria. Abstract ID: EGU23-9538. DOI: https://doi.org/10.5194/egusphere-egu23-9538.</a>')
                          ),
                          tags$li(
                            HTML('<a href="https://nhess.copernicus.org/articles/23/1483/2023/nhess-23-1483-2023-assets.html" target="_blank">Steger, S., Moreno, M., Crespi, A., Zellner, P. J., Gariano, S. L., Brunetti, M. T., Melillo, M., Peruccacci, S., Marra, F., Kohrs, R., Goetz, J., Mair, V., Pittore, M. (2023). "Deciphering seasonal effects of triggering and preparatory precipitation for improved shallow landslide prediction using generalized additive mixed models." Natural Hazards and Earth System Sciences, 23(4), 1483–1506. DOI: 10.5194/nhess-23-1483-2023.</a>')
                          )
                        ))
               
    )
    
    
    
    
    
             ))
             
#icon.fa <- makeAwesomeIcon(icon = 'fa-tint', markerColor = 'blue', library='fa')



server <- function(input, output, session) {
  output$map2 <- renderLeaflet({
    
    
    
    # Your leaflet code, but use 'filtered_data' for the heatmap and markers
    m2 <- leaflet(options = leafletOptions(minZoom = 11, maxZoom = 13)) %>%
      addTiles() %>%
      leafem::addMouseCoordinates() %>%
      
      addCircleMarkers(data = final_rioslides_wgs84, 
                       radius = 1,      
                       color = "black",  
                       stroke = FALSE,  
                       fillOpacity = .6, group = "Landslides") %>%
      
      addRasterImage(as.factor(pred), colors = customPalette, opacity = .2, group = "Susceptibility") %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = study_area_wgs84, color = "black", fillOpacity = 0, weight = 3) %>%
      addPolygons(data = Limite_Favelas_2019, color = "red", fillOpacity = 0.1, weight = 1, group = "Favelas") %>%
      addMiniMap(position = "topleft", tiles = providers$OpenStreetMap.Mapnik, toggleDisplay = TRUE) %>%
      addScaleBar() %>%
      
      addLegend(position = "bottomright", 
                colors = "red",            
                labels = "Favelas", 
                opacity = 1,                
                title = "") %>%
      
      addControl(html = '<div style="background: rgba(255, 255, 255, 0.8); padding: 5px; border-radius: 5px; border: 1px solid #ccc;">
                            <div><span style="height:12px; width:12px; background-color:black; border-radius:50%; display:inline-block; margin-right:5px;"></span>Landslides (# 1660)</div>
                         </div>',
                 position = "bottomright") %>%
      addLayersControl(
        baseGroups = c("Base Map"),
        overlayGroups = c("Landslides", "Susceptibility", "Favelas"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    m2  # Return the leaflet map
    
  })
  ##############################################################################
  ##############################################################################
  ##############################################################################
  output$map <- renderLeaflet({
    
    # # Filter the data based on the selected year
    # filtered_data <- final_rioslides_wgs84 %>%
    #   filter(anolaudo >= input$yearSlider[1], anolaudo <= input$yearSlider[2])
    # 
    # Your leaflet code, but use 'filtered_data' for the heatmap and markers
    m <- leaflet(options = leafletOptions(minZoom = 10, maxZoom = 13)) %>%
      addTiles() %>%
      leafem::addMouseCoordinates() %>%
      leaflet.extras::addHeatmap(data = st_coordinates(final_rioslides_wgs84), 
                                 max = 40, radius = 10, blur = 10, group = "Heatmap") %>%
      addCircleMarkers(data = final_rioslides_wgs84, 
                       radius = 1,      # adjust as desired
                       color = "black",  # adjust as desired
                       stroke = FALSE,  # to remove the border
                       fillOpacity = .6, group = "Landslides") %>%
      addCircleMarkers(data = stations, 
                       radius = 6,      # adjust as desired
                       color = "blue",  # adjust as desired
                       stroke = FALSE,  # to remove the border
                       fillOpacity = .6, group = "Rain Gauges") %>%
      
      addProviderTiles(providers$CartoDB.Positron) %>%
      #addProviderTiles(providers$Stadia.StamenToner) %>%
      addPolygons(data = study_area_wgs84, color = "black", fillOpacity = 0, weight = 3, group = "Study Area") %>%
      addPolygons(data = Limite_Favelas_2019, color = "red", fillOpacity = 0.1, weight = 1, group = "Favelas") %>%
      
      addMiniMap(position = "topleft", tiles = providers$OpenStreetMap.Mapnik, toggleDisplay = TRUE) %>%
      addScaleBar() %>%
      addLegend(position = "bottomright",  # Adjust position as desired
                colors = "red",            # Color to represent in the legend
                labels = "Favelas", # Legend label
                opacity = 1,                # Adjust opacity if needed
                title = ""      # Legend title
      )%>%
      
      # Add a custom HTML legend for rain gauges
      addControl(html = '<div style="background: rgba(255, 255, 255, 0.8); padding: 5px; border-radius: 5px; border: 1px solid #ccc;">
                            <div><span style="height:12px; width:12px; background-color:blue; border-radius:50%; display:inline-block; margin-right:5px;"></span>Rain Gauges  (# 33)</div>
                         </div>',
                 position = "bottomright") %>%
      addControl(html = '<div style="background: rgba(255, 255, 255, 0.8); padding: 5px; border-radius: 5px; border: 1px solid #ccc;">
                            <div><span style="height:12px; width:12px; background-color:black; border-radius:50%; display:inline-block; margin-right:5px;"></span>Landslides (# 1660)</div>
                         </div>',
                 position = "bottomright") %>%
      addLayersControl(
        baseGroups = c("Base Map"),
        overlayGroups = c( "Heatmap", "Landslides", "Rain Gauges", "Study Area", "Favelas"),
        options = layersControlOptions(collapsed = FALSE)
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

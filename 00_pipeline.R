list.files()
# Master Pipeline Script
# This script loads and checks the input slide data. It verifies data integrity, ensures correct formatting, 
# and performs initial exploratory data analysis on the slides dataset.
source("1_checkingSlides.R")

# This script calculates susceptibility by intersecting slide data with predictor variables. 
# It prepares a dataset for further analysis, focusing on vulnerability and hazard assessment.
source("2_slidesXsusc.R")

# This script loads various predictors used in modeling landslide risk, such as rainfall, soil moisture, and elevation. 
# It processes and prepares the predictors for integration with the landslide data.
source("3_load_predictors.R")
source("3_load_predictors_GRAPH_2025.R")

# This script creates training samples for machine learning models, ensuring proper balancing of positive 
# (landslide-prone) and negative (non-prone) regions. It outputs the datasets for model training.
source("4_trainning_samples.R")

# This script uses Generalized Additive Models (GAM) to evaluate the importance of different predictor variables 
# in landslide susceptibility. It ranks the variables based on their contribution to model performance.
source("5_GAM_Variable_Importance_Evaluation.R")

# This script exports the prediction results from cross-validated models. 
# It prepares the outputs for visualization and further evaluation of model accuracy.
source("6_SCV_export_pred.R")

# This script contains the prediction code using the MEGAM algorithm. 
# It performs model predictions based on trained datasets and prepares the output for susceptibility mapping.
source("7_predCode_MEGAM.R")

# This script calculates rainfall-related predictors, such as daily rainfall, maximum hourly intensity, 
# and accumulated rainfall over different periods. It also handles missing data by filling in gaps with median values.
source("8_rain.R") #This is the very first approach applied for the ICG

# An updated version of the rainfall calculation script, including refined methods for estimating rainfall metrics 
# and ensuring more accurate predictions for input into landslide risk models.
source("8_rain_NEW.R")

# This script exports raster data related to rainfall metrics for further use in spatial analysis.
# It handles the export of processed rainfall predictors to geospatial formats like GeoTIFF.
source("8_rain_exportRaster.R")

# This script exports station data, focusing on rainfall measurement stations.
# It formats and prepares station data for further use in modeling and analysis.
source("9_exportStations.R")

# This script refines the susceptibility model by integrating Generalized Additive Models (GAM).
# It combines the slides data with predictors for final susceptibility mapping.
source("10_slidesXsuscGAM.R")

# This script allows for the selection of specific rainfall events based on user-defined thresholds.
# It filters rainfall data for targeted analyses, such as studying extreme precipitation events.
source("11_pickRain.R")

# This script calculates rainfall thresholds, determining the levels at which rainfall events become significant.
# It helps in defining critical precipitation amounts linked to landslide risks.
source("12_Rainfall_thresh.R")

# This script generates various graphs and visualizations to support landslide susceptibility analysis.
# It focuses on illustrating the relationship between rainfall and landslide occurrence.
source("13_Graphs.R")

# This script contains the code for the Shiny app, which is used to visualize and interact with landslide susceptibility data.
# It provides a user-friendly interface for exploring model results and predictors.
source("14_Shinny_app.R")

# This script creates GIF animations that show the evolution of rainfall over time.
# It visualizes dynamic changes in rainfall data across the study area.
source("15_GIF_rain.R")

# This script exports prediction results to the Shiny app for real-time visualization.
# It formats the outputs in a way that is compatible with the interactive platform.
source("16_export_pred_to_shiny.R")

# This script generates or integrates the project logo into reports or visualizations.
# It is responsible for adding branding elements to output graphics.
source("17_PROJ_LOGO.R")

# This script focuses on susceptibility analysis specific to favelas (informal settlements).
# It integrates landslide data with spatial information on favelas for targeted risk assessment.
source("18_FAVELASXsusc.R")


# This script analyzes susceptibility across the broader Rio de Janeiro region, focusing on landslide-prone areas.
# It integrates various predictors to assess regional risks.
source("18_RJXsusc.R")

# This script generates a detailed analysis of the Rocinha favela, using a slider-based approach to compare different layers.
# It focuses on visualizing landslide susceptibility and related risks in Rocinha.
source("19_Slider_rocinha.R")

# This script calculates susceptibility by intersecting slide data with predictor variables. 
# It prepares a dataset for further analysis, focusing on vulnerability and hazard assessment.
source("2_slidesXsusc.R")

# This script produces susceptibility predictions using GAM for each neighborhood (bairro).
# It visualizes and maps the predictions, providing insights into landslide risks.
source("20_pred_GAM_per_bairro.R")

# This script generates numerical predictions for each neighborhood (bairro) using a Generalized Additive Model (GAM).
# It focuses on evaluating landslide susceptibility at a neighborhood level.
source("20_pred_GAM_NUMERICAL_per_bairro.R")

# This script provides predictions of landslide susceptibility per neighborhood (bairro), without specifying the model used.
# It focuses on producing maps and numerical outputs for further interpretation.
source("20_pred_per_bairro.R")

# This script handles slope units for the study area, processing slope-related data for susceptibility analysis.
# It prepares slope data for integration into risk models.
source("21_slopeunits.R")

source("22_FIG_weather_RIO.R")

source("23_FIG_landcover_discussion.R")


source("CTRL-T\CTRL-T_code.R")



list.files()
# Master Pipeline Script

# Source individual scripts
source("1_checkingSlides.R")
source("2_slidesXsusc.R")
source("3_load_predictors.R")
source("4_trainning_samples.R")
source("5_GAM_Variable_Importance_Evaluation.R")
source("6_SCV_export_pred.R")
source("7_predCode_MEGAM.R")
source("8_rain.R") #This is the very first approach appliedfor the ICG
source("8_rain_NEW.R")
source("8_rain_exportRaster.R")
source("9_exportStations.R")
source("10_slidesXsuscGAM.R")
source("11_pickRain.R")
source("12_Rainfall_thresh.R")
source("13_Graphs.R")

source("15_GIF_rain.R")            
source("16_export_pred_to_shiny.R")            
source("17_PROJ_LOGO.R")                
source("18_FAVELASXsusc.R")  
source("18_RJXsusc.R")  
source("19_Slider_rocinha.R")              
source("20_pred_GAM_per_bairro.R")
source("20_pred_per_favela.R")
source("20_pred_per_bairro.R")     
source("21_slopeunits.R")

source("CTRL-T\CTRL-T_code.R")



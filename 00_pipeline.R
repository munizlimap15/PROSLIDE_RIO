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

source("CTRL-T\CTRL-T_code.R")fcmouse
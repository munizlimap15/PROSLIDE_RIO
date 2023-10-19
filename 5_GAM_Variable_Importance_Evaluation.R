#In the study of landslide susceptibility, we employed a Generalized Additive Model (GAM) to assess the influence of 
#various topographic and environmental predictors. The dataset, final_train, comprises a range of variables 
#including digital terrain model (DTM), plan curvature, profile curvature, relative slope position, slope, 
#topographic wetness index (TWI), geomorphological features, geological characteristics, aspect, 
#topographic position index (TPI), and land cover. These variables were subjected to a series of statistical 
#tests to evaluate their significance in predicting landslide occurrences. The model was fitted using the 
#mgcv package in R, and its performance was assessed through metrics such as AUC and AIC. 
#Variable importance was further scrutinized using permutation tests and drop-one term AIC evaluations. 
#Based on these assessments, certain variables were identified as more influential in predicting landslide susceptibility, 
#thereby aiding in the refinement of the model for more accurate and robust predictions.



# Load the required libraries
library(mgcv)
library(pROC)


library(dplyr)
library(tidyr)
library(raster)
library(RSAGA)
# Load the glmnet package
library(glmnet)

(load("C:/Users/pedro/Documents/PROslide_RIO/DATA/final_train.Rd"))
summary(final_train)
summary(as.factor(final_train$geol))
summary(as.factor(final_train$geomorph))
summary(as.factor(final_train$aspect))
summary(as.factor(final_train$tpi))

my.trafo = function(x) 
{
  x$geomorph = as.factor(x$geomorph)
  x$geol = as.factor(x$geol)
  x$aspect = as.factor(x$aspect_class)
  x$tpi = as.factor(x$tpi_class)
  x$landcover = as.factor(x$landcover19_reclass)
  return(x)
}
final_train = my.trafo(final_train)
final_train$aspect_class = NULL
final_train$tpi_class    = NULL
final_train$landcover19_reclass  = NULL

summary(final_train)



# Assuming final_train is your data frame and slide is your response variable
# Make sure to replace 'final_train' and 'slide' with your actual data frame and response variable names

fo1 <- slide ~ s(dtm_5m) + s(plan_curv) + s(prof_curv) + s(rel_slp_pos) + s(slope) + s(twi) + # Numerical
  landcover + geomorph + geol + aspect + tpi                                                  # Categorical
 
# Fit the GAM model
gam_fit <- gam(fo1, data = final_train, family = binomial)

# Summary statistics for variable importance
summary(gam_fit)

# Permutation Test for Variable Importance
original_auc <- auc(final_train$slide, predict(gam_fit, type = "response"))
predictor_names <- names(final_train)[!(names(final_train) %in% "slide")]
delta_auc <- numeric(length(predictor_names))

for (i in predictor_names) {
  temp_data <- final_train
  temp_data[[i]] <- sample(temp_data[[i]])
  temp_fit <- gam(fo1, data = temp_data, family = binomial)
  temp_auc <- auc(temp_data$slide, predict(temp_fit, type = "response"))
  delta_auc[which(predictor_names == i)] <- original_auc - temp_auc
}

# Print the change in AUC for each predictor
print(data.frame(Variable = predictor_names, Delta_AUC = delta_auc))
# dtm_5m: 0.0074       - Relatively important
# plan_curv: 0.00006   - Not very important
# prof_curv: 0.0017    - Somewhat important
# rel_slp_pos: 0.00024 - Not very important
# slope: 0.0029        - Somewhat important
# twi: 0.00069         - Not very important
# geomorph: 0.0021     - Somewhat important
# geol: 0.0103         - Relatively important
# aspect: -0.00017     - Not very important (negative value suggests it might even be detrimental)
# tpi: 0.0045          - Somewhat important
# landcover: 0.0293    - Most important among the variables
#Based on these Delta_AUC values, you might consider excluding variables like plan_curv, rel_slp_pos, twi, and aspect from your model, as they have low Delta_AUC values,
# indicating that they are not very important for predicting the outcome.



# Drop-One Term for Variable Importance
aic_values <- numeric(length(predictor_names))
for (i in predictor_names) {
  reduced_formula <- as.formula(paste("slide ~ ", paste(predictor_names[predictor_names != i], collapse = " + ")))
  reduced_model <- gam(reduced_formula, data = final_train, family = binomial)
  aic_values[which(predictor_names == i)] <- AIC(reduced_model)
}

# Print the AIC for each reduced model
print(data.frame(Variable = predictor_names, AIC = aic_values))
# dtm_5m: 2760.111      - Relatively high AIC
# plan_curv: 2654.858   - Lower AIC, better fit
# prof_curv: 2656.134   - Lower AIC, better fit
# rel_slp_pos: 2655.357 - Lower AIC, better fit
# slope: 2726.723       - Relatively high AIC
# twi: 2655.249         - Lower AIC, better fit
# geomorph: 2729.553    - Relatively high AIC
# geol: 2823.542        - Highest AIC among the variables
# aspect: 2643.770      - Lowest AIC, best fit among the variables
# tpi: 2756.386         - Relatively high AIC
# landcover: 3067.160   - Highest AIC, worst fit among the variables

#Based on these AIC values, you might consider focusing on variables like plan_curv, prof_curv, rel_slp_pos, and aspect, 
#as they have the lowest AIC values, indicating a better fit to the data. 
#On the other hand, variables like geol and landcover have the highest AIC values, suggesting they might not be good predictors in the model.

#Create a data frame to hold the variable names and their Delta_AUC and AIC values
variable_importance_df <- data.frame(
  Variable = c("dtm_5m", "plan_curv", "prof_curv", "rel_slp_pos", "slope", "twi", "geomorph", "geol", "aspect", "tpi", "landcover"),
  Delta_AUC = c(0.0078, 0.00007, 0.0023, 0.00024, 0.0040, 0.00066, 0.0019, 0.0115, -0.00034, 0.0046, 0.0285),
  AIC = c(2760.111, 2654.858, 2656.134, 2655.357, 2726.723, 2655.249, 2729.553, 2823.542, 2643.770, 2756.386, 3067.160)
)

# Create a bar chart for Delta_AUC
ggplot(variable_importance_df, aes(x = reorder(Variable, -Delta_AUC), y = Delta_AUC)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  ggtitle("Variable Importance Based on Delta_AUC") +
  xlab("Variables") +
  ylab("Delta_AUC")

# Create a bar chart for AIC
ggplot(variable_importance_df, aes(x = reorder(Variable, AIC), y = AIC)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  ggtitle("Variable Importance Based on AIC") +
  xlab("Variables") +
  ylab("AIC")







##################################################################
#######
####### LASSO
####### LASSO
####### LASSO
#######
##################################################################


















# Save the original data frame
original_final_train <- final_train

# One-hot encode the categorical variables
one_hot_matrix <- model.matrix(~ . - 1, data = original_final_train[, c("aspect", "tpi", "landcover", "geomorph", "geol")])

# Convert the one-hot encoded matrix to a data frame
one_hot_df <- as.data.frame(one_hot_matrix)

# Combine the one-hot encoded variables with the original data frame, excluding the original categorical variables
final_train <- cbind(original_final_train[, !(names(original_final_train) %in% c("aspect", "tpi", "landcover", "geomorph", "geol"))], one_hot_df)

# Prepare the data
X <- as.matrix(final_train[, !(names(final_train) %in% c("slide", "ID"))])  #  # Predictor variables Excluding 'ID' as well
y <- final_train$slide  # Response variable

# Fit the LASSO model
lasso_model <- glmnet(X, y, alpha = 1, family = "binomial")

# Cross-validation for LASSO
cv.lasso <- cv.glmnet(X, y, alpha = 1, family = "binomial")

# Optimal lambda value
lambda_optimal <- cv.lasso$lambda.min

# Coefficients at optimal lambda
coef(cv.lasso, s = lambda_optimal)
# 52 x 1 sparse Matrix of class "dgCMatrix"
# s1
# (Intercept) -1.034150926
# dtm_5m      -0.006417254
# plan_curv    .          
# prof_curv   -4.791098590
# rel_slp_pos  .          
# slope        0.046629082
# twi         -0.012020133
# aspect1     -0.009340082
# aspect2      .          
# aspect3      .          
# aspect4      .          
# aspect5      .          
# aspect6      0.007408463
# aspect7     -0.040233857
# aspect8      0.003910651
# tpi2        -0.059280007
# tpi3        -1.260352337
# tpi4        -0.684453090
# tpi5        -0.115726305
# tpi6         .          
# landcover2   .          
# landcover3  -0.523641110
# landcover4  -0.875187564
# landcover5   2.597846424
# landcover6   1.748405266
# landcover7   .          
# landcover8   0.674685215
# geomorph2    0.642173765
# geomorph3    0.218051953
# geomorph4    0.458608155
# geomorph5   -0.700027067
# geomorph6    0.087776173
# geomorph7   -0.394537928
# geomorph8    .          
# geomorph9    0.699079381
# geomorph255 -0.358368254
# geol2        0.329200774
# geol3        0.438754187
# geol4       -0.243528638
# geol5       -0.700445724
# geol6        0.338173083
# geol7       -0.185241157
# geol8       -1.684506401
# geol9       -0.477171815
# geol10       0.843756817
# geol11      -0.311292214
# geol12       1.156555549
# geol13       1.286444234
# geol14       1.819660818
# geol15      -0.838922705
# geol16      -1.978064725
# geol255      .        

# Here's a summary of variables and levels that have been shrunk to zero:
# 
# plan_curv
# rel_slp_pos
# Several levels of geomorph: geomorph3, geomorph6, geomorph8, geomorph255
# Several levels of geol: geol255
# Several levels of aspect: aspect2, aspect3, aspect4, aspect5, aspect6, aspect8
# Several levels of tpi: tpi6
# Several levels of landcover: landcover2, landcover7
# For categorical variables like geomorph, geol, aspect, tpi, and landcover, you should be cautious when interpreting these results. A zero coefficient for a level doesn't necessarily mean you should remove that level from the variable; it simply means that, relative to the reference level, those levels are not providing additional information.
# 
# For continuous variables like plan_curv and rel_slp_pos, a zero coefficient suggests that these variables may not be contributing to the model and could be candidates for exclusion.

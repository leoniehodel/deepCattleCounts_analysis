#' ---
#' Title: "Regression analysis with stocking rate as regressand"
#' Author: "Leonie"
#' Date: 2024_02_17
#' ---
library(tidyverse)
library(car)
library(curl)
library(texreg)
library(vtable)

################################
#'## Read in data
################################

#curl_download(destfile='data/lhodel_stocking_rates_regression_data.xlsx' ,url = "https://zenodo.org/records/10674947/files/lhodel_stocking_rates_regression_data.xlsx?download=1")
dsdrop<- readxl::read_excel("data/lhodel_stocking_rates_regression_data.xlsx")
dsdrop$SIGLA_UF <- factor(dsdrop$SIGLA_UF, levels = c("PA","RO", "AC" ,'AM'))

################################
#'##  model construction
################################
# chose dependent variable
dependent_variable <- "stocking_rate" # 'stocking rate' or 'log(stocking_rate)'
# chose control variables 
control_variables <- c(
                      "SIGLA_UF - 1" , 
                       "area",
                       "mean_temp_18_19",
                       "precipitation_2018_19",
                       "nearest_fedSh",
                       "buffer_forest_tot_2013",
                       'population')

robust_model <- FALSE  # Set to TRUE for robust model (lm_robust()), FALSE for regular model (lm())
standardize <- TRUE # Set to TRUE if you want to standardize the data

# make a sub dataset for the models III, IV, and V
dsdrop2<- dsdrop%>%filter(SIGLA_UF %in% c("PA","RO") )
dsdrop2$SIGLA_UF<- factor(dsdrop2$SIGLA_UF, levels = c("PA","RO"))

dsdrop2$IBGE_CODE<-as.character(dsdrop2$IBGE_CODE)
dsdrop2$zdc_bin<- ifelse(dsdrop2$mean_mun_msg4_1317 > 0.5,1,0)

# summary stats for all 

st(dsdrop, file='results/summarystats_dsdrop.csv')
st(dsdrop2, file= 'results/summarystats_dsdrop2.csv')

standardize_dataset <- function(dataset, variables) {
  standardized_dataset <- dataset

  for (variable in variables) {
    if (variable %in% colnames(dataset)) {
      variable_data <- dataset[[variable]]
      variable_mean <- mean(variable_data)
      variable_sd <- sd(variable_data)

      standardized_variable <- (variable_data - variable_mean) / variable_sd

      standardized_dataset[[variable]] <- standardized_variable
    }
  }

  return(standardized_dataset)
}
if(standardize){
dsdrop<- standardize_dataset(dsdrop, c(control_variables,dependent_variable,"defo_abs_13_17" ,"mod_deg_sum_201819","sev_deg_sum_201819","crop_201819_tot",
                 "agricultural_credits",  "zdc_exposure", 'mean_mun_msg4_1317','defo_buffer_abs_13_17'))

dsdrop2<- standardize_dataset(dsdrop2, c(control_variables,dependent_variable,"defo_abs_13_17" ,"mod_deg_sum_201819","sev_deg_sum_201819","crop_201819_tot",
                "agricultural_credits",  "zdc_exposure", 'mean_mun_msg4_1317','defo_buffer_abs_13_17'))

}

#model 1 shows the most important property level variables
model1_formula <- as.formula(paste(dependent_variable, "~", paste(c(control_variables,
                "defo_abs_13_17" ,"mod_deg_sum_201819","sev_deg_sum_201819","crop_201819_tot"),
                            collapse = "+")))

#model 2 shows agricultural credits
model2_formula <- as.formula(paste(dependent_variable, "~", paste(c(control_variables,
              'defo_buffer_abs_13_17',"mod_deg_sum_201819","sev_deg_sum_201819","crop_201819_tot"),
                                                                  collapse = "+")))

model3_formula <- as.formula(paste(dependent_variable, "~", paste(c(control_variables,
               'defo_buffer_abs_13_17', 'defo_abs_13_17',"mod_deg_sum_201819","sev_deg_sum_201819","crop_201819_tot", "agricultural_credits"),collapse = "+")))



model4_formula <- as.formula(paste(dependent_variable, "~", paste(c(control_variables,
               'defo_buffer_abs_13_17', "defo_abs_13_17" ,"mod_deg_sum_201819","sev_deg_sum_201819","crop_201819_tot",  "mean_mun_msg4_1317"),
                                                                 collapse = "+")))
model5_formula <- as.formula(paste(dependent_variable, "~", paste(c(control_variables,
               'defo_buffer_abs_13_17', "defo_abs_13_17" ,"mod_deg_sum_201819","sev_deg_sum_201819","crop_201819_tot",  "agricultural_credits*zdc_bin"),collapse = "+")))



# Linear models
models <- list()
formulas <- list(model1_formula, model2_formula,model3_formula, model4_formula, model5_formula)
datasets <- list(dsdrop, dsdrop, dsdrop2,dsdrop2, dsdrop2)

for (i in 1:length(formulas)) {
  if(robust_model){
    model <-  lm_robust(formula = formulas[[i]], data = datasets[[i]])
  }else{
    model <-  lm(formula = formulas[[i]], data = datasets[[i]])
    #if (standardize) {
    #  model <- arm::standardize(model)
    #}
  }
  models[[i]] <- model
}

################################
#'## print regression models
################################

htmlreg(file = "Results/reg_model.html",
        list(
            models[[1]],
             models[[2]],
             models[[3]],
             models[[4]],
             models[[5]]
             
        ),
        caption="Linear regression models on stocking rate estimates",
        #dcolumn=FALSE,
        digits = 3,
        "scipen"=-30,
        leading.zero = FALSE,
         # custom.coef.names = c('Intercept','&delta;=2  (Acre)','&delta;=3  (Para)',"&delta;=4  (Amazonas)",
         #                       "Area<sub>a</sub>",
         #                       "Temperature<sub>t</sub>","Precipitation<sub>t</sub>",
         #                       "DistSlaughterhouse<sub></sub>",
         #                       "ForestAvailability<sub>t-6</sub>",
         #                       'Deforestation<sub>t-2-t-6</sub>','ModDegradation<sub>t</sub>',
         #                       'SevereDegradation<sub>t</sub>','Crops<sub>t</sub>',
         #                       'ABC credits<sub>t-2-t-6</sub>', 'ZDC<sub>t-2-t-6</sub>', 'ZDC<sub>t-2-t-6(binary)</sub>',
         #                        'ZDC<sub>t-2-t-6(binary)</sub> x ABC credits,<sub>13-17</sub>'
         #                       
         #                       ),
        custom.model.names = c("I<sub>&delta;</sub>",'II<sub>&delta;</sub>', 'III<sub>&delta;</sub>', "IV<sub>&delta;</sub>", 'V<sub>&delta;</sub>'),
        loat.pos = "h", return.string = TRUE, single.row = TRUE,  booktabs = TRUE)


# save the regression models

saveRDS(models, file="results/regression_models.RData")
saveRDS(dsdrop2,"results/dsdrop2.RData")

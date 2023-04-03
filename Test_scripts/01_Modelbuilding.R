####READ ME####
#The purpose of this script is to test linear and non linear regression models to predict
#drying in the Rio Grande

####Libraries ####
library(tidyverse)
library(DHARMa)
library(lme4) # for creating mixed models
library(car) # for Anova(), vif()
library(MuMIn) # for AICc
library(emmeans) # for emmeans, emtrends, all the post hoc tests and plotting

####load data frames list####
alldf <- readRDS("Data/df_list.RData")

#### GLM ####
fit_glm <- lapply(alldf, function(df) {
  glm_all <- glm(Sum_days_rm_dry ~ discharge_sum, 
                 family = poisson(link = "log"), data = df)
})

####extract error####
#extracting coefficients for one 
summary <- summary(fit_glm[["08313000_102"]])$coefficients
#extracting error for one 
error <- summary(fit_glm[["08313000_102"]])$coefficients[, 2]

#extracting coefficients for all data frames
glmsummary <- lapply(fit_glm, function(df) { #create the function
  summary(df)$coefficients
})
#extracting error for all data frames
glmserror <- lapply(fit_glm, function(df) { #create the function
  summary(df)$coefficients[, 2]
})



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

alldfzx <- read.csv("Data/df_list.csv", header = TRUE)

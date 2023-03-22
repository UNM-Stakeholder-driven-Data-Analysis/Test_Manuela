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

####Linear models####

# Filter data for a specific site
site_no <- "08313000" #Otowi gauge
RM <- "248" #RM for Otowi 257???
site_year_data <- discharge_sum %>%
  filter(site_no == site_no, RM == RM)

# create the linear model
m1 <- lm(Sum_days_rm_dry ~ discharge_sum, data = site_year_data)
summary(m1)
# check assumptions
plot(m1)

# Filter data for a specific site and year
site_no <- "08313150"
RM <- "254"
site_year_data <- discharge_sum %>%
  filter(site_no == site_no, RM == RM)

# create the linear model
m1 <- lm(Sum_days_rm_dry ~ discharge_sum, data = site_year_data)
summary(m1)
# check assumptions
plot(m1)

####GLM####
# Filter data for a specific site and year
site_no <- "08313150"
RM <- "254"
site_year_data <- merged_data %>%
  filter(site_no == site_no, RM == RM)
# build model
glm_full <- glm(Sum_days_rm_dry ~ discharge_sum, 
                family = "poisson", 
                data = site_year_data)

#satisfies the assumptions
simulateResiduals(glm_full, plot = T)

#run anova
car::Anova(glm_full, type = "III")

# Filter data for a specific site and year
site_no <- "08313150"
RM <- "254"

site_year_data <- merged_data %>%
  filter(site_no == site_no & RM == RM)

tmod_lme4_L <- glmer(Sum_days_rm_dry ~ factor(discharge_sum) + (1|year),
                     family = poisson(link = "log"), data = site_year_data)

site <- "08313150"
rivermile <- "254"

site_year_data <- merged_data %>%
  filter(site_no == site & RM == rivermile)

tmod_lme4_L <- glmer(Sum_days_rm_dry ~ discharge_sum  + (1|year),
                     family = poisson(link = "log"), data= site_year_data)
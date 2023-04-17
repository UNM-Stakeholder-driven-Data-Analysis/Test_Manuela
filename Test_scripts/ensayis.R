####ensayis#####
#### Poisson ####
poisson_united <- glm(Sum_days_rm_dry ~ discharge_sum, 
               family = poisson(link = "log"), data = all.united)
#### Negative Binomial ####
nb_united <- glm.nb(Sum_days_rm_dry ~ discharge_sum, data = df)
  
#### Zero-inflated Poisson ####
zip_united <- glmmTMB(Sum_days_rm_dry ~ discharge_sum, 
                   data = df, ziformula = ~1, family = "poisson")

#### Zero-inflated negative binomial ####
zinb_united <- glmmTMB(Sum_days_rm_dry ~ discharge_sum, 
                     data = df, ziformula = ~1, family = nbinom2)
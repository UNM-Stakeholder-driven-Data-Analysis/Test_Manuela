####READ ME####
#The purpose of this script is to run a zero-inflated negative binomial model 
# to test models for convergence and compare with the other models ran.
# test the model using DHARMa residual simulations and discard the models that don't have a good fit

####Libraries ####
library(tidyverse)
library(DHARMa) #simulations
library(purrr)
library(glmmTMB) #zero inflated

####load data frames list and distance matrix####
#model data frame list 
alldf <- readRDS("Data/df_list.RData")
#### Zero-inflated negative binomial ####
fit_zinb <- lapply(alldf, function(df) {
  glm_all <- glmmTMB(Sum_days_rm_dry ~ discharge_sum, 
                     data = df, ziformula = ~1, family = nbinom2)
})
summary_fit_zinb <- summary(fit_zinb)
#You can also check directly whether the Hessian (curvature) of the model is OK by examining the pdHess ("positive-definite Hessian") component of the sdr ("standard deviation report") component of the model:
pdHess_model <- lapply(fit_zinb, function(df){
  pdHess <- df$sdr$pdHess
})
#OR
fit_zinb <- readRDS("Data/zinb_models.RData")
#In general models with non-positive definite Hessian matrices should be excluded from further consideration
#A TRUE value indicates that the Hessian matrix of the corresponding model is positive-definite, while a FALSE value indicates that the Hessian matrix is not positive-definite.
#In the context of model fitting, having a positive-definite Hessian matrix is generally desirable because it ensures that the model has a unique global minimum and that optimization algorithms can converge to that minimum efficiently. On the other hand, a non-positive-definite Hessian matrix implies that the model may have multiple minima or saddle points, which can make it difficult or impossible to find the true minimum of the objective function.
#keep the TRUE 

#combine the model output and true/false value into a single list element
fit_zinb_tf <- function(fit_zinb, tf_value) {
  list(output = fit_zinb, tf = tf_value)
}
# Use Map() function to apply the fit_zinb_tf function to each pair of list elements
zinb_tf <- Map(fit_zinb_tf, fit_zinb, pdHess_model)

#save models
saveRDS(fit_zinb, file="zinb.models.RData")

####testing 10 random models####
#simulate and plot residuals for one model
#KS the higher the better, closer to 1?
#deviation should not be significant
#Zero inflation and dispersion if p-value small (less than ex 0.05) 
#we reject the null hypothesis and residuals are zero inflated or overdispersed 
#1
simulationOutput1 <- simulateResiduals(fittedModel = fit_zinb[["08330000_100"]])
plot(simulationOutput1, main = "Simulated Residuals 1")
testZeroInflation(simulationOutput1)
testDispersion(simulationOutput1) #Overdispersion describes the observation that variation is higher than would be expected.
testTemporalAutocorrelation(simulationOutput1, alldf[["08330000_100"]][["year"]],
                            alternative = c("two.sided"), plot = T)
pdHess <- fit_zinb[["08330000_100"]]$sdr$pdHess
ks1 <- testUniformity(simulationOutput1)

#2
simulationOutput2 <- simulateResiduals(fittedModel = fit_zinb[["08317400_133"]])
plot(simulationOutput2, main = "Simulated Residuals 2")
testZeroInflation(simulationOutput2)
testDispersion(simulationOutput2)
testTemporalAutocorrelation(simulationOutput2, alldf[["08317400_133"]][["year"]],
                            alternative = c("two.sided"), plot = T)

#3
simulationOutput3 <- simulateResiduals(fittedModel = fit_zinb[["08331160_91"]])
plot(simulationOutput3, main = "Simulated Residuals 3")
testZeroInflation(simulationOutput3)
testDispersion(simulationOutput3)
testTemporalAutocorrelation(simulationOutput3, alldf[["08331160_91"]][["year"]],
                            alternative = c("two.sided"), plot = T)
#4
simulationOutput4 <- simulateResiduals(fittedModel = fit_zinb[["08330875_80"]])
plot(simulationOutput4,  main = "Simulated Residuals 4")
testZeroInflation(simulationOutput4)
testDispersion(simulationOutput4)
testTemporalAutocorrelation(simulationOutput4, alldf[["08330875_80"]][["year"]],
                            alternative = c("two.sided"), plot = T)

#5
simulationOutput5 <- simulateResiduals(fittedModel = fit_zinb[["08317400_103"]])
plot(simulationOutput5, main = "Simulated Residuals 5")
testZeroInflation(simulationOutput5)
testDispersion(simulationOutput5)
testTemporalAutocorrelation(simulationOutput5, alldf[["08317400_103"]][["year"]],
                            alternative = c("two.sided"), plot = T)
#6
simulationOutput6 <- simulateResiduals(fittedModel = fit_zinb[["08329928_57"]])
plot(simulationOutput6, main = "Simulated Residuals 6")
testZeroInflation(simulationOutput6)
testDispersion(simulationOutput6)
testTemporalAutocorrelation(simulationOutput6, alldf[["08329928_57"]][["year"]],
                            alternative = c("two.sided"), plot = T)
#7
simulationOutput7 <- simulateResiduals(fittedModel = fit_zinb[["08313000_70"]])
plot(simulationOutput, main = "Simulated Residuals 7")
testZeroInflation(simulationOutput7)
testDispersion(simulationOutput7)
testTemporalAutocorrelation(simulationOutput7, alldf[["08313000_70"]][["year"]],
                            alternative = c("two.sided"), plot = T)
#8
simulationOutput8 <- simulateResiduals(fittedModel = fit_zinb[["08313000_164"]])
testUniformity(simulationOutput8, alternative = c("two.sided", "less", "greater"))
plot(simulationOutput8, main = "Simulated Residuals 8")
testZeroInflation(simulationOutput8)
dispersion8 <- testDispersion(simulationOutput8)
#getting p-value for dispersion test
pvalue <- dispersion8[["p.value"]]
testTemporalAutocorrelation(simulationOutput8, alldf[["08313000_164"]][["year"]],
                            alternative = c("two.sided"), plot = T)
#9
simulationOutput9 <- simulateResiduals(fittedModel = fit_zinb[["08331160_66"]])
plot(simulationOutput9, main = "Simulated Residuals 9")
testZeroInflation(simulationOutput9)
testDispersion(simulationOutput9)
testTemporalAutocorrelation(simulationOutput9, alldf[["08331160_66"]][["year"]],
                            alternative = c("two.sided"), plot = T)
#10
simulationOutput10 <- simulateResiduals(fittedModel = fit_zinb[["08319000_123"]])
plot(simulationOutput10, main = "Simulated Residuals 10")
testZeroInflation(simulationOutput10)
testDispersion(simulationOutput10)
testTemporalAutocorrelation(simulationOutput10, alldf[["08319000_123"]][["year"]],
                            alternative = c("two.sided"), plot = T)

####remove non convergent models (false)tf#### 
#I am doing this after testing the 10 random models in case I need to go back and check them
# Use the Filter() function to keep only the elements where tf is TRUE
zinb_true <- Filter(function(x) x$tf, zinb_tf)

#remove tf to run DHARMa
# Use map() from purrr to extract the first element from each list
zinb <- map(zinb_true, 1)

####test all remaining (true) models####
simulationOutput <- lapply(zinb, function(df) {
  simulateResiduals(fittedModel = df)
})
simulationOutput_zi <- lapply(simulationOutput, function(df) {
  testZeroInflation(df) 
})
simulationOutput_dispersion <- lapply(simulationOutput, function(df) {
  testDispersion(df) 
})

simulationOutput_590 <- lapply(fit_zinb, function(df) {
  simulateResiduals(fittedModel = df)
})
#still need to get this to work
#year <- seq(from = 1, to = nrow(alldf))
#simulationOutput_temp <- lapply(simulationOutput_590, function(df) {
 # testTemporalAutocorrelation(df, year, alternative = "two.sided")
#})
#run ks test again independently (from the simulation output)
ks <- lapply(simulationOutput, function(df) {
  testUniformity(df)
})

####filtering by KS####
#extract KS for remaining models
pvalue_ks<- lapply(ks, function(df) {
  df[["p.value"]]
})

#testing. Retain p-values over 0.05 for ks
pvalule_0.05_ks <- subset(pvalue_ks, pvalue_ks >=0.05)
#because there are a few models that have pvalues under 0.05, I'll remove them

#combine the model output and ks value into a single list element
funct_zinb_ks <- function(zinb, pvalue_ks) {
  list(output = zinb, ks = pvalue_ks)
}
#use Map() function to apply the fit_zinb_tf function to each pair of list elements
zinb_ks <- Map(funct_zinb_ks, zinb, pvalue_ks)

#remove all the models that have a ks under using the Filter() function
zinb_5 <- Filter(function(x) x$ks>=0.05, zinb_ks)

####p-values for dispersion test####
#extract p-values
pvalue_dispersion<- lapply(simulationOutput_dispersion, function(df) {
 df[["p.value"]]
})
#dispersion
#testing. Retain p-values over 0.05 for dispersion
pvalue_0.05_disp <- subset(pvalue_dispersion, pvalue_dispersion >=0.05)
#there are no dispersion tests that have a p-value under 0.05.

#save remaining models after cleaning 
saveRDS(zinb, file="Data/zinb_models.RData")

#remove non converged models or assign them a really high error value.
#for example assign a bad error to the bad models and include them later. 
#extract KS for remaining models
#extract error

#McFadden's pseudo R2 
#i want a higher value of it
#try the written down formula to make sure that the function from the package 
#handles the model correctly because the package is made for a logistic
#regression. the package says that it can handle glm model
#but it specifies a logistic regression. the zero inflated ones can be 
#logistic regression but because I am using a zero inflated model, I should verify that 
#it works before trusting it.

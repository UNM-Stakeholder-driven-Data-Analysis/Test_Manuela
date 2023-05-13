####READ ME####
#The purpose of this script is to test the poisson regression model 
#then... CHECK!!! the bigger the distance, the higher the error?

####libraries####
library(DHARMa) #read: https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html

####load####
#data frames list and distance matrix
#df <- read...?
#models
  fit_poisson <- readRDS("models.RData")

####plotting data just to look at it####
hist(df$deviance)
hist(df$distance)
plot(deviance ~ distance, data = df)

hist(merged2$deviance)
hist(merged2$distance)
plot(deviance ~ distance, data = merged2)
plot(distance ~ deviance, data = merged2)
is.na(merged2$distance)

####testing####
#pull 10 models out randomly and check for over dispersion
#test for temporal autocorrelation
#pull out autocorrelation residuals and apply acf function to them 

#pull 10 random models from my fit_glm model list
random_glm <- sample(fit_poisson, 10)

#for those 10 models, extract and plot residuals
for (i in 1:length(random_glm)) {
  # compute residuals
  residuals <- residuals(random_glm[[i]])
  
  # plot residuals vs. fitted values
  plot(random_glm[[i]]$fitted.values, residuals, 
       xlab = "Fitted Values", ylab = "Residuals",
       main = paste0("Residuals Plot for Model ", i))
}

#simulate and plot residuals for one model
#KS the higher the better, closer to 1?
#deviation should not be significant
#Zero inflation and dispersion if p-value small (less than ex 0.05) 
#we reject the null hypothesis and residuals are zero inflated or overdispersed 
#1
simulationOutput <- simulateResiduals(fittedModel = fit_poisson[["08330000_100"]])
plot(simulationOutput, main = "Simulated Residuals 1")
testZeroInflation(simulationOutput)
testDispersion(simulationOutput) #Overdispersion describes the observation that variation is higher than would be expected.
#2
simulationOutput <- simulateResiduals(fittedModel = fit_poisson[["08317400_133"]])
plot(simulationOutput, main = "Simulated Residuals 2")
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
#3
simulationOutput <- simulateResiduals(fittedModel = fit_poisson[["08331160_91"]])
plot(simulationOutput, main = "Simulated Residuals 3")
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
#4
simulationOutput <- simulateResiduals(fittedModel = fit_poisson[["08330875_80"]])
plot(simulationOutput,  main = "Simulated Residuals 4")
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
#5
simulationOutput <- simulateResiduals(fittedModel = fit_poisson[["08317400_103"]])
plot(simulationOutput, main = "Simulated Residuals 5")
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
#6
simulationOutput <- simulateResiduals(fittedModel = fit_poisson[["08329928_57"]])
plot(simulationOutput, main = "Simulated Residuals 6")
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
#7
simulationOutput <- simulateResiduals(fittedModel = fit_poisson[["08313000_70"]])
plot(simulationOutput, main = "Simulated Residuals 7")
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
#8
simulationOutput <- simulateResiduals(fittedModel = fit_poisson[["08313000_164"]])
plot(simulationOutput, main = "Simulated Residuals 8")
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
#9
simulationOutput <- simulateResiduals(fittedModel = fit_poisson[["08331160_66"]])
plot(simulationOutput, main = "Simulated Residuals 9")
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
#10
simulationOutput <- simulateResiduals(fittedModel = fit_poisson[["08319000_123"]])
plot(simulationOutput, main = "Simulated Residuals 10")
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)

####this is to do the process automated####
#for those 10 models, simulate and plot residuals
for (i in 1:length(random_glm)) {
  # compute residuals
  poisson_simulation <- simulateResiduals(fittedModel = random_glm[[i]])
  
  # extract residuals from simulated data
  sim_residuals <- poisson_simulation$residuals
  
  # plot residuals vs. fitted values
  plot(poisson_simulation, 
       xlab = "Fitted Values", ylab = "Residuals",
       main = paste0("Simulated Residuals Plot for Model ", i))
}

#for those 10 models, test dispersion
for (i in 1:length(random_glm)) {
  # compute deviance residuals
  dispersion <- testDispersion(random_glm[[i]])
}

#test overdispersion for simulation
for (i in 1:length(poisson_simulation)) {
  # compute deviance residuals
  dispersion <- testDispersion(poisson_simulation[[i]])
}
####The reason for simulating residuals is to create a set of data that is similar to the 
#observed data, but with known characteristics (i.e., normally distributed residuals with mean 0 
#and variance equal to the estimated dispersion parameter). This simulated data can then be used to assess 
#the adequacy of the model fit by comparing the simulated residuals to the observed residuals.

#for those 10 models, test zero inflation
for (i in 1:length(random_glm)) {
  # compute deviance residuals
  zeroinf <- testZeroInflation(random_glm[[i]])
}

#test zero inflation for simulation
for (i in 1:length(poisson_simulation)) {
  # compute deviance residuals
  zeroinf <- testZeroInflation(poisson_simulation[[i]])
}

####READ ME####
#The purpose of this script is to run a regression model to test our hypothesis
#the bigger the distance, the higher the error?

####libraries####
library(DHARMa) #read: https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html

####load####
#data frames list and distance matrix
df <- rea
#models
fit_glm <- readRDS("models.RData")

####testing####
#plotting data just to look at it
hist(df$deviance)
hist(df$distance)
plot(deviance ~ distance, data = df)

#pull 10 models out randomly and check for over dispersion
#test for temporal autocorrelation
#pull out autocorrelation residuals and apply acf function to them 

#pull 10 random models from my fit_glm model list
random_glm <- sample(fit_glm, 10)

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
simulationOutput <- simulateResiduals(fittedModel = fit_glm[["08330000_100"]])
plot(simulationOutput)
testZeroInflation(simulationOutput)
testDispersion(simulationOutput) #Overdispersion describes the observation that variation is higher than would be expected.

simulationOutput <- simulateResiduals(fittedModel = fit_glm[["08317400_133"]])
plot(simulationOutput)
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)

simulationOutput <- simulateResiduals(fittedModel = fit_glm[["08331160_91"]])
plot(simulationOutput)
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)

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


testDispersion(poisson_simulation)

#for those 10 models, extract and plot deviance
for (i in 1:length(random_glm)) {
  # compute deviance residuals
  dev_residuals <- residuals(random_glm[[i]], type = "deviance")
  
  # plot deviance residuals vs. fitted values
  plot(random_glm[[i]]$fitted.values, dev_residuals, 
       xlab = "Fitted Values", ylab = "Deviance Residuals",
       main = paste0("Dispersion Plot for Model ", i))
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
testZeroInflation(poisson_halland_simulation)

####READ ME####
#The purpose of this script is to run a Poisson model to obtain error values
#then make a table with the gauge, RM, distance between RM and gauge and error of each GLM for that pair 

####Libraries ####
library(tidyverse)
library(DHARMa)
library(emmeans) # for emmeans, emtrends, all the post hoc tests and plotting
library(data.table)
library(reshape2)
library(MASS)

####load data frames list and distance matrix####
#model data frame list 
alldf <- readRDS("Data/df_list.RData")

#distance matrix
dist_matri <- read.table("Data/distance_matrix.csv", sep = ",", header = TRUE)
colnames(dist_matri)[1] <- "gauge"  
#remove X from front of column names
colnames(dist_matri) <- gsub("X", "", colnames(dist_matri))
dist_matri$gauge <- paste0("0", dist_matri$gauge)
#### Negative Binomial ####
fit_nb <- lapply(alldf, function(df) {
  glm_all <- glm.nb(Sum_days_rm_dry ~ discharge_sum, data = df)
})

#save models
saveRDS(fit_nb, file="nb.models.RData")

####testing 10 random models####
#simulate and plot residuals for one model
#KS the higher the better, closer to 1?
#deviation should not be significant
#Zero inflation and dispersion if p-value small (less than ex 0.05) 
#we reject the null hypothesis and residuals are zero inflated or overdispersed 
#1
simulationOutput <- simulateResiduals(fittedModel = fit_nb[["08330000_100"]])
plot(simulationOutput, main = "Simulated Residuals 1")
testZeroInflation(simulationOutput)
testDispersion(simulationOutput) #Overdispersion describes the observation that variation is higher than would be expected.
#2
simulationOutput <- simulateResiduals(fittedModel = fit_nb[["08317400_133"]])
plot(simulationOutput, main = "Simulated Residuals 2")
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
#3
simulationOutput <- simulateResiduals(fittedModel = fit_nb[["08331160_91"]])
plot(simulationOutput, main = "Simulated Residuals 3")
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
#4
simulationOutput <- simulateResiduals(fittedModel = fit_nb[["08330875_80"]])
plot(simulationOutput,  main = "Simulated Residuals 4")
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
#5
simulationOutput <- simulateResiduals(fittedModel = fit_nb[["08317400_103"]])
plot(simulationOutput, main = "Simulated Residuals 5")
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
#6
simulationOutput <- simulateResiduals(fittedModel = fit_nb[["08329928_57"]])
plot(simulationOutput, main = "Simulated Residuals 6")
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
#7
simulationOutput <- simulateResiduals(fittedModel = fit_nb[["08313000_70"]])
plot(simulationOutput, main = "Simulated Residuals 7")
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
#8
simulationOutput <- simulateResiduals(fittedModel = fit_nb[["08313000_164"]])
plot(simulationOutput, main = "Simulated Residuals 8")
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
#9
simulationOutput <- simulateResiduals(fittedModel = fit_nb[["08331160_66"]])
plot(simulationOutput, main = "Simulated Residuals 9")
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
#10
simulationOutput <- simulateResiduals(fittedModel = fit_nb[["08319000_123"]])
plot(simulationOutput, main = "Simulated Residuals 10")
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)


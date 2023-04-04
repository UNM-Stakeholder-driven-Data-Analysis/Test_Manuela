####READ ME####
#The purpose of this script is to run a regression model to predict
#drying in the Rio Grande

####Libraries ####
library(tidyverse)
library(DHARMa)
library(lme4) # for creating mixed models
library(car) # for Anova(), vif()
library(MuMIn) # for AICc
library(emmeans) # for emmeans, emtrends, all the post hoc tests and plotting
library(data.table)
library(reshape2)

####load data frames list and distance matrix####
alldf <- readRDS("Data/df_list.RData")
dist_matrix <- read.csv("Data/distance_matrix.csv")
colnames(dist_matrix) <- gsub("X", "", colnames(dist_matrix))

#### GLM ####
fit_glm <- lapply(alldf, function(df) {
  glm_all <- glm(Sum_days_rm_dry ~ discharge_sum, 
                 family = poisson(link = "log"), data = df)
})
#extract deviance#
deviance <- lapply(fit_glm,function(df) { 
  summary(df)[["deviance"]]
})
##Look at deviance for goodness of fit test. deviance can be like R2 for poisson.
#For Poisson models you could use the deviance; which is akin to a MSE but better suited to a Poisson.
#The deviance is a measure of how well the model fits the data – if the model fits well, the observed values  will be close to their predicted means , causing both of the terms in  to be small, and so the deviance to be small.

####make final table####
#convert deviance list into a data frame
deviance_df <- data.frame(id = names(deviance), value = unlist(deviance))

#separate the identifier column into two columns
df <- separate(deviance_df, id, into = c("gauge", "RM"), sep = "_")

#add distance matrix values to data frame
#convert row names to character
rownames(dist_matrix) <- as.character(rownames(dist_matrix))
df_matrix <- melt(dist_matrix, value.name[1])
df_matrix <- cbind(df_matrix, colsplit(df_matrix$variable, "_", names = c("gauge", "RM")))
df_matrix <- spread(df_matrix, RM, value)
?melt
#get the row names of the distance matrix as a new column in the data frame
df$gauge <- rownames(dist_matrix)[match(df$gauge, rownames(dist_matrix))]

# Merge the two data frames using "gauge" and "RM" columns as keys
merged_df <- merge(df, dist_matrix, by = c("gauge", "RM"))
?melt

####extract error, not the one that I need####
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

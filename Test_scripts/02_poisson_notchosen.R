####READ ME####
#The purpose of this script is to run a Poisson model to obtain error values
#then make a table with the gauge, RM, distance between RM and gauge and error of each GLM for that pair 

####Libraries ####
library(tidyverse)
library(DHARMa)
library(MASS)
library(emmeans) # for emmeans, emtrends, all the post hoc tests and plotting
library(data.table)
library(reshape2)

####load data frames list and distance matrix####
#model data frame list 
alldf <- readRDS("Data/df_list.RData")
#distance matrix
dist_matri <- read.table("Data/distance_matrix.csv", sep = ",", header = TRUE)
colnames(dist_matri)[1] <- "gauge"  
#remove X from front of column names
colnames(dist_matri) <- gsub("X", "", colnames(dist_matri))
dist_matri$gauge <- paste0("0", dist_matri$gauge)
#### Poisson ####
fit_poisson <- lapply(alldf, function(df) {
  glm_all <- glm(Sum_days_rm_dry ~ discharge_sum, 
                 family = poisson(link = "log"), data = df)
})

#save models
#saveRDS(fit_poisson, file="models.RData")

#extract deviance
deviance <- lapply(fit_poisson,function(df) { 
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
df$gauge_RM <- paste(df$gauge, df$RM, sep = "_")

#create a new data frame from the distance matrix
dist <- gather(dist_matri, key = "RM", value = "distance", -gauge)
#create a new column with RM and gauge to use for merge
dist$gauge_RM <- paste(dist$gauge, dist$RM, sep = "_")

#merge the two data frames
# Merge data frames based on a common column
merged <- merge(df, dist, by = "gauge_RM", all.x = TRUE)
#fill in missing values in the new data frame merged with values from the distance data frame
merged$distance <- ifelse(is.na(merged$distance), dist$distance, merged$distance)

#remove duplicate gauge and RM columns
merged2 <- merged[,-5:-6]
#rename some solumns
colnames(merged2)[2] ="gauge"
colnames(merged2)[3] ="RM"
colnames(merged2)[4] ="deviance"

#save final table
write.csv(merged2, "Data/final_table.csv", row.names=TRUE)

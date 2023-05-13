####load data####
#models
fit_zinb <- readRDS("zinb.models.RData")
#distance matrix
dist_matri <- read.table("Data/distance_matrix.csv", sep = ",", header = TRUE)
colnames(dist_matri)[1] <- "gauge"  
#remove X from front of column names
colnames(dist_matri) <- gsub("X", "", colnames(dist_matri))
dist_matri$gauge <- paste0("0", dist_matri$gauge)

####extract deviance####
deviance <- lapply(fit_zinb,function(df) { 
  summary(df)[["deviance"]]
})

deviance <- lapply(fit_zinb, function(model) {
  summary_model <- summary(model)
  return(summary_model$deviance)
})
summary_model <- summary(fit_zinb[["08330000_100"]])
converged <- summary_model$converged
#convert deviance list into a data frame
deviance_df <- data.frame(id = names(deviance), value = unlist(deviance))

####make final table####
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
#rename some columns
colnames(merged2)[2] ="gauge"
colnames(merged2)[3] ="RM"
colnames(merged2)[4] ="deviance"
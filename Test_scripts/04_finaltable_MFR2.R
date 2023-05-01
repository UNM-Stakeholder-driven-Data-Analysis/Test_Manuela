####READ ME####
#The purpose of this script is to make the final table that includes
# the McFadden's pseudo R2 and the distance between RM and gage

####libraries####
library(tidyr) #separate function

####calling distance and error####
#distance matrix
dist_matri <- read.table("Data/distance_matrix.csv", sep = ",", header = TRUE)
colnames(dist_matri)[1] <- "gauge"  
#remove X from front of column names
colnames(dist_matri) <- gsub("X", "", colnames(dist_matri))
dist_matri$gauge <- paste0("0", dist_matri$gauge)

#McFadden's pseudo R2
loglik_list <- readRDS("Data/loglik_list.RData")
#convert R2 list into a data frame
MFR2_df <- data.frame(id = names(loglik_list), value = unlist(loglik_list))

####make final table####
#separate the identifier column into two columns
df <- separate(MFR2_df, id, into = c("gauge", "RM"), sep = "_")
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
colnames(merged2)[4] ="MFR2"

#write csv for final table
write.csv(merged2,  "Data/final_df.csv", row.names = FALSE)

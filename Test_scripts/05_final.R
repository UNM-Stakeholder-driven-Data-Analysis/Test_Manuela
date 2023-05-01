####READ ME####
#The purpose of this script is to 

####calling final table####
final_df <- read.table("Data/final_df.csv", sep = ",", header = TRUE)

####plotting data just to look at it####
hist(merged2$MFR2)
hist(merged2$distance)
plot(distance ~ MFR2, data = merged2)
plot(MFR2 ~ distance, data = merged2)

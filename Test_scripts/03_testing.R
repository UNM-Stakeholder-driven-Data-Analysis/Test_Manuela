####READ ME####
#The purpose of this script is to run a regression model to test our hypothesis
#the bigger the distance, the higher the error?

####load data frames list and distance matrix####
df <- read.table("Data/final_table.csv", sep = ",", header = TRUE, row.names=1)

####testing####
#plotting data just to look at it
hist(df$deviance)
hist(df$distance)
plot(deviance ~ distance, data = df)

#pull 10 models out randomly and check for over dispersion
#test for temporal autocorrelation
#pull out autocorrelation residuals and apply acf function to them 

#### read me ####

# The purpose of this script is to explore my River Eyes data, including
# - describing dataset size (# variables & # observations)
# - describing data types
# - checking data distributions
# - checking for spatial autocorrelation
# - checking for temporal autocorrelation
# - checking for correlation between variables

#### libraries ####
library(dataRetrieval) #USGS data pagacke
library(tidyverse)
library(lubridate)
library(psych) # to plot pair-wise correlations
library(car) # I like their qq plot fxn
library(tsibble) # useful for creating time series objects
library(forecast) # I like their Acf fxn
library(ape) # for spatial autocorrelation
library(ade4)# for spatial autocorrelation
library(rgdal) # for mapping

#### load and tidy River Eyes data  ####

Reyes <- read.csv("Data/DailyOccurrenceDryRm.csv", header = TRUE)
latlong <- read.csv("Data/WholeRiverMiles_LatLong.csv", header = TRUE)

# format date/time
# as.POSIXct is for date + time + time zone
# as.Date is for date only
#?strptime: Date-time Conversion Functions to and from Character

Reyes$Date_RE = as.POSIXct(Reyes$Date, format="%Y-%m-%d")

# convert characters that should be factors (categories) to factors
# and int that should be numbers (the year and month)
Reyes$Condition = as.factor(Reyes$Condition)
Reyes$RM = as.factor(Reyes$RM)
Reyes$Year = as.numeric(Reyes$Year)
Reyes$Month = as.numeric(Reyes$Month)

#are there any missing values?
sum(is.na(Reyes$Condition)) #no

# plot all RM by number of days dry 
options(scipen = 999)

ggplot(data = Reyes, aes(x = RM, y = Year, fill = Condition))+
  geom_col()+
  ylab("Number of days") + xlab("Year") + ggtitle("Drying at individual river miles between 2003 and 2018")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none")


# can also look at single years in detail
ggplot(data=Reyes, aes(x=RM, y= Year, fill = Condition))+
  geom_line() + geom_path()+
  facet_wrap(~Year, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()

#### temporal autocorrelation ####

#### spatial autocorrelation ####


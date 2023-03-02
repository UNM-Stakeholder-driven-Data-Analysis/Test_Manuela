#### READ ME####
#The purpose of this script is to gather the data from both USGS gage system 
#and the River Eyes dataset and explore both of them by.
# - describing dataset size (# variables & # observations)
# - describing data types
# - checking data distributions
# - checking for spatial autocorrelation
# - checking for temporal autocorrelation
# - checking for correlation between variables

install.packages("tidyverse")
#### packages ####
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

# intro to package 
vignette("dataRetrieval", package = "dataRetrieval")

#### data info ####

## service is either uv = 15 min data or dv = day data
# gages of interest (add a cero at the begining of each one!)

# 8313000	RIO GRANDE AT OTOWI BRIDGE, NM
# 8313150	Rio Grande abv Buckman Diversion, nr White Rock,NM
# 8317400	RIO GRANDE BELOW COCHITI DAM, NM
# 8319000	RIO GRANDE AT SAN FELIPE, NM
# 8329918	RIO GRANDE AT ALAMEDA BRIDGE AT ALAMEDA, NM
# 8329928	RIO GRANDE NR ALAMEDA, NM
# 8330000	RIO GRANDE AT ALBUQUERQUE, NM
# 8330830	RIO GRANDE AT VALLE DE ORO, NM
# 8330875	RIO GRANDE AT ISLETA LAKES NR ISLETA, NM
# 8331160	RIO GRANDE NEAR BOSQUE FARMS, NM
# 8331510	RIO GRANDE AT STATE HWY 346 NEAR BOSQUE, NM
# 8332010	RIO GRANDE FLOODWAY NEAR BERNARDO, NM
# 8354900	RIO GRANDE FLOODWAY AT SAN ACACIA, NM

#### retrieving data USGS ####

# retrieve data for one site
otowi <- readNWISdata(siteNumbers = "08313000",
                      parameterCd = "00060",
                      startDate = "2002-04-01",
                      endDate = "2022-04-01",
                      service="dv")

#retrieve data for all dates on all 8 gages
siteNumber <- c("08313000", "08313150", "08317400", "08319000", "08329918", "08329928", 
                "08330000", "08330830", "08330875", "08331160", "08331510", "08332010", 
                "08354900")
#this code retrieves the discharge data
pCode <- "00060"
#start and end date from 2002 to 2022
sdate <- "2002-04-01"
edate <- "2022-10-31"

#get all the data from 2002 to 2022
fulldischarge <- readNWISdv(siteNumbers = siteNumber,
                            parameterCd = pCode,
                            startDate = sdate,
                            endDate = edate
                            )

# add year and day of year for plotting
fulldischarge$year = lubridate::year(fulldischarge$Date)
fulldischarge$day = lubridate::yday(fulldischarge$Date)

#get something... check this again
with(fulldischarge, table(site_no, X_00060_00003))
range(with(fulldischarge, table(site_no, X_00060_00003)))

#separate data set in sites for plotting
dis_otowi <- 
  fulldischarge %>% 
  filter(site_no=="08319000") %>%
  arrange(Date)

# add year and day of year for plotting Just did that for whole dataset
dis_otowi$year = lubridate::year(dis_otowi$Date)
dis_otowi$day = lubridate::yday(dis_otowi$Date)

# plot discharge by year 
ggplot(data=dis_otowi, aes(x=day, y=X_00060_00003))+
  geom_point() + geom_path()+
  facet_wrap(~year, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()

# can also look at single years in detail
ggplot(data=dis_otowi, aes(x=Date, y=X_00060_00003))+
  geom_point() + geom_path()+
  xlim(c(as.Date("2021-01-01"), as.Date("2021-12-31")))+
  theme(legend.title = element_blank()) +
  theme_bw()

####retrieving data USGS for the summer only####
#use paste function and for loop to get all the dates from only the summer
#start date
start.date1 = c(2002:2022)
start.date <- vector(mode="character", length=length(start.date1))
for (i in 1:length(start.date1)){
  start.date[i] = paste(start.date1[i], "04","01", sep="-")
}
start.date
start.date = as.Date(start.date)
#do I need it as a date?

#end date
end.date1 = c(2002:2022)
end.date <- vector("character", length(end.date1))
for (i in seq_along(end.date1)){
  end.date[i] = paste(end.date1[i], "10","31", sep="-")
}
end.date

#get all the data from 2002 to 2022
discharge <- readNWISdv(siteNumbers = siteNumber,
                        parameterCd = pCode,
                        startDate = start.date,
                        endDate = end.date
)

# data from Rio Grande? fix
#data_nm <- readNWISdata(huc="13020211", parameterCd="00060", service="uv")


####check distributions####
#Otowi
temp = fulldischarge[fulldischarge$site_no == "08313000",]
qqPlot(temp$X_00060_00003); shapiro.test(temp$X_00060_00003[0:5000]) # not normal
#White Rock
temp = fulldischarge[fulldischarge$site_no == "08313150",]
qqPlot(temp$X_00060_00003); shapiro.test(temp$X_00060_00003[0:5000]) # not normal
#Cochiti
temp = fulldischarge[fulldischarge$site_no == "08317400",]
qqPlot(temp$X_00060_00003); shapiro.test(temp$X_00060_00003[0:5000]) # not normal

#Otowi
temp = fulldischarge[fulldischarge$site_no == "08313000",]
summary(temp$X_00060_00003)
hist(temp$X_00060_00003)
plot(density(temp$X_00060_00003))
#White Rock
temp = fulldischarge[fulldischarge$site_no == "08313150",]
summary(temp$X_00060_00003)
hist(temp$X_00060_00003)
plot(density(temp$X_00060_00003))
#Cochiti
temp = fulldischarge[fulldischarge$site_no == "08317400",]
summary(temp$X_00060_00003)
hist(temp$X_00060_00003)
plot(density(temp$X_00060_00003))

####temporal autocorrelation####

#### retrieving data River Eyes ####
REyes <- read.csv("Data/DailyOccurrenceDryRm.csv", header = TRUE)





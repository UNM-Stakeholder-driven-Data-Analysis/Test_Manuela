####READ ME####
#The purpose of this script is to gather the data from the USGS gage system 
# FOR THE SUMMER ONLY - APRIL 1st TO OCTOBER 31st FOR EVERY YEAR from 2003 - 2018 
# and sum it!!!

####libraries####
library(dataRetrieval) #USGS data pagacke
library(tidyverse)
library(lubridate)
library(DHARMa) #testing glm assumptions
library(lme4)
library(dplyr)


####data info ####

## service is either uv = 15 min data or dv = day data
#default setting for day data is mean.
# gages of interest (add a cero at the begining of each one!)

# 8313000	RIO GRANDE AT OTOWI BRIDGE, NM
# 8313150	Rio Grande abv Buckman Diversion, nr White Rock,NM ##deleted for lack of data
# 8317400	RIO GRANDE BELOW COCHITI DAM, NM
# 8319000	RIO GRANDE AT SAN FELIPE, NM
# 8329918	RIO GRANDE AT ALAMEDA BRIDGE AT ALAMEDA, NM
# 8329928	RIO GRANDE NR ALAMEDA, NM
# 8330000	RIO GRANDE AT ALBUQUERQUE, NM
# 8330830	RIO GRANDE AT VALLE DE ORO, NM ##deleted for lack of data
# 8330875	RIO GRANDE AT ISLETA LAKES NR ISLETA, NM
# 08331160	RIO GRANDE NEAR BOSQUE FARMS, NM
# 08331510	RIO GRANDE AT STATE HWY 346 NEAR BOSQUE, NM
# 08332010	RIO GRANDE FLOODWAY NEAR BERNARDO, NM
# 08354900	RIO GRANDE FLOODWAY AT SAN ACACIA, NM

####retrieving data USGS for the summer only####

#use paste function and for loop to get all the dates from only the summer
#from 2003 to 208, because of the River Eyes Data 

#start date
start.date1 = c(2003:2018)
start.date <- vector(mode="character", length=length(start.date1))
for (i in 1:length(start.date1)){
  start.date[i] = paste(start.date1[i], "04","01", sep="-")
}
start.date
start.date = as.Date(start.date)
#do I need it as a date?

#end date
end.date1 = c(2003:2018)
end.date <- vector("character", length(end.date1))
for (i in seq_along(end.date1)){
  end.date[i] = paste(end.date1[i], "10","31", sep="-")
}
end.date
end.date = as.Date(end.date)

#retrieve data for all dates on all 8 gages
siteNumber <- c("08313000", "08317400", "08319000", "08329918", "08329928", 
                "08330000", "08330830", "08330875", "08331160", "08331510", 
                "08354900")
#this code retrieves the discharge data
pCode <- "00060"

# Initialize an empty data frame to hold the results
discharge <- data.frame()

# Loop through each date range and retrieve the corresponding data
for (i in seq_along(start.date)) {
  # Retrieve data for the current date range
  temp <- readNWISdv(siteNumbers = siteNumber,
                     parameterCd = pCode,
                     startDate = start.date[i],
                     endDate = end.date[i])
  
  # Append the data to the results data frame
  discharge <- rbind(discharge, temp)
}

# View the resulting data frame
discharge

####load and tidy River Eyes data  ####

Reyes <- read.csv("Data/DailyOccurrenceDryRm.csv", header = TRUE)
latlong_o <- read.csv("Data/WholeRiverMiles_LatLong.csv", header = TRUE)
Annual_dry_rm <- read.csv("Data/Persistence.txt")


####summing discharge data by year####
# add year column from date 
discharge$year = lubridate::year(discharge$Date)

#add by year and by site number
discharge_sum <- discharge %>%
  group_by(site_no, year) %>%
  summarise(discharge_sum = sum(X_00060_00003))

####merging USGS sum discharge and River Eyes data frames####
Annual_dry_rm$year <- Annual_dry_rm$Year
merged_data <- merge(discharge_sum, Annual_dry_rm, by = "year")
merged_data$RM <- as.character(merged_data$RM)


# plot sum discharge by gauge 
ggplot(data=discharge_sum, aes(x=year, y=discharge_sum))+
  geom_point() + geom_path()+
  facet_wrap(~site_no, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()

# plot all gauges together
ggplot(data=discharge_sum, aes(x=year, y=discharge_sum, color=site_no))+
  geom_point() + geom_path()+
  theme(legend.title = element_blank()) +
  theme_bw()

####trouble shooting missing values####
# retrieve data for White Rock and Bernardo
#White Rock's data starts in 2016
whiterock <- readNWISdata(siteNumbers = "08313150",
                      parameterCd = "00060",
                      startDate = "2002-04-01",
                      endDate = "2018-10-31",
                      service="dv")
#Bernardo's data stops in 2005 and starts in 2011
bernardo <- readNWISdata(siteNumbers = "08332010",
                          parameterCd = "00060",
                          startDate = "2002-04-01",
                          endDate = "2018-10-31",
                          service="dv")

####linear models####

# Filter data for a specific site
site <- "08313000" #Otowi gauge
RivMile <- "248" #RM for Otowi 257???
site_year_data <- discharge_sum %>%
  filter(site_no == site, RM == RivMile)

# create the linear model
m1 <- lm(Sum_days_rm_dry ~ discharge_sum, data = site_year_data)
summary(m1)
# check assumptions
plot(m1)

# Filter data for a specific site and year
site_no <- "08313150"
RM <- "54"
site_year_data <- discharge_sum %>%
  filter(site_no == site_no, RM == RM)

# create the linear model
m1 <- lm(Sum_days_rm_dry ~ discharge_sum, data = site_year_data)
summary(m1)
# check assumptions
plot(m1)

####GLM for only one gauge and one river mile####
# Filter data for a site with 12 zeros out of 14 values and 
site <- "08313000" #otowi
rivermile <- "167"

site_year_data <- merged_data %>%
  filter(site_no == site & RM == rivermile)
#run poisson model for that site
mod_poiss <- glm(Sum_days_rm_dry ~ discharge_sum,
                   family = poisson(link = "log"), data= site_year_data)

summary(mod_poiss)
plot(mod_poiss)


# Filter data for a site with 1 zero out of 14 values and 
site <- "08313000" #otowi
rivermile <- "75"

site_year_data <- merged_data %>%
  filter(site_no == site & RM == rivermile)
#run poisson model for that site
mod_poiss <- glm(Sum_days_rm_dry ~ discharge_sum,
                   family = poisson(link = "log"), data= site_year_data)

summary(mod_poiss)
plot(mod_poiss)

# Filter data for a site with 12 zeros out of 14 values and 
site <- "08330875" #isleta
rivermile <- "167"

site_year_data <- merged_data %>%
  filter(site_no == site & RM == rivermile)
#run poisson model for that site
mod_poiss <- glm(Sum_days_rm_dry ~ discharge_sum,
                 family = poisson(link = "log"), data= site_year_data)

summary(mod_poiss)
plot(mod_poiss)

# Filter data for a site with 1 zero out of 14 values and 
site <- "08330875" #isleta
rivermile <- "75"

site_year_data <- merged_data %>%
  filter(site_no == site & RM == rivermile)
#run poisson model for that site
mod_poiss <- glm(Sum_days_rm_dry ~ discharge_sum,
                 family = poisson(link = "log"), data= site_year_data)

summary(mod_poiss)
plot(mod_poiss)


#retrieve gauges that are upstream of specific river miles
upstream = read.csv("Data/upstream.csv")

# Remove duplicates by single column
upstream2 <- upstream[!duplicated(upstream$RMNum), ]

#change RMNum to RM
upstream2$RM<- upstream2$RMNum
#merge upstream and data data set
d <- merge(merged_data, upstream2, by = "RM")

####create list of data frames ####
# extract unique values of variables "site_no" and "RM"
site_noz <- unique(d$site_no) 
RMz <- unique(d$RM)
# create a data frame called "dfz" using the "expand.grid" function which generates a sequence of all combinations of the variables.
dfz = expand.grid(site_noz, RMz)
names(dfz) = c("site_no", "RM")
# add a new column called "df_ID" to "dfz" which is a combination of "site_no" and "RM" variables separated by an underscore.
dfz$df_ID = paste(dfz$site_no, dfz$RM, sep="_")
# an empty list called "alldfz" is created, which will be used to store individual data frames.
alldfz = list()
#loop over each unique value in "df_ID" column of "dfz" and filters "merged_data" for the corresponding values of "site_no" and "RM" to create individual data frames.
#these data frames are stored as elements of the "alldfz" list with their corresponding names.
for(i in dfz$df_ID){
  RMi = dfz$RM[dfz$df_ID==i]
  site_noi = dfz$site_no[dfz$df_ID==i]
  alldfz[[i]] = d %>%
    filter(site_no == site_noi, RM == RMi)
  for(i in dfz){
    pattern <- trimws(as.character(df_ID$site_no[1]))
    dfzx <- i[, c(1:9, grep(pattern, names(i)))]
  }
}

#hoow to delete all north/south columns minus the one for a specific data frame
pattern <- trimws(as.character(x08330000_100$site_no[1]))
x0833000_100x <- x08330000_100[, c(1:9, grep(pattern, names(x08330000_100)))]

#remove river miles with less than 3 points of data (non-zero)
# define  column to check
col <- "Sum_days_rm_dry"
# subset the list to remove data frames with less than 3 non-zero values
alldfz_cleaned <- lapply(alldfz, function(df) { #create the function
  sum_nonzero <- sum(!is.na(df[, col]) & df[, col]!= 0) #sum the number of zeros in the column and catch any na's
  if (sum_nonzero >= 3) { #if there are 2 or less zeros, apply function
    return(df)
  }
})
#to remove any NULL elements that may have been created by the lapply 
alldfz_cleaned <- alldfz_cleaned[!sapply(alldfz_cleaned, is.null)]

##verify that it worked
# create a list to store the counts
zero_counts <- list()
# iterate over each data frame in the list
for (i in seq_along(alldfz_cleaned)) {
  # create a vector to store the counts for this data frame
  alldfz_counts <- numeric(ncol(alldfz_cleaned[[i]]))
  
  # iterate over each column in the data frame
  for (j in seq_along(alldfz_cleaned[[i]])) {
    # count the number of zeros in the column
    alldfz_counts[j] <- sum(alldfz_cleaned[[i]][, j] == 0, na.rm = TRUE)
  }
  
  # add the counts for this data frame to the list
  zero_counts[[i]] <- alldfz_counts
}

# print the list of zero counts
print(zero_counts)

#remove river upstream of gauges

# subset the list to remove data frames of RM upstream of gauges
alldfz_downs <- lapply(alldfz_cleaned, function(df) { #create the function
  norths <- df[, cols][grep("north", df[, cols])]
  if (nrow(norths) > 0){
    return(NULL)  # return NULL instead of the original data frame
  } else {
    return(df)
  }
})

#to remove any NULL elements that may have been created by the lapply 
alldfz_downs <- alldfz_downs[!sapply(alldfz_downs, is.null)]

#### GLM ####
fit_glm <- lapply(alldfz_cleaned, function(df) {
  glm_poi <- glm(Sum_days_rm_dry ~ discharge_sum, 
      family = poisson(link = "log"), data = df)
})
 
summary(fit_glm)
plot(fit_glm)






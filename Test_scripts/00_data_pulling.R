#### READ ME####
#The purpose of this script is to gather the data from both USGS gage system 
#and the River Eyes dataset and explore both of them by.
# - describing dataset size (# variables & # observations)
# - describing data types
# - checking data distributions
# - checking for spatial autocorrelation
# - checking for temporal autocorrelation
# - checking for correlation between variables

#### packages ####
library(dataRetrieval)
# intro to package 
vignette("dataRetrieval", package = "dataRetrieval")

#### retrieving data ####

### USGS ###
## service is either uv = 15 min data or dv = day data

# data from Rio Grande? fix
data_nm <- readNWISdata(huc="13020211", parameterCd="00060", service="uv")
head(data_nm)

length(unique(data_nm$site_no))

# gages of interest

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

# retrieve data for one site
otowi <- readNWISdata(siteNumbers = "08313000",
                    parameterCd = "00060",
                    startDate = "2002-04-01",
                    endDate = "2022-10-31",
                    service="dv")

#retrieve data for all sites using a for loop
siteNumber <- c("08319000", "08329918", "08329928", "08330000", "08330830", "08330875", "08331160", "08331510", 
                "08332010", "08354900")
pCode <- "00060"
start.date <- "2002-04-01"
end.date <- "2022-10-31"

discharge <- readNWISdv(siteNumber, pCode, start.date, end.date)


### River Eyes ###
REyes <- read.csv("Data/DailyOccurrenceDryRm.csv", header = TRUE)
x

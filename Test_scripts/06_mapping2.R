####READ ME####
#The purpose of this script is to create a map showing the river gages and the
#river miles segmentalong the Rio Grande

####libraries####
library(dataRetrieval) #USGS data pagacke
library(ggmap) # making maps
library(osmdata)
library(sf) 
library(tidyverse) 
library(ggplot2)
library(sp) # helpful for maps 


####this is my key####
#add AIzaS to start of key
api_secret <- "yA2hxrVEEzF45kbWfDphd_gvZTMn0503lw"
register_google(key = api_secret)

#NM map
nm_map <- get_map(getbb('NM'), source="stamen",
                  maptype="terrain-background")
ggmap(nm_map)

#map using API functions
NM_satellite <- get_map(location = "New Mexico", maptype="terrain", source="stamen", api_key = api_secret)
ggmap(NM_satellite)
#to zoom in or out of map
NM_map <- get_map(location = "New Mexico", zoom = 7, maptype = "terrain", source = "stamen", api_key = api_secret)
ggmap(NM_map)

####add points to a map####
#obtain information for USGS sites
siteInfo <- readNWISsite(c("08313000", "08317400", "08319000", "08329918", "08329928", 
                           "08330000", "08330875", "08331160", "08331510", "08332010", "08354900"))

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

#fix gauge names for later
site_no <- c("08313000", "08317400", "08319000", "08329918", "08329928", 
             "08330000", "08330875", "08331160", "08331510", "08332010", "08354900")
name <- c("OTOWI", "COCHITI DAM", "SAN FELIPE", "ALAMEDA BRIDGE", "ALAMEDA",
          "ALBUQUERQUE", "ISLETA", "BOSQUE FARMS",
          "STATE HWY 346 NEAR BOSQUE", "BERNARDO", "SAN ACACIA")
num <- c(1:11)

#make data frame
gauge_name <- cbind(site_no, name, num)

#merge gauge names to merged data frame
siteInfo<- merge(siteInfo, gauge_name[,c("site_no", "name", "num")], by = "site_no", all.x = TRUE)

# find the range of longitude and latitude
box <- siteInfo %>%
  summarise(left = min(dec_long_va), 
            right = max(dec_long_va), 
            bottom = min(dec_lat_va), 
            top = max(dec_lat_va))

#include river miles#
coords_RM <- read.csv("Data/coords_RM.csv" , header = TRUE)
subset_coords <- coords_RM %>% filter(RMNum >= 54 & RMNum <= 167 & (RMNum-54) %% 10 == 0)
# Subset the coordinates you want to plot
coords_subset <- coords_RM[seq(54, 160), ]

#create a data frame with the coordinates and labels for the cities
cities <- data.frame(city = c("Albuquerque", "Santa Fe"),
                     lon = c(-106.0, -105.9378),
                     lat = c(35.0844, 35.6869))
#create a data frame with the coordinates and labels
rmlables <- data.frame(rmlabs = c("RM 164", "RM 54"),
                     lon = c(-107.0, -107.1125),
                     lat = c(35.1, 33.6))

#crop the map a bit
buffer_top <- .5
buffer_sides <- .8
bottom_buffer <- 1
# create the base map using the coordinates and stamen
gauge_base <- get_map(location=c(left = box$left-buffer_sides, 
                                 bottom = box$bottom-bottom_buffer, 
                                 right = box$right + buffer_sides, 
                                 top = box$top + buffer_top), 
                      provider = "stamen", maptype = 'watercolor', scale = 10, zoom = 10)

#define colors and labels for legend
legend_colors <- c("cornflowerblue", "violet")
legend_labels <- c("Coords Subset", "Site Info")

#map it, no numbers
ggmap(gauge_base) + 
  geom_point(data = coords_subset, aes(x = longitude, y = latitude, color = "River Eyes"), size = 0.3) +
  geom_point(data = siteInfo, aes(x = dec_long_va, y = dec_lat_va, color = "USGS Gauge"), size = 1) + 
  scale_color_manual(values = c("River Eyes" = "cornflowerblue", "USGS Gauge" = "violet")) +
  geom_text(data = cities, aes(x = lon, y = lat, label = city), size = 3) +
  geom_text(data = rmlables, aes(x = lon, y = lat, label = rmlabs), size = 3)
  
#map New Mexico
nm_map <- get_map(getbb('NM'), source="stamen",
                  maptype="terrain-background")
ggmap(nm_map) +
  geom_point(data = coords_subset, aes(x = longitude, y = latitude), size = 0.3, color = "cornflowerblue") +
  geom_point(data = siteInfo, aes(x = dec_long_va, y = dec_lat_va), size = 1, color = "violet")

#map numbers on gauges
ggmap(gauge_base) + 
  geom_point(data = coords_subset, aes(x = longitude, y = latitude, color = "River Eyes"), size = 0.3) +
  geom_point(data = siteInfo, aes(x = dec_long_va, y = dec_lat_va, color = "USGS Gage"), size = 1) + 
  geom_text(data = siteInfo, aes(x = dec_long_va + 0.1, y = dec_lat_va - 0.1, label = num), size = 2, color = "violet") +
  scale_color_manual(name = "", values = c("River Eyes" = "cornflowerblue", "USGS Gage" = "violet", as.factor(siteInfo$num))) +
  geom_text(data = cities, aes(x = lon, y = lat, label = city), size = 2.5) +
  geom_text(data = rmlables, aes(x = lon, y = lat, label = rmlabs), size = 2, color = "cornflowerblue")


####map two different results from models in map####
#split river miles in the two different results
south_rm <- coords_RM %>% filter(RMNum >= 54 & RMNum <= 100 & (RMNum-54) %% 1 == 0)
north_rm <- coords_RM %>% filter(RMNum >= 101 & RMNum <= 167 & (RMNum-54) %% 1 == 0)

#map differentiating river miles by results
ggmap(gauge_base) + 
  geom_point(data = south_rm, aes(x = longitude, y = latitude, color = "South River Eyes"), size = 0.3) +
  geom_point(data = north_rm, aes(x = longitude, y = latitude, color = "North River Eyes"), size = 0.3) +
  geom_point(data = siteInfo, aes(x = dec_long_va, y = dec_lat_va, color = "USGS Gauge"), size = 1) + 
  geom_text(data = siteInfo, aes(x = dec_long_va + 0.1, y = dec_lat_va - 0.1, label = num), size = 2, color = "violet") +
  scale_color_manual(name = "", values = c("South River Eyes" = "cornflowerblue", "North River Eyes" = "#FFCDB2", "USGS Gauge" = "violet", as.factor(siteInfo$num))) +
  geom_text(data = cities, aes(x = lon, y = lat, label = city), size = 2.5)

####do I want a river layer? maybe not####
usa_rivers <- read_sf(dsn = "USA_wat", layer = "USA_water_lines_dcw")
usa_rivers <- st_transform(usa_rivers, crs = st_crs(nm_map)) # transform to same CRS as nm_map
#extract the longitude and latitude from the geometry column
usa_rivers_coords <- st_coordinates(usa_rivers)

#convert the coordinates to a data frame with columns "X" and "Y"
usa_rivers_df <- as.data.frame(usa_rivers_coords)

#plot the map
ggmap(nm_map) +
  # Add the rivers as lines
  geom_point(data = usa_rivers_df, aes(x = X, y = Y), color = "lightblue", size = 0.05)



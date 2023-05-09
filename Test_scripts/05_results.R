####READ ME####
#The purpose of this script is to plot results

####gage info####
# 8313000	RIO GRANDE AT OTOWI BRIDGE, NM
# 8317400	RIO GRANDE BELOW COCHITI DAM, NM
# 8319000	RIO GRANDE AT SAN FELIPE, NM
# 8329918	RIO GRANDE AT ALAMEDA BRIDGE AT ALAMEDA, NM
# 8329928	RIO GRANDE NR ALAMEDA, NM
# 8330000	RIO GRANDE AT ALBUQUERQUE, NM
# 8330830	RIO GRANDE AT VALLE DE ORO, NM
# 8330875	RIO GRANDE AT ISLETA LAKES NR ISLETA, NM
# 8331160	RIO GRANDE NEAR BOSQUE FARMS, NM
# 8331510	RIO GRANDE AT STATE HWY 346 NEAR BOSQUE, NM
# 8354900	RIO GRANDE FLOODWAY AT SAN ACACIA, NM

####libraries####
library(dplyr)
library(devtools)
library(ggeffects)


####calling final table####
merged2 <- read.table("Data/final_df.csv", sep = ",", header = TRUE)

#making gauge name df to make figures
gauge <- c("8313000", "8317400", "8319000", "8329918", "8329928", "8330000",
           "8330875", "8331160", "8331510", "8354900")
name <- c("1. Otowi", "2. Cochiti Dam", "3. San Felipe", "4. Alameda Bridge", "5. Alameda",
          "6. Albuquerque", "7. Isleta", "8. Bosque Farms",
          "9. St. Hwy 346", "10. San Acacia")
#make data frame
gauge_name <- cbind(gauge, name)

#merge gauge names to merged data frame
merged2 <- merge(merged2, gauge_name[,c("gauge", "name")], by = "gauge", all.x = TRUE)
#River Miles as numeric
merged2$RM <- as.numeric(merged2$RM)

#changing distance from m to km
merged2$distance <- merged2$distance / 1000


####plotting data just to look at it####
#distribution of McFadden's r-squared values
ggplot(merged2, aes(x = MFR2)) +
  geom_histogram(color = "black", fill = "cornflowerblue", bins = 100) +
  labs(title = "Distribution of McFadden's r-squared values",
       x = "MFR2",
       y = "Frequency")

#distance vs MFR2 all together
plot(MFR2 ~ distance, data = merged2)

# Change order of facet wrap plots
merged2$name <- factor(merged2$name, levels = c("1. Otowi", "2. Cochiti Dam", "3. San Felipe", "4. Alameda Bridge", "5. Alameda",
                                                 "6. Albuquerque", "7. Valle de Oro", "8. Isleta", "9. Bosque Farms",
                                                 "10. St. Hwy 346", "11. San Acacia"))


#look at single gauges in detail
ggplot(data=merged2, aes(x=distance, y=MFR2))+
  geom_point(shape = 21, fill = "transparent", size = 1) +
  facet_wrap(~name, scales="free_y")+
  theme(legend.title = element_blank()) +
  labs(x = "distance (km)") +
  scale_y_continuous(limits = c(0, 0.41)) +
  theme_bw() 


#look at error by RM
ggplot(data=merged2, aes(x=RM, y=MFR2, )) +
  geom_point(shape = 21, fill = "transparent", size = 1) +
  facet_wrap(~name, scales="free_y")+
  theme_bw() +
  scale_y_continuous(limits = c(0, 0.41)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "River Miles") +
  #specify X breaks of approximately equal size for x axis
  scale_x_continuous(breaks=seq(54, 200, 20)) 

#look at distance by RM
ggplot(data=merged2, aes(x=RM, y=distance)) +
  geom_point(shape = 21, fill = "transparent", size = 1) +
  facet_wrap(~name, scales="free_y")+
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  #specify X breaks of approximately equal size for x axis
  scale_x_continuous(breaks=seq(54, 200, 10)) 

#look at error by RM from RM 50 to 100
low_RM <- merged2 %>% filter(RM %in% (60:100))
ggplot(data=low_RM, aes(x=distance, y=MFR2)) +
  geom_point(shape = 21, fill = "transparent", size = 1) +
  facet_wrap(~name, scales="free_y")+
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "distance (km)") +
  theme_bw()
#look at error by RM
ggplot(data=low_RM, aes(x=RM, y=MFR2)) +
  geom_point(shape = 21, fill = "transparent", size = 1) +
  facet_wrap(~name, scales="free_y")+
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()

#look at error by RM from RM 50 to 100
high_RM <- merged2 %>% filter(RM %in% (100:165))
ggplot(data=high_RM, aes(x=distance, y=MFR2)) +
  geom_point(shape = 21, fill = "transparent", size = 1)  +
  facet_wrap(~name, scales="free_y")+
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "distance (km)") +
  theme_bw()
  
#look at error by RM
ggplot(data=high_RM, aes(x=RM, y=MFR2)) +
  geom_point() +
  facet_wrap(~name, scales="free_y")+
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()


#look at error by RM from RM 50 to 100
ggplot(data=low_RM, aes(x=RM, y=MFR2)) +
  geom_point(shape = 21, fill = "transparent", size = 1)  +
  facet_wrap(~name, scales="free_y")+
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()


ggplot(data = merged3, aes(x = RM, y = MFR2, color = RM) +
         geom_point(shape = 21, fill = "transparent", size = 1)  +
         scale_color_gradient(low = "blue", high = "red") +
         theme(legend.title = element_blank()) +
         theme_bw()
)

####testing models vs goodness of fit####
#look at models, if lower MFR2 is really a worse model than ones with higher MFR2
plot(ggpredict(fit_zinb[["08329918_61"]], terms = "discharge_sum")) #0.4389152
plot(ggpredict(fit_zinb[["08319000_165"]], terms = "discharge_sum")) #0.3966822
plot(ggpredict(fit_zinb[["08313000_104"]], terms = "discharge_sum")) #0.1820187
plot(ggpredict(fit_zinb[["08313000_102"]], terms = "discharge_sum")) #0.1295644
plot(ggpredict(fit_zinb[["08329928_163"]], terms = "discharge_sum")) #0.0852178
plot(ggpredict(fit_zinb[["08313000_100"]], terms = "discharge_sum")) #0.03747339
plot(ggpredict(fit_zinb[["08329918_99"]], terms = "discharge_sum")) #0.02703253
plot(ggpredict(fit_zinb[["08329918_84"]], terms = "discharge_sum")) #0.009004342
plot(ggpredict(fit_zinb[["08330000_63"]], terms = "discharge_sum")) #7.327575e-05
plot(ggpredict(fit_zinb[["08354900_101"]], terms = "discharge_sum")) #-0.00208127

####calculate what distance you need to get ~.1 for a reasonable model####
#Select Rows by column value
MFR2_1 <- merged3[merged3$MFR2 > 0.1,]

ggplot(data=MFR2_1, aes(x=RM, y=distance)) +
  geom_point() +
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()

####stats and other results####
mean(merged2$MFR2)
gage_points <- table(merged2['name'])
gage_points <- as.data.frame(gage_points)
gage_points$Freq <- as.numeric(gage_points$Freq)
sum(gage_points$Freq) # 427

#plot gage points
#remove numbers from names for plot
gage_points$name <- gsub("^\\d+\\.\\s", "", gage_points$name)
#keep same order of facet wrap plots
gage_points$name <- factor(gage_points$name, levels = c("Otowi", "Cochiti Dam", "San Felipe", "Alameda Bridge", "Alameda",
                                                "Albuquerque", "Valle de Oro", "Isleta", "Bosque Farms",
                                                "St. Hwy 346", "San Acacia"))
ggplot(gage_points, aes(x=name, y=Freq)) + 
  geom_bar(stat = "identity", color = "black", fill = "cornflowerblue") +
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



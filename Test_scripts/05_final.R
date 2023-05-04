####READ ME####
#The purpose of this script is to 

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
gauge <- c("08313000", "08317400", "08319000", "08329918", "08329928", "08330000",
           "08330830", "08330875", "08331160", "08331510", "8354900")
name <- c("OTOWI", "COCHITI DAM", "SAN FELIPE", "ALAMEDA BRIDGE", "ALAMEDA",
          "ALBUQUERQUE", "VALLE DE ORO", "ISLETA", "BOSQUE FARMS",
          "STATE HWY 346 NEAR BOSQUE", "SAN ACACIA")
#make data frame
gauge_name <- cbind(gauge, name)

#merge gauge names to merged data frame
merged2 <- merge(merged2, gauge_name[,c("gauge", "name")], by = "gauge", all.x = TRUE)
#River Miles as numeric
merged2$RM <- as.numeric(merged2$RM)

####plotting data just to look at it####
hist(merged2$MFR2)
hist(merged2$distance)
plot(MFR2 ~ distance, data = merged2)

#look at single gauges in detail
ggplot(data=merged2, aes(x=distance, y=MFR2))+
  geom_point() +
  facet_wrap(~gauge, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()

#look at error by RM
ggplot(data=merged2, aes(x=RM, y=MFR2, )) +
  geom_point() +
  facet_wrap(~name, scales="free_y")+
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw() +
  #specify X breaks of approximately equal size for x axis
  scale_x_continuous(breaks=seq(54, 200, 10)) 

#look at error by RM
ggplot(data=merged2, aes(x=RM, y=distance)) +
  geom_point() +
  facet_wrap(~name, scales="free_y")+
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw() +
  #specify X breaks of approximately equal size for x axis
  scale_x_continuous(breaks=seq(54, 200, 10)) 

#look at error by RM from RM 50 to 100
low_RM <- merged2 %>% filter(RM %in% (60:100) )
low_RM
ggplot(data=low_RM, aes(x=distance, y=MFR2)) +
  geom_point() +
  facet_wrap(~name, scales="free_y")+
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()
#look at error by RM
ggplot(data=low_RM, aes(x=RM, y=MFR2)) +
  geom_point() +
  facet_wrap(~name, scales="free_y")+
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()

#look at error by RM from RM 50 to 100
high_RM <- merged2 %>% filter(RM %in% (100:165))
high_RM
ggplot(data=high_RM, aes(x=distance, y=MFR2)) +
  geom_point() +
  facet_wrap(~name, scales="free_y")+
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
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
  geom_point() +
  facet_wrap(~name, scales="free_y")+
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()


ggplot(data = merged3, aes(x = RM, y = MFR2, color = RM) +
  geom_point() +
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

###notes####
#separate and plot distance vs error in rm 100 up and 100 down
#results:
#the more data (non zero) the easier it is to predict something, even with a model that is suited specificly to that
#there is a difference between in rm 100 up and 100 down
#
#section parts of the river in the map 


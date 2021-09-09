#Combined data exploration plotting and analysis... now that data are all combined

#read in output from: 3_combineFiles_Detections_SB.R

rm(list=ls())

library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)

#-----------------------------------------------------------------------------------------
# READ IN DATA
#-----------------------------------------------------------------------------------------
tDir  =  "E:\\RESEARCH\\SanctSound\\"
inDir = paste0(tDir,"data2\\combineFiles3_Detections")
sanct = "SB"
site = "SB02"
inHr  = list.files(inDir,sanct,full.names = T)
load(inHr[1])
load(inHr[3])

#-----------------------------------------------------------------------------------------
#COVID change: select days in March-- to match AIS summaries
#-----------------------------------------------------------------------------------------
tmp = dataA[dataA$JulianDay > 82 & dataA$JulianDay < 91,]
as.data.frame(colnames(tmp))

#WIND SPEED
ggplot(tmp, aes(as.factor(JulianDay), meanWSPD, group=as.factor(Yr), color = as.factor(Yr))) +
  geom_line() + 
  xlab("")+ ylab("Wind Speed (mps) ") +
  ggtitle(("SB02: March 25-31 [2019=5.8, 2020=7.1]"))+
  theme_minimal()
unique(tmp$Yr)
tmp %>% group_by((Yr)) %>% summarise(newvar = mean(meanWSPD, na.rm = T) )

#TILE PLOT of all sources present...
BioAllomelt = reshape :: melt(tmp, id.vars = "DateFs", 
                              measure.vars = c("bluewhaleP","finwhaleP","humpbackwhaleP",
                                               "northatlanticrightwhaleP","seiwhaleP", 
                                               "dolphinsDetDay","atlanticcodDetDay" ))
BioAllomelt$value = as.numeric(as.character(BioAllomelt$value) )

ggplot(BioAllomelt, aes(DateFs, variable, fill= (value))) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  labs(title = paste0(site,": summary of sounds"),  fill = "Detections") +
  xlab("") +
  ylab("")

#plot of conditions
pwind = ggplot(tmp, aes(as.factor(JulianDay), meanWSPD, group=as.factor(Yr), color = as.factor(Yr))) +
  geom_line() +   geom_point() +
  xlab("Julian Day")+  ylab("Wind Speed (mps) ") +
  theme_minimal()+ theme(legend.position = "none")

ptide = ggplot(tmp, aes(as.factor(JulianDay), maxTide, group=as.factor(Yr), color = as.factor(Yr))) +
  geom_line() +
  geom_point() +
  xlab("")+  ylab("Max tide change ") +
  theme_minimal()+
  theme(legend.position = "none")

p125 = ggplot(tmp, aes(as.factor(JulianDay), OL_250, group=as.factor(Yr), color = as.factor(Yr))) +
  geom_line() +
  geom_point() +
  xlab("")+  ylab("Ol_125 ") +
  theme_minimal()+
  theme(legend.position = "none")

p63 = ggplot(tmp, aes(as.factor(JulianDay), OL_63, group=as.factor(Yr), color = as.factor(Yr))) +
  geom_line() +
  geom_point() +
  xlab("")+  ylab("Ol_63 ") +
  theme_minimal()+
  theme(legend.position = "none")

BioAllomelt$JulianDay = yday(BioAllomelt$DateFs)
BioAllomelt$Yr = year(BioAllomelt$DateFs)
#plot of sources by year, source=x,Y=value
pBio = ggplot(BioAllomelt, aes(variable, value, fill=as.factor(Yr)) ) +
  geom_bar(position = "fill",stat = "identity" )+
  xlab("")+ ylab("")+
  scale_x_discrete(labels = c("Blue","Fin","Humpback","NARW", 
                              "Sei","Dolphin","Cod"))+
  scale_fill_discrete(name = "")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 50),legend.position="top" )
uSources = unique(BioAllomelt$variable)
BioAllomelt[BioAllomelt$variable == uSources[1],]
BioAllomelt[BioAllomelt$variable == uSources[2],] #everyday had 1
BioAllomelt[BioAllomelt$variable == uSources[3],] #one more day 2020 with humpbacks
BioAllomelt[BioAllomelt$variable == uSources[4],] #2 day 2019, 7 day in 2020
BioAllomelt[BioAllomelt$variable == uSources[5],] #one more day 2020 with sei
BioAllomelt[BioAllomelt$variable == uSources[6],] #no data for dolphins/cod
BioAllomelt[BioAllomelt$variable == uSources[7],] 

#line plots
grid.arrange(p125,p63,pwind,ptide,pBio,ncol = 5)

#boxplots
pwind = ggplot(tmp, aes(as.factor(Yr), meanWSPD, fill = as.factor(Yr))) +
  geom_boxplot() +
  xlab("Julian Day")+  ylab("Wind Speed (mps) ") +
  theme_minimal()+ theme(legend.position = "none")

ptide = ggplot(tmp, aes(as.factor(Yr), maxTide, fill = as.factor(Yr))) +
  geom_boxplot() +
  xlab("")+  ylab("Max tide change ") +
  theme_minimal()+   theme(legend.position = "none")

p125 = ggplot(tmp, aes(as.factor(Yr), OL_250, fill = as.factor(Yr))) +
  geom_boxplot() +
  xlab("")+  ylab("Ol_125 ") +
  theme_minimal()+   theme(legend.position = "none")

p63 = ggplot(tmp, aes(as.factor(Yr), OL_63, fill = as.factor(Yr))) +
  geom_boxplot() +
  xlab("")+  ylab("Ol_63 ") +
  theme_minimal()+   theme(legend.position = "none")

#VESSEL Metrics
pDetVes = ggplot(tmp, aes(as.factor(Yr), PercentDay_TimeVesselDet, fill = as.factor(Yr))) +
  geom_boxplot() +
  xlab("")+  ylab("% day with vessel noise (Detector) ") +
  theme_minimal()+   theme(legend.position = "none")

pAISa = ggplot(tmp, aes(as.factor(Yr), LOA_ALL_UV, fill = as.factor(Yr))) +
  geom_boxplot() +
  xlab("")+  ylab("Unique Vessels (AIS) ") +
  theme_minimal()+   theme(legend.position = "none")
pAISs = ggplot(tmp, aes(as.factor(Yr), LOA_S_UV, fill = as.factor(Yr))) +
  geom_boxplot() +
  xlab("")+  ylab("Small Unique Vessels (AIS) ") +
  theme_minimal()+   theme(legend.position = "none")

pAISm = ggplot(tmp, aes(as.factor(Yr), LOA_M_UV, fill = as.factor(Yr))) +
  geom_boxplot() +
  xlab("")+  ylab("Medium Unique Vessels (AIS) ") +
  theme_minimal()+   theme(legend.position = "none")

pAISl = ggplot(tmp, aes(as.factor(Yr), LOA_L_UV, fill = as.factor(Yr))) +
  geom_boxplot() +
  xlab("")+  ylab("Large Unique Vessels (AIS) ") +
  theme_minimal()+   theme(legend.position = "none")

grid.arrange(p125,p63,pwind,ptide,pBio,ncol = 5)
grid.arrange(pDetVes,pAISa,pAISs,pAISm,pAISl,ncol = 5)

#-----------------------------------------------------------------------------------------
#monthly difference graphics
#-----------------------------------------------------------------------------------------
as.data.frame(colnames(dataA))

#1) boxplot of monthly differences
#-----------------------------------------------------------------------------------------
#sound level- 125Hz
#-----------------------------------------------------------------------------------------
p125 = ggplot(dataA, aes(as.factor(Mth), OL_125, fill = as.factor(Yr))) +
  geom_boxplot(outlier.colour="black", outlier.size =.2, outlier.alpha = .2) +
  theme_minimal() +
  xlab("") +   ylab ("") + 
  ggtitle("Low frequency sound levels [125 Hz]")+ #median [125 Hz]
  #coord_cartesian(ylim = c(65, 120)) +
  scale_fill_manual(values=c("gray","#E69F00", "#56B4E9") ) +
  theme(axis.text        = element_text(size=11),
        panel.background = element_rect(fill = "transparent"),
        plot.caption     = element_text(color = "black", face = "italic",hjust = 0),
        axis.title       = element_text(size=11,face="bold"),
        plot.background  = element_rect(fill = "transparent", color = NA),
        #plot.margin      = unit(c(1,2,2,2), "lines"),
        legend.position = "none",
        legend.title = element_blank() )
#vessel noise 
#-----------------------------------------------------------------------------------------
pDet = ggplot(dataA, aes(as.factor(Mth), PercentDay_TimeVesselDet, fill = as.factor(Yr))) +
  geom_boxplot(outlier.colour="black", outlier.size =.2, outlier.alpha = .2) +
  theme_minimal() +
  xlab("") +   ylab ("") + 
  ggtitle("% Day vessel noise")+ #median [125 Hz]
  #coord_cartesian(ylim = c(65, 120)) +
  scale_fill_manual(values=c("gray","#E69F00", "#56B4E9") ) +
  theme(axis.text        = element_text(size=11),
        panel.background = element_rect(fill = "transparent"),
        plot.caption     = element_text(color = "black", face = "italic",hjust = 0),
        axis.title       = element_text(size=11,face="bold"),
        plot.background  = element_rect(fill = "transparent", color = NA),
        #plot.margin      = unit(c(1,2,2,2), "lines"),
        legend.position = "none",
        legend.title = element_blank() )
pAIS = ggplot(dataA, aes(as.factor(Mth), LOA_ALL_UV, fill = as.factor(Yr))) +
  geom_boxplot(outlier.colour="black", outlier.size =.2, outlier.alpha = .2) +
  theme_minimal() +
  xlab("") +   ylab ("") + 
  ggtitle("Unique AIS vessels")+ #median [125 Hz]
  #coord_cartesian(ylim = c(65, 120)) +
  scale_fill_manual(values=c("gray","#E69F00", "#56B4E9") ) +
  theme(axis.text        = element_text(size=11),
        panel.background = element_rect(fill = "transparent"),
        plot.caption     = element_text(color = "black", face = "italic",hjust = 0),
        axis.title       = element_text(size=11,face="bold"),
        plot.background  = element_rect(fill = "transparent", color = NA),
        #plot.margin      = unit(c(1,2,2,2), "lines"),
        legend.position = "none",
        legend.title = element_blank() )
pWind = ggplot(dataA, aes(as.factor(Mth), meanWSPD, fill = as.factor(Yr))) +
  geom_boxplot(outlier.colour="black", outlier.size =.2, outlier.alpha = .2) +
  theme_minimal() +
  xlab("") +   ylab ("") + 
  ggtitle("Wind speed ")+ #median [125 Hz]
  #coord_cartesian(ylim = c(65, 120)) +
  scale_fill_manual(values=c("gray","#E69F00", "#56B4E9") ) +
  theme(axis.text        = element_text(size=11),
        panel.background = element_rect(fill = "transparent"),
        plot.caption     = element_text(color = "black", face = "italic",hjust = 0),
        axis.title       = element_text(size=11,face="bold"),
        plot.background  = element_rect(fill = "transparent", color = NA),
        #plot.margin      = unit(c(1,2,2,2), "lines"),
        legend.position = "bottom",
        legend.title = element_blank() )
#biology-- bar?
#-----------------------------------------------------------------------------------------
BioAllomeltA = reshape :: melt(dataA, id.vars = "DateFs", 
                               measure.vars = c("bluewhaleP","finwhaleP","humpbackwhaleP",
                                                "northatlanticrightwhaleP","seiwhaleP" ))
BioAllomeltA$value = as.numeric(as.character(BioAllomeltA$value) )
BioAllomeltA$Yr  = year( as.Date(BioAllomeltA$DateFs ))
BioAllomeltA$Mth = month( as.Date(BioAllomeltA$DateFs ))
BioAllomeltA = BioAllomeltA[ BioAllomeltA$Yr > 2018,]
#values for each factor (month) to be summed up within each bar
BioAllomeltA %>% 
  group_by(variable, Yr, Mth) %>% 
  summarize(summedvalues = sum(value,na.rm = T)) %>% 
  ggplot(aes(as.factor(Mth), summedvalues, fill=as.factor(Yr)) ) +
  geom_bar(position = "dodge", stat = "identity",width = .5)+
  scale_fill_manual(values=c("#E69F00", "#56B4E9") )+
  facet_wrap(~variable, ncol=1)+
  xlab("")+ ylab("Days with calls")+
  theme(legend.title = element_blank())
#thanks Max!

grid.arrange(p125,pDet,pAIS,pWind,ncol = 1)

#-----------------------------------------------------------------------------------------
#Difference MONTH COMPARISION--- 2019 vs 2020, heat map
#-----------------------------------------------------------------------------------------
as.data.frame(colnames(tmp))
SPLAll1 = dataA[dataA$Yr == 2019 | dataA$Yr == 2020, ]
SPLAll1$Bal  = rowSums ( SPLAll1[,33:37]) #sum of baleen whale calls present in given day
SPLAll1$BalP = 0 
SPLAll1$BalP[SPLAll1$Bal> 0] = 1
#summarize by year, month as median values or mean
by125 = as.data.frame ( SPLAll1 %>%
                          group_by(Mth,Yr) %>%
                          summarise(qs125 = quantile(OL_125, .5), nDays = n() ) )
diff125 = NULL
for(mm in 1:12){
  tmp1 = by125[by125$Mth == mm,]
  if (nrow( tmp1) == 0 ) { 
    diff125 = rbind(diff125, c( mm, NA,NA,NA)) #no date for site/month
  }else if (nrow( tmp1) == 1 ){ 
    diff125 = rbind(diff125, c( mm, NA,NA,NA)) #not data for both years
  }else if (nrow(tmp1) == 2) {
    
    d2019 = tmp1$qs125[tmp1$Yr == 2019]
    d2020 = tmp1$qs125[tmp1$Yr == 2020]
    diff125 = rbind(diff125, c(mm, (d2020 - d2019), d2019, d2020 ) ) #not data for both years
  }
}
colnames(diff125) = c("Mth","Diff_125","2019_125","2020_125")
diff125 = as.data.frame(diff125)
diff125$Mth = ordered(diff125$Mth, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11","12"))

#OL_63
by63= as.data.frame ( SPLAll1 %>%
                        group_by(Mth,Yr) %>%
                        summarise(qs63 = quantile(OL_63, .5), nDays = n() ) )
diff63 = NULL
for(mm in 1:12){
  tmp1 = by63[by63$Mth == mm,]
  if (nrow( tmp1) == 0 ) { 
    diff63 = rbind(diff63, c( mm, NA,NA,NA)) #no date for site/month
  }else if (nrow( tmp1) == 1 ){ 
    diff63 = rbind(diff63, c( mm, NA,NA,NA)) #not data for both years
  }else if (nrow(tmp1) == 2) {
    
    d2019 = tmp1$qs63[tmp1$Yr == 2019]
    d2020 = tmp1$qs63[tmp1$Yr == 2020]
    diff63 = rbind(diff63, c(mm, (d2020 - d2019), d2019, d2020 ) ) #not data for both years
  }
}
colnames(diff63) = c("Mth","Diff_63","2019_63","2020_63")
diff63 = as.data.frame(diff63)

#WIND
byWind = as.data.frame ( SPLAll1 %>%
                           group_by(Mth,Yr) %>%
                           summarise(avgWSPD = mean(meanWSPD,na.rm = T), sdWSPD = sd(meanWSPD,na.rm = T), nDays = n() ) )

diffW = NULL
for(mm in 1:12){
  tmp1 = byWind[byWind$Mth == mm,]
  if (nrow( tmp1) == 0 ) { 
    diffW = rbind(diffW, c( mm, NA,NA,NA)) #no date for site/month
  }else if (nrow( tmp1) == 1 ){ 
    diffW = rbind(diffW, c( mm, NA,NA,NA)) #not data for both years
  }else if (nrow(tmp1) == 2) {
    
    d2019 = tmp1$avgWSPD[tmp1$Yr == 2019]
    d2020 = tmp1$avgWSPD[tmp1$Yr == 2020]
    diffW = rbind(diffW, c(mm, (d2020 - d2019), d2019, d2020 ) ) #not data for both years
  }
}
colnames(diffW) = c("Mth","Diff_WIND","2019_WIND","2020_WIND")
diffW = as.data.frame(diffW)

#days with baleen calls present
byBio = as.data.frame ( SPLAll1 %>%
                          group_by(Mth,Yr) %>%
                          summarise(daysBio = sum(BalP,na.rm = T), nDays = n() ) )
diffB = NULL
for(mm in 1:12){
  tmp1 = byBio[byBio$Mth == mm,]
  if (nrow( tmp1) == 0 ) { 
    diffB = rbind(diffB, c( mm, NA,NA,NA)) #no date for site/month
  }else if (nrow( tmp1) == 1 ){ 
    diffB = rbind(diffB, c( mm, NA,NA,NA)) #not data for both years
  }else if (nrow(tmp1) == 2) {
    
    d2019 = tmp1$daysBio[tmp1$Yr == 2019]
    d2020 = tmp1$daysBio[tmp1$Yr == 2020]
    diffB = rbind(diffB, c(mm, (d2020 - d2019), d2019, d2020 ) ) #not data for both years
  }
}
colnames(diffB) = c("Mth","Diff_BioDay","2019_BioDay","2020_BioDay")
diffB = as.data.frame(diffB)


byBioC = as.data.frame ( SPLAll1 %>%
                           group_by(Mth,Yr) %>%
                           summarise(countBio = sum(Bal,na.rm = T), nDays = n() ) )
diffBc = NULL
for(mm in 1:12){
  tmp1 = byBioC[byBioC$Mth == mm,]
  if (nrow( tmp1) == 0 ) { 
    diffBc = rbind(diffBc, c( mm, NA,NA,NA)) #no date for site/month
  }else if (nrow( tmp1) == 1 ){ 
    diffBc = rbind(diffBc, c( mm, NA,NA,NA)) #not data for both years
  }else if (nrow(tmp1) == 2) {
    
    d2019 = tmp1$countBio[tmp1$Yr == 2019]
    d2020 = tmp1$countBio[tmp1$Yr == 2020]
    diffBc = rbind(diffBc, c(mm, (d2020 - d2019), d2019, d2020 ) ) #not data for both years
  }
}
colnames(diffBc) = c("Mth","Diff_BioC","2019_BioC","2020_BioC")
diffBc = as.data.frame(diffBc)

# combine all metrics
#-----------------------------------------------------------------------------------------
diffALL = cbind(diff125,diff63[,2:4],diffW[,2:4],diffB[,2:4],diffBc[,2:4] )
diffALLmelt = reshape :: melt(diffALL, id.vars = "Mth", 
                              measure.vars = c("Diff_125","Diff_63","Diff_WIND","Diff_BioDay","Diff_BioC" ))

#PLOT: x = month, y = metric, fill is difference
#-----------------------------------------------------------------------------------------
#positive is higher in 2020, negative in lower in 2020
ggplot(diffALLmelt, aes(Mth, variable ,fill=as.numeric(as.character(value)) ) ) +
  geom_tile()+
  xlab("Month") + ylab("")+
  scale_fill_gradient2(low="white", high="black")+
  scale_y_discrete(labels = c("125 Hz", "63 Hz", "Wind", "Baleen whale [days]","Baleen whale [calls]"))+
  scale_x_discrete(labels = c("Jan", "Feb", "Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec"))+
  ggtitle("Change in soundscape features (+ higher 2020, - lower 2020)")+
  theme(legend.title = element_blank())


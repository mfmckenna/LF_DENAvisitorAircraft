# combine abiotic conditions with SanctSound 1_processVesselDetecionsAIS.R
# CURRENTLY, ONLY WORKS FOR SB02!!!


rm(list=ls())

library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)
library(suncalc)
#-----------------------------------------------------------------------------------------
# MAIN functions-- by site processing
#-----------------------------------------------------------------------------------------
#INPUT a per site .csv file: 
tDir = "E:\\RESEARCH\\SanctSound\\data2\\"
inDir =  paste0(tDir,"combineFiles1_SPLShips")
outDir = paste0(tDir,"combineFiles2_Abiotic")

inDay = list.files(inDir,"_Day_",full.names = T)
inHr  = list.files(inDir,"Hr_",full.names = T)

#start a loop here!!!
ii = 22
cat("Processing...", basename( inHr[ii]) )
ifile = inHr[ii]
idataH = read.csv(ifile)
ifile = inDay[ii]
idataD = read.csv(ifile)

#check date range
min(as.Date(idataH$Day))
max(as.Date(idataH$Day))
dcolH=as.data.frame(colnames(idataH))
dcolH
dcolD=as.data.frame(colnames(idataD))
dcolD
site = substr( idataH$Site[1],1,2) 

cat("Processing...", site, ":", basename( inHr[ii]) )

#format data
SplAis = idataH
head(SplAis)

#-----------------------------------------------------------------------------------------
#read in abiotic data for the sanctuary
#-----------------------------------------------------------------------------------------
#location
#-----------------------------------------------------------------------------------------
dirAb = paste0(tDir,"abioticFiles\\")
siteLoc = read.csv(paste0(dirAb, "LocationSummary.csv"))
sLoc    = siteLoc[as.character(siteLoc$Site) == as.character(idataH$Site[1]),]

#tide
#-----------------------------------------------------------------------------------------
setwd(dirAb)
multmerge = function(path){
  filenames = intersect(list.files(dirAb,pattern = site), 
                        list.files(dirAb,pattern = "^CO-OPS"))
  rbindlist(lapply(filenames, fread))
}
#filenames = intersect(list.files(dirAb,pattern = site), list.files("E:\\RESEARCH\\SanctSound\\data2\\abioticFiles",pattern = "^CO-OPS"))
TIDE <- multmerge(dirAb)
TIDE$DateFday = as.Date(TIDE$Date ,format = "%Y/%m/%d")
TIDE$DateF    = as.POSIXct( paste(TIDE$DateFday, paste(TIDE$`Time (GMT)`,":00", sep = ""), sep=" "), tz = "GMT")
TIDE$changeWL = c("NA", diff(TIDE$`Verified (ft)`))
#Data check plot
pTide = ggplot(TIDE, aes(DateFday, as.numeric(as.character(changeWL)) ) )+
  geom_point() +
  xlab("")+
  ylab("Hourly change in water level") +
  theme_minimal()+ggtitle(paste("Check- TIDE(",site, ")"))
#pTide

#wind
#-----------------------------------------------------------------------------------------
multmerge = function(path){
  filenames = intersect(list.files(dirAb,pattern = site), 
                        list.files(dirAb,pattern = "MetA"))
  rbindlist(lapply(filenames, fread))
}
#filenames = intersect(list.files("E:\\RESEARCH\\SanctSound\\data2\\abioticFiles",pattern = site), list.files("E:\\RESEARCH\\SanctSound\\data2\\abioticFiles",pattern = "^CO-OPS"))
WSPD <- multmerge(dirAb)
# wFiles = intersect(list.files("E:\\RESEARCH\\SanctSound\\data2\\abioticFiles",pattern = site), list.files("E:\\RESEARCH\\SanctSound\\data2\\abioticFiles",pattern = "MetA"))
# head( read.csv( wFiles[1] )) 
# head( read.csv( wFiles[2] )) 
# head( read.csv( wFiles[3] )) 
endR = nrow(WSPD)
WSPD = as.data.frame(WSPD[2:endR,])
#head( WSPD )
WSPD$DateC    =  ( paste( WSPD$`#YY`, "-", WSPD$MM, "-", WSPD$DD, " ", WSPD$hh, ":",  WSPD$mm , sep = ""))
WSPD$DateF = as.POSIXct(WSPD$DateC,"%Y-%m-%d %H:%M",tz ="GMT")
WSPD$WSPD  = as.numeric(as.character(WSPD$WSPD  )) #some days with NAs which( is.na(WSPD$WSPD)) = 9
max(WSPD$WSPD,na.rm= T) 
#na_if(WSPD$WSPD, 99) 
#need to remove 99!!!
WSPD$WSPD[WSPD$WSPD == 99] = NA
pWind = ggplot(WSPD, aes(DateF, WSPD ) )+
  geom_point() +
  xlab("")+
  ylab("Wind Speed (mps) ") +
  theme_minimal()+ggtitle(paste("Check- WIND (",site, ")"))
# pWind

WSPD$Day   = as.Date(WSPD$DateF, format = "%Y-%m-%d")
WSPD$HR    = strftime(WSPD$DateF, format="%H") #hour(WSPD$DateF)
WSPD$DayHR = as.POSIXct( paste0(WSPD$Day," ", WSPD$HR,":00:00") ,format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
WSPDavg = aggregate(WSPD$WSPD, by=list(WSPD$DayHR), mean)
colnames(WSPDavg) = c("DateF","avgWSPD")

#-----------------------------------------------------------------------------------------
#COMBINE SUN ALTITUDE, WIND and TIDE
#-----------------------------------------------------------------------------------------
#matches the hourly windspeed with SPL values
SplAis$DateF = as.POSIXct(   SplAis$DateTime, tz = "GMT")
SplAisWspd     = merge(SplAis, WSPDavg,  all = FALSE, all.x = TRUE, by = "DateF" )#mean at the top of the hour
# check:  WSPDavg[10000,] , SplAisWspd[SplAisWspd$DateF == WSPDavg[10000,1],]

#SUN ALTITUDE-- append to wind
tmp = getSunlightPosition(SplAisWspd$DateF ,sLoc$lat,sLoc$Lon)
SplAisWspd$sunAlt = tmp$altitude
SplAisWspdTide = merge(SplAisWspd, TIDE, all = FALSE, all.x = TRUE, by = "DateF" )

rm(WSPD,WSPDavg,TIDE,AIS)

setwd(outDir)
DC = Sys.Date()
SplAisWspd$Site[1]
fnameAll =     paste0(SplAisWspdTide$Site[1], "_CombinedData_SPLShipsAbiotic_HR_",as.character(min(as.Date(SplAisWspdTide$Day))),"_",
                      as.character(max(as.Date(SplAisWspdTide$Day))), "_v", DC, ".csv")  
write.csv(SplAisWspdTide,fnameAll) 
dcol = as.data.frame(colnames(SplAisWspdTide))

dcol

#-----------------------------------------------------------------------------------------
# EXTRA: DATA EXPLORATION-- SB02
#-----------------------------------------------------------------------------------------
#March 25-31 201 Vs 2020 julian day 83-90
#WIND SPEED
tmp = SplAisWspdTide[SplAisWspdTide$JulianDay > 82 & SplAisWspdTide$JulianDay < 91,]
tmp$Yr = year(tmp$Day)

ggplot(tmp, aes(as.factor(JulianDay), avgWSPD,fill=as.factor(Yr))) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=1, notch=FALSE) +
  xlab("")+
  ylab("Wind Speed (mps) ") +
  ggtitle(("SB02: March 25-31 [2019=5.8, 2020=7.1]"))+
  theme_minimal()
tmp %>% group_by(Yr) %>% summarize(newvar = mean(avgWSPD,na.rm = T))


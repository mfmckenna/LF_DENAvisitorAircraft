# combine detections with SanctSound 2_combineFiles_Abiotic.R
#only works for SB02, also plots comparisons for 2020 vs 2021

rm(list=ls())

#-----------------------------------------------------------------------------------------
library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)

#-----------------------------------------------------------------------------------------
tDir   = "E:\\RESEARCH\\SanctSound\\"
inDir  = paste0(tDir, "data2\\combineFiles2_Abiotic")
inDir2 = paste0(tDir, "data2\\combineFiles1_SPLShips")
outDir = paste0(tDir, "data2\\combineFiles3_Detections")

#-----------------------------------------------------------------------------------------
#hourly files
sanct = "SB"
site  = "SB02"
inHr  = list.files(inDir,sanct,full.names = T)
iData = read.csv(inHr[1])
iData$DateF = as.POSIXct (iData$DateF,tz = "GMT")
as.data.frame( colnames(iData) )
summary(iData)

#Daily files (from previous steps)
inDay = list.files( inDir2, "_Day_", full.names = T)
ii = 22
cat("Processing...", basename( inDay[ii]) )
ifile = inDay[ii]
idataD = read.csv(ifile)
numDays = unique(idataD$Day)
as.data.frame( colnames(idataD) )
summary(idataD)

#-----------------------------------------------------------------------------------------
#Biological Detections
## atlanticcod, dolphins, bluewhale, finwhale, humpbackwhale, northatlanticrightwhale, seiwhale
#-----------------------------------------------------------------------------------------
dir2  = paste0(tDir, "data\\",sanct,"\\",site)

#ATLANTIC COD-- start and end times, need to convert to daily detections
#-----------------------------------------------------------------------------------------
nFiles = length( list.files(path=dir2, pattern = "atlanticcod", full.names=TRUE, recursive = TRUE))
atlanticcod = NULL
for (ff in 1 : nFiles){
  fname  = list.files(path=dir2, pattern = "atlanticcod", full.names=FALSE, recursive = TRUE)[ff]
  fnameF = list.files(path=dir2, pattern = "atlanticcod", full.names=TRUE, recursive = TRUE)[ff]
  dname = sapply (strsplit( fname, "_" ),"[",3 )
  tmp  = fread(fnameF,header=F) #remove these after
  tmp$deplm = dname
  #cat(dname, "_", as.character( tmp[1,1]), ", ", as.character( tmp[1,2]) ,"\n")
  atlanticcod = rbind(atlanticcod,tmp)
  rm(fname,fnameF,tmp)
}
colnames(atlanticcod) = c( as.character(atlanticcod[1,1:3]) ,"site")
atlanticcod = atlanticcod[atlanticcod$Presence != "Presence",] #remove headers in the data frame
atlanticcod$DateFs   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", atlanticcod$Begin_datetime_ISO6801)), tz = "GMT" )
atlanticcod$DateFe   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", atlanticcod$End_datetime_ISO6801))  , tz = "GMT" )
atlanticcod$DUR_mins = atlanticcod$DateFe- atlanticcod$DateFs #in minutes!
atlanticcod$DayHR    = as.POSIXct( paste0(as.Date( atlanticcod$DateFs )," ", hour( atlanticcod$DateFs ),":00:00") ,format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
atlanticcod$DateDay  = as.Date( atlanticcod$DateFs )
#combine with hourly data- start of the detection in specific hour
iData$atlanticcod = 0
for(ii in 1:nrow(iData)){
  tdet = nrow( atlanticcod[atlanticcod$DayHR== iData$DateF[ii], ])
  if (tdet>0){iData$atlanticcod[ii] =  tdet}
} #plot(iData$atlanticcod )
#convert to daily detections
udays = unique(atlanticcod$DateDay)
atlanticcodD = NULL
for(dd in 1:length(udays)){
  tmp = atlanticcod[ atlanticcod$DateDay == udays[dd],]
  atlanticcodD = rbind(atlanticcodD, c(as.character(udays[dd]),nrow(tmp) ) )
}
colnames(atlanticcodD) = c("DateFs","atlanticcodDetDay")
atlanticcodD = as.data.frame(atlanticcodD)
atlanticcodD$DateFs = as.Date(atlanticcodD$DateFs)
atlanticcodD$atlanticcodDetDay = as.numeric( as.character( (atlanticcodD$atlanticcodDetDay) ) )

#DOLPHINS-- hourly presence/absence [0 or 1]
#-----------------------------------------------------------------------------------------
multmerge = function(path){
  filenames = list.files(path=path, pattern = "dolphins", full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
} 
dolphins = ( multmerge(dir2) ) # unique(dolphins$Presence)
dolphins = as.data.frame(dolphins)
dolphins$DateFs  = as.POSIXct( gsub(".000Z", "", gsub("T", " ", dolphins$ISOStartTime)), tz = "GMT" )
dolphins$DayHR   = as.POSIXct( paste0(as.Date( dolphins$DateFs )," ", hour( dolphins$DateFs ),":00:00") ,format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
dolphins$DateDay = as.Date( dolphins$DateFs )
names(dolphins)[2] = "dolphinsP" 
dolphins  = dolphins[!is.na(dolphins$DateFs),]
iData$dolphins = 0
for(ii in 1:nrow(iData)){
  tdet = nrow( dolphins[dolphins$DayHR== iData$DateF[ii], ])
  if (tdet>0){iData$dolphins[ii] =  tdet}
}# plot(iData$dolphins )
#convert to daily detections
udays = unique(dolphins$DateDay)
dolphinsD = NULL
for(dd in 1:length(udays)){
  tmp = dolphins[ dolphins$DateDay == udays[dd],]
  dolphinsD = rbind(dolphinsD, c(as.character(udays[dd]),nrow(tmp),sum(tmp$dolphinsP)))
}
colnames(dolphinsD) = c("DateFs","DolphinHrs","dolphinsDetDay")
dolphinsD = as.data.frame(dolphinsD)
dolphinsD$DateFs = as.Date(dolphinsD$DateFs)
dolphinsD$DolphinHrs = as.numeric( as.character(dolphinsD$DolphinHrs) )
dolphinsD$dolphinsDetDay = as.numeric( as.character( (dolphinsD$dolphinsDetDay) ) )

#BLUE WHALE-- presence in a given day [0 or 1]
#-----------------------------------------------------------------------------------------
nFiles = length( list.files(path=dir2,pattern = "bluewhale", full.names=TRUE, recursive = TRUE))
bluewhale = NULL
for (ff in 1 : nFiles){
  fname  = list.files(path=dir2, pattern = "bluewhale", full.names=FALSE, recursive = TRUE)[ff]
  fnameF = list.files(path=dir2, pattern = "bluewhale", full.names=TRUE, recursive = TRUE)[ff]
  dname  = sapply (strsplit( fname, "_" ),"[",3 )
  tst  =  sapply (strsplit( fname, "_" ),"[",5 )
  
  if(tst == "bluewhale"){
    cat("Processing...", fname,"\n")
    tmp  = fread(fnameF,header=F) #remove these after
    tmp$Site = dname
    bluewhale = rbind(bluewhale,tmp)
    rm(fname,fnameF,tmp)
  }
  
}
colnames(bluewhale)= c( as.character(bluewhale[1,1:3]))
bluewhale = bluewhale[bluewhale$Presence != "Presence",] #remove headers in the data frame
bluewhale$DateFs   = as.Date( bluewhale$ISOStartTime,tz="GMT")
names(bluewhale)[2] = "bluewhaleP" 
names(bluewhale)[3] = "Site"
bluewhale = bluewhale[!is.na(bluewhale$DateFs),]
bluewhale = bluewhale[!duplicated(bluewhale)]

#FIN WHALE-- presence in a given day [0 or 1]
#-----------------------------------------------------------------------------------------
nFiles = length( list.files(path=dir2,pattern = "finwhale", full.names=TRUE, recursive = TRUE))
finwhale = NULL
for (ff in 1 : nFiles){
  fname  = list.files(path=dir2, pattern = "finwhale", full.names=FALSE, recursive = TRUE)[ff]
  fnameF = list.files(path=dir2, pattern = "finwhale", full.names=TRUE, recursive = TRUE)[ff]
  dname = sapply (strsplit( fname, "_" ),"[",3 )
  tst  =  sapply (strsplit( fname, "_" ),"[",5 )
  if(tst == "finwhale"){
    cat("Processing...", fname,"\n")
    tmp  = fread(fnameF,header=F) #remove these after
    tmp$Site = dname
    finwhale = rbind(finwhale,tmp)
    rm(fname,fnameF,tmp)
  }
}
colnames(finwhale)= c( as.character(finwhale[1,1:3]) )
finwhale = finwhale[finwhale$Presence != "Presence",] #remove headers in the data frame
finwhale = finwhale[finwhale$ISOStartTime != "",] #remove headers in the data frame
finwhale$DateFs   = as.Date( finwhale$ISOStartTim,tz="GMT")
names(finwhale)[2] = "finwhaleP" 
names(finwhale)[3] = "Site" 
finwhale = finwhale[!is.na(finwhale$DateFs),]
finwhale = finwhale[!duplicated(finwhale)]
#still duplicated entries...- between deployment, keep the presence = 1
finwhale$finwhaleP[duplicated(finwhale$ISOStartTime) ] = 1
finwhale = finwhale[!duplicated(finwhale)]

#HUMPBACK WHALE-- presence in a given day [0 or 1]
#-----------------------------------------------------------------------------------------
# list.files(path=dir2,pattern = "humpbackwhale", full.names=TRUE, recursive = TRUE)
nFiles = length( list.files(path=dir2,pattern = "humpbackwhale", full.names=TRUE, recursive = TRUE))
humpbackwhale = NULL
for (ff in 1 : nFiles){
  fname  = list.files(path=dir2, pattern = "humpbackwhale", full.names=FALSE, recursive = TRUE)[ff]
  fnameF = list.files(path=dir2, pattern = "humpbackwhale", full.names=TRUE, recursive = TRUE)[ff]
  dname = sapply (strsplit( fname, "_" ),"[",3 )
  tst  =  sapply (strsplit( fname, "_" ),"[",5 )
  if(tst == "humpbackwhale"){
    cat("Processing...", fname,"\n")
    tmp  = fread(fnameF,header=F) #remove these after
    tmp$Site = dname
    humpbackwhale = rbind(humpbackwhale,tmp)
    rm(fname,fnameF,tmp)
  }
}
colnames(humpbackwhale)= c( as.character(humpbackwhale[1,1:3]) )
humpbackwhale = humpbackwhale[humpbackwhale$Presence != "Presence",] #remove headers in the data frame
humpbackwhale = humpbackwhale[humpbackwhale$ISOStartTime != "",] #remove headers in the data frame
humpbackwhale$DateFs   = as.Date( humpbackwhale$ISOStartTim,tz="GMT")
names(humpbackwhale)[2] = "humpbackwhaleP" 
names(humpbackwhale)[3] = "Site" 
humpbackwhale = humpbackwhale[!is.na(humpbackwhale$DateFs),]
humpbackwhale = humpbackwhale[!duplicated(humpbackwhale)]
dIdx = humpbackwhale$ISOStartTime[duplicated(humpbackwhale$ISOStartTime) ] 
for (dd in 1 :length(dIdx)) {humpbackwhale$humpbackwhaleP[ humpbackwhale$ISOStartTime == dIdx[dd]] = 1}
humpbackwhale = humpbackwhale[!duplicated(humpbackwhale)]

#NORTH ATLANTIC RIGHT WHALE-- presence in a given day [0 or 1]
#-----------------------------------------------------------------------------------------
nFiles = length( list.files(path=dir2,pattern = "northatlanticrightwhale", full.names=TRUE, recursive = TRUE))
northatlanticrightwhale = NULL
for (ff in 1 : nFiles){
  fname  = list.files(path=dir2, pattern = "northatlanticrightwhale", full.names=FALSE, recursive = TRUE)[ff]
  fnameF = list.files(path=dir2, pattern = "northatlanticrightwhale", full.names=TRUE, recursive = TRUE)[ff]
  dname = sapply (strsplit( fname, "_" ),"[",3 )
  tst  =  sapply (strsplit( fname, "_" ),"[",5 )
  if(tst == "northatlanticrightwhale"){
    cat("Processing...", fname,"\n")
    tmp  = fread(fnameF,header=F) #remove these after
    tmp$Site = dname
    northatlanticrightwhale = rbind(northatlanticrightwhale,tmp)
    rm(fname,fnameF,tmp)
  }
}
colnames(northatlanticrightwhale)= c( as.character(northatlanticrightwhale[1,1:3]) )
northatlanticrightwhale = northatlanticrightwhale[northatlanticrightwhale$Presence != "Presence",] #remove headers in the data frame
northatlanticrightwhale = northatlanticrightwhale[northatlanticrightwhale$ISOStartTime != "",] #remove headers in the data frame
northatlanticrightwhale$DateFs   = as.Date( northatlanticrightwhale$ISOStartTim,tz="GMT")
names(northatlanticrightwhale)[2] = "northatlanticrightwhaleP" 
names(northatlanticrightwhale)[3] = "Site" 
northatlanticrightwhale = northatlanticrightwhale[!is.na(northatlanticrightwhale$DateFs),]
northatlanticrightwhale = northatlanticrightwhale[!duplicated(northatlanticrightwhale)]
dIdx = northatlanticrightwhale$ISOStartTime[duplicated(northatlanticrightwhale$ISOStartTime) ] 
for (dd in 1 :length(dIdx)) {northatlanticrightwhale$northatlanticrightwhaleP[ northatlanticrightwhale$ISOStartTime == dIdx[dd]] = 1}
northatlanticrightwhale = northatlanticrightwhale[!duplicated(northatlanticrightwhale)]

#SEI WHALE-- presence in a given day [0 or 1]
#-----------------------------------------------------------------------------------------
nFiles = length( list.files(path=dir2,pattern = "seiwhale", full.names=TRUE, recursive = TRUE))
seiwhale = NULL
for (ff in 1 : nFiles){
  fname  = list.files(path=dir2, pattern = "seiwhale", full.names=FALSE, recursive = TRUE)[ff]
  fnameF = list.files(path=dir2, pattern = "seiwhale", full.names=TRUE, recursive = TRUE)[ff]
  dname = sapply (strsplit( fname, "_" ),"[",3 )
  tst  =  sapply (strsplit( fname, "_" ),"[",5 )
  if(tst == "seiwhale"){
    cat("Processing...", fname,"\n")
    tmp  = fread(fnameF,header=F) #remove these after
    tmp$Site = dname
    seiwhale = rbind(seiwhale,tmp)
    rm(fname,fnameF,tmp)
  }
}
colnames(seiwhale)= c( as.character(seiwhale[1,1:3]) )
seiwhale = seiwhale[seiwhale$Presence != "Presence",] #remove headers in the data frame
seiwhale = seiwhale[seiwhale$ISOStartTime != "",] #remove headers in the data frame
seiwhale$DateFs   = as.Date( seiwhale$ISOStartTim,tz="GMT")
names(seiwhale)[2] = "seiwhaleP" 
names(seiwhale)[3] = "Site" 
seiwhale = seiwhale[!is.na(seiwhale$DateFs),]
seiwhale = seiwhale[!duplicated(seiwhale)]
dIdx = seiwhale$ISOStartTime[duplicated(seiwhale$ISOStartTime) ] 
for (dd in 1 :length(dIdx)) {seiwhale$seiwhaleP[ seiwhale$ISOStartTime == dIdx[dd]] = 1}
seiwhale = seiwhale[!duplicated(seiwhale)]

#-----------------------------------------------------------------------------------------
# MERGE all the biologics into one matrix
#-----------------------------------------------------------------------------------------
#start with all days from input data frame
idataD$DateFs = as.Date(idataD$Day)
#bluewhale, finwhale, humpbackwhale, northatlanticrightwhale,  seiwhale
#nrows should always be same as idataD... 750, dupliates (fixed above!)
b0 = merge(idataD, bluewhale, by ="DateFs",  all.x=TRUE)
b1 = merge(b0,      finwhale, by = "DateFs", all.x=TRUE)
b1 = b1[,c(1:32,34,37)]
b2 = merge(b1, humpbackwhale, by = "DateFs", all.x=TRUE)
b2 = b2[,c(1:34,36)]
b3 = merge(b2, northatlanticrightwhale, by = "DateFs", all.x=TRUE)
b3 = b3[,c(1:35,37)]
b4 = merge(b3, seiwhale, by = "DateFs", all.x=TRUE)
b4 = b4[,c(1:36,38)]
bio = merge(b4,dolphinsD, by="DateFs", all.x=TRUE)

bio$bluewhaleP = as.numeric(as.character(bio$bluewhaleP ))
bio$finwhaleP = as.numeric(as.character(bio$finwhaleP ))
bio$humpbackwhaleP = as.numeric(as.character(bio$humpbackwhaleP ))
bio$northatlanticrightwhaleP = as.numeric(as.character(bio$northatlanticrightwhaleP ))
bio$seiwhaleP = as.numeric(as.character(bio$seiwhaleP ))
bio$DolphinHrs = as.numeric(as.character(bio$DolphinHrs ))
bio$dolphinsDetDay = as.numeric(as.character(bio$dolphinsDetDay ))
as.data.frame( colnames(bio) )

bioAll = merge(bio,atlanticcodD, by="DateFs", all.x=TRUE)
rm(numDays,udays,b0,b1,b2,b3,b4,bio,dolphins,dolphinsD,humpbackwhale,bluewhale,finwhale,seiwhale,northatlanticrightwhale,atlanticcodD,atlanticcod, dd,ff,tst)

#-----------------------------------------------------------------------------------------
# COMBINE ABIOTIC HOURLY SUMMARIES with SD values
#-----------------------------------------------------------------------------------------
udays = unique(as.Date( iData$Day) )
#iData$avgWSPD = as.numeric( as.character(iData$avgWSPD))
#iData$avgWSPD[iData$avgWSPD == 99] = NA
#hist( iData$avgWSPD )

abioD = NULL
#this loop takes a bit... not sure why-- >80 days!
for(dd in 1:length(udays)){
  tmp   = iData[ as.Date( iData$Day) == udays[dd],]
  ck = all( is.na(tmp$changeWL) )
  if(ck == TRUE){
    abioD = rbind(abioD, c(as.character(udays[dd]), nrow(tmp), 
                           mean(tmp$avgWSPD,na.rm = T),       sd(tmp$avgWSPD,na.rm = T), 
                           NA, NA ) )
  } else {
  abioD = rbind(abioD, c(as.character(udays[dd]), nrow(tmp), 
                         mean(tmp$avgWSPD,na.rm = T),       sd(tmp$avgWSPD,na.rm = T), 
                         mean(abs(tmp$changeWL),na.rm = T), max(abs(tmp$changeWL),na.rm = T) ) )
  }
}
colnames(abioD) = c("DateFs","HrsSampled","meanWSPD","sdWSPD","meanTide","maxTide")
abioD = as.data.frame(abioD)
abioD$DateFs   = as.Date(abioD$DateFs)
abioD$meanWSPD = as.numeric(as.character( abioD$meanWSPD) )
abioD$sdWSPD   = as.numeric(as.character( abioD$sdWSPD) )
abioD$meanTide = as.numeric(as.character( abioD$meanTide) )
abioD$maxTide  = as.numeric(as.character( abioD$maxTide) )

dataA = merge(bioAll,abioD, by="DateFs", all.x=TRUE)
as.data.frame(colnames(dataA))
summary(dataA)
dataA$Yr = year(dataA$DateFs)
dataA$JulianDay = yday(dataA$DateFs)
dataA$Mth = month(dataA$DateFs)
dataA$DayW= weekdays(dataA$DateFs)
summary(dataA)

#-----------------------------------------------------------------------------------------
#ADD daily information to hourly--- just repeat the measure for each hour
#-----------------------------------------------------------------------------------------
iData$Day = as.Date(iData$Day)
as.data.frame(colnames(dataA))
dataAhr = NULL
for(ii in 1:nrow(dataA)){
  tdata = iData[iData$Day == as.Date(dataA$Day[ii]),] #houly data for the day
  dataA[ii, c(23:37,41)]
  dataAhr = rbind( dataAhr, cbind(tdata, dataA[ii, c(23:37,41)]) )
}
dataAhr= as.data.frame(dataAhr)
summary(dataAhr)

#-----------------------------------------------------------------------------------------
#SAVE OUT combined matrix
#-----------------------------------------------------------------------------------------
setwd(outDir)
DC = Sys.Date()
fnameAll =     paste0(as.character( dataA$Site[1] ), "_CombinedData_SPLShipsAbioticBio_Day_",
                      as.character(min(as.Date(dataA$Day))),"_",
                      as.character(max(as.Date(dataA$Day))), "_v", DC, ".csv")  
write.csv(dataA,fnameAll) 
fnameAll2 =     paste0(as.character( dataA$Site[1] ), "_CombinedData_SPLShipsAbioticBio_Day_",
                      as.character(min(as.Date(dataA$Day))),"_",
                      as.character(max(as.Date(dataA$Day))), "_v", DC)  
save(dataA,file =fnameAll2 )
#hourly data
fnameAll =     paste0(as.character( dataAhr$Site[1] ), "_CombinedData_SPLShipsAbioticBio_HR_",
                      as.character(min(as.Date(dataA$Day))),"_",
                      as.character(max(as.Date(dataA$Day))), "_v", DC, ".csv")  
write.csv(dataAhr,fnameAll) 
nameAll2 =     paste0(as.character( dataAhr$Site[1] ), "_CombinedData_SPLShipsAbioticBio_HR_",
                     as.character(min(as.Date(dataA$Day))),"_",
                     as.character(max(as.Date(dataA$Day))), "_v", DC) 
save(dataAhr,file =nameAll2 )

#-----------------------------------------------------------------------------------------
# (optional) DATA EXPLORATION PLOTS... now that it is all combined
# Copied to 4_combineFiles_plots_SB02.R

# #-----------------------------------------------------------------------------------------
# #select days in March
# #-----------------------------------------------------------------------------------------
# tmp = dataA[dataA$JulianDay > 82 & dataA$JulianDay < 91,]
# as.data.frame(colnames(tmp))
# ggplot(tmp, aes(as.factor(JulianDay), meanWSPD, group=as.factor(Yr), color = as.factor(Yr))) +
#   geom_line() +
#   xlab("")+
#   ylab("Wind Speed (mps) ") +
#   ggtitle(("SB02: March 25-31 [2019=5.8, 2020=7.1]"))+
#   theme_minimal()
# 
# tmp %>% group_by(Yr) %>% summarize(newvar = mean(meanWSPD,na.rm = T))
# 
# #TILE PLOT of all sources present...
# BioAllomelt = reshape :: melt(tmp, id.vars = "DateFs", 
#                               measure.vars = c("bluewhaleP","finwhaleP","humpbackwhaleP",
#                                                "northatlanticrightwhaleP","seiwhaleP", 
#                                                "dolphinsDetDay","atlanticcodDetDay" ))
# BioAllomelt$value = as.numeric(as.character(BioAllomelt$value) )
# 
# ggplot(BioAllomelt, aes(DateFs, variable, fill= (value))) + 
#   geom_tile() +
#   scale_fill_gradient(low="white", high="blue") +
#   labs(title = paste0(site,": summary of sounds"),  fill = "Detections") +
#   xlab("") +
#   ylab("")
# 
# #plot of conditions
# pwind = ggplot(tmp, aes(as.factor(JulianDay), meanWSPD, group=as.factor(Yr), color = as.factor(Yr))) +
#   geom_line() +   geom_point() +
#   xlab("Julian Day")+  ylab("Wind Speed (mps) ") +
#   theme_minimal()+ theme(legend.position = "none")
# 
# ptide = ggplot(tmp, aes(as.factor(JulianDay), maxTide, group=as.factor(Yr), color = as.factor(Yr))) +
#   geom_line() +
#   geom_point() +
#   xlab("")+  ylab("Max tide change ") +
#   theme_minimal()+
#   theme(legend.position = "none")
# 
# p125 = ggplot(tmp, aes(as.factor(JulianDay), OL_250, group=as.factor(Yr), color = as.factor(Yr))) +
#   geom_line() +
#   geom_point() +
#   xlab("")+  ylab("Ol_125 ") +
#   theme_minimal()+
#   theme(legend.position = "none")
# 
# p63 = ggplot(tmp, aes(as.factor(JulianDay), OL_63, group=as.factor(Yr), color = as.factor(Yr))) +
#   geom_line() +
#   geom_point() +
#   xlab("")+  ylab("Ol_63 ") +
#   theme_minimal()+
#   theme(legend.position = "none")
# 
# BioAllomelt$JulianDay = yday(BioAllomelt$DateFs)
# BioAllomelt$Yr = year(BioAllomelt$DateFs)
# #plot of sources by year, source=x,Y=value
# pBio = ggplot(BioAllomelt, aes(variable, value, fill=as.factor(Yr)) ) +
#   geom_bar(position = "fill",stat = "identity" )+
#   xlab("")+ ylab("")+
#   scale_x_discrete(labels = c("Blue","Fin","Humpback","NARW", 
#                               "Sei","Dolphin","Cod"))+
#   scale_fill_discrete(name = "")+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 50),legend.position="top" )
# uSources = unique(BioAllomelt$variable)
# BioAllomelt[BioAllomelt$variable == uSources[1],]
# BioAllomelt[BioAllomelt$variable == uSources[2],] #everyday had 1
# BioAllomelt[BioAllomelt$variable == uSources[3],] #one more day 2020 with humpbacks
# BioAllomelt[BioAllomelt$variable == uSources[4],] #2 day 2019, 7 day in 2020
# BioAllomelt[BioAllomelt$variable == uSources[5],] #one more day 2020 with sei
# BioAllomelt[BioAllomelt$variable == uSources[6],] #no data for dolphins/cod
# BioAllomelt[BioAllomelt$variable == uSources[7],] 
# 
# grid.arrange(p125,p63,pwind,ptide,pBio,ncol = 5)
# 
# #boxplots
# pwind = ggplot(tmp, aes(as.factor(Yr), meanWSPD, fill = as.factor(Yr))) +
#   geom_boxplot() +
#   xlab("Julian Day")+  ylab("Wind Speed (mps) ") +
#   theme_minimal()+ theme(legend.position = "none")
# 
# ptide = ggplot(tmp, aes(as.factor(Yr), maxTide, fill = as.factor(Yr))) +
#   geom_boxplot() +
#   xlab("")+  ylab("Max tide change ") +
#   theme_minimal()+   theme(legend.position = "none")
# 
# p125 = ggplot(tmp, aes(as.factor(Yr), OL_250, fill = as.factor(Yr))) +
#   geom_boxplot() +
#   xlab("")+  ylab("Ol_125 ") +
#   theme_minimal()+   theme(legend.position = "none")
# 
# p63 = ggplot(tmp, aes(as.factor(Yr), OL_63, fill = as.factor(Yr))) +
#   geom_boxplot() +
#   xlab("")+  ylab("Ol_63 ") +
#   theme_minimal()+   theme(legend.position = "none")
# 
# #VESSEL Metrics
# pDetVes = ggplot(tmp, aes(as.factor(Yr), PercentDay_TimeVesselDet, fill = as.factor(Yr))) +
#   geom_boxplot() +
#   xlab("")+  ylab("% day with vessel noise (Detector) ") +
#   theme_minimal()+   theme(legend.position = "none")
# 
# pAISa = ggplot(tmp, aes(as.factor(Yr), LOA_ALL_UV, fill = as.factor(Yr))) +
#   geom_boxplot() +
#   xlab("")+  ylab("Unique Vessels (AIS) ") +
#   theme_minimal()+   theme(legend.position = "none")
# pAISs = ggplot(tmp, aes(as.factor(Yr), LOA_S_UV, fill = as.factor(Yr))) +
#   geom_boxplot() +
#   xlab("")+  ylab("Small Unique Vessels (AIS) ") +
#   theme_minimal()+   theme(legend.position = "none")
# 
# pAISm = ggplot(tmp, aes(as.factor(Yr), LOA_M_UV, fill = as.factor(Yr))) +
#   geom_boxplot() +
#   xlab("")+  ylab("Medium Unique Vessels (AIS) ") +
#   theme_minimal()+   theme(legend.position = "none")
# 
# pAISl = ggplot(tmp, aes(as.factor(Yr), LOA_L_UV, fill = as.factor(Yr))) +
#   geom_boxplot() +
#   xlab("")+  ylab("Large Unique Vessels (AIS) ") +
#   theme_minimal()+   theme(legend.position = "none")
# 
# grid.arrange(p125,p63,pwind,ptide,pBio,ncol = 5)
# grid.arrange(pDetVes,pAISa,pAISs,pAISm,pAISl,ncol = 5)
# 
# #-----------------------------------------------------------------------------------------
# #monthly difference graphics
# #-----------------------------------------------------------------------------------------
# as.data.frame(colnames(dataA))
# #boxplot of monthly differences
# #sound level- 125Hz
# p125 = ggplot(dataA, aes(as.factor(Mth), OL_125, fill = as.factor(Yr))) +
#   geom_boxplot(outlier.colour="black", outlier.size =.2, outlier.alpha = .2) +
#   theme_minimal() +
#   xlab("") +   ylab ("") + 
#   ggtitle("Low frequency sound levels [125 Hz]")+ #median [125 Hz]
#   #coord_cartesian(ylim = c(65, 120)) +
#   scale_fill_manual(values=c("gray","#E69F00", "#56B4E9") ) +
#   theme(axis.text        = element_text(size=11),
#         panel.background = element_rect(fill = "transparent"),
#         plot.caption     = element_text(color = "black", face = "italic",hjust = 0),
#         axis.title       = element_text(size=11,face="bold"),
#         plot.background  = element_rect(fill = "transparent", color = NA),
#         #plot.margin      = unit(c(1,2,2,2), "lines"),
#         legend.position = "none",
#         legend.title = element_blank() )
# #vessel noise 
# pDet = ggplot(dataA, aes(as.factor(Mth), PercentDay_TimeVesselDet, fill = as.factor(Yr))) +
#   geom_boxplot(outlier.colour="black", outlier.size =.2, outlier.alpha = .2) +
#   theme_minimal() +
#   xlab("") +   ylab ("") + 
#   ggtitle("% Day vessel noise")+ #median [125 Hz]
#   #coord_cartesian(ylim = c(65, 120)) +
#   scale_fill_manual(values=c("gray","#E69F00", "#56B4E9") ) +
#   theme(axis.text        = element_text(size=11),
#         panel.background = element_rect(fill = "transparent"),
#         plot.caption     = element_text(color = "black", face = "italic",hjust = 0),
#         axis.title       = element_text(size=11,face="bold"),
#         plot.background  = element_rect(fill = "transparent", color = NA),
#         #plot.margin      = unit(c(1,2,2,2), "lines"),
#         legend.position = "none",
#         legend.title = element_blank() )
# 
# pAIS = ggplot(dataA, aes(as.factor(Mth), LOA_ALL_UV, fill = as.factor(Yr))) +
#   geom_boxplot(outlier.colour="black", outlier.size =.2, outlier.alpha = .2) +
#   theme_minimal() +
#   xlab("") +   ylab ("") + 
#   ggtitle("Unique AIS vessels")+ #median [125 Hz]
#   #coord_cartesian(ylim = c(65, 120)) +
#   scale_fill_manual(values=c("gray","#E69F00", "#56B4E9") ) +
#   theme(axis.text        = element_text(size=11),
#         panel.background = element_rect(fill = "transparent"),
#         plot.caption     = element_text(color = "black", face = "italic",hjust = 0),
#         axis.title       = element_text(size=11,face="bold"),
#         plot.background  = element_rect(fill = "transparent", color = NA),
#         #plot.margin      = unit(c(1,2,2,2), "lines"),
#         legend.position = "none",
#         legend.title = element_blank() )
# 
# pWind = ggplot(dataA, aes(as.factor(Mth), meanWSPD, fill = as.factor(Yr))) +
#   geom_boxplot(outlier.colour="black", outlier.size =.2, outlier.alpha = .2) +
#   theme_minimal() +
#   xlab("") +   ylab ("") + 
#   ggtitle("Wind speed ")+ #median [125 Hz]
#   #coord_cartesian(ylim = c(65, 120)) +
#   scale_fill_manual(values=c("gray","#E69F00", "#56B4E9") ) +
#   theme(axis.text        = element_text(size=11),
#         panel.background = element_rect(fill = "transparent"),
#         plot.caption     = element_text(color = "black", face = "italic",hjust = 0),
#         axis.title       = element_text(size=11,face="bold"),
#         plot.background  = element_rect(fill = "transparent", color = NA),
#         #plot.margin      = unit(c(1,2,2,2), "lines"),
#         legend.position = "bottom",
#         legend.title = element_blank() )
# #biology-- bar?
# BioAllomeltA = reshape :: melt(dataA, id.vars = "DateFs", 
#                               measure.vars = c("bluewhaleP","finwhaleP","humpbackwhaleP",
#                                                "northatlanticrightwhaleP","seiwhaleP" ))
# BioAllomeltA$value = as.numeric(as.character(BioAllomeltA$value) )
# BioAllomeltA$Yr  = year( as.Date(BioAllomeltA$DateFs ))
# BioAllomeltA$Mth = month( as.Date(BioAllomeltA$DateFs ))
# BioAllomeltA = BioAllomeltA[ BioAllomeltA$Yr > 2018,]
# 
# #values for each factor (month) to be summed up within each bar
# BioAllomeltA %>% 
#   group_by(variable, Yr, Mth) %>% 
#   summarize(summedvalues = sum(value,na.rm = T)) %>% 
#   ggplot(aes(as.factor(Mth), summedvalues, fill=as.factor(Yr)) ) +
#   geom_bar(position = "dodge", stat = "identity",width = .5)+
#   scale_fill_manual(values=c("#E69F00", "#56B4E9") )+
#   facet_wrap(~variable, ncol=1)+
#   xlab("")+ ylab("Days with calls")+
#   theme(legend.title = element_blank())
# 
# ggplot(BioAllomeltA, aes(as.factor(Mth), value, fill=as.factor(Yr)) ) +
#   geom_bar(position = "dodge", stat = "identity",width = .5)+
#   scale_fill_manual(values=c("#E69F00", "#56B4E9") )+
#   facet_wrap(~variable, ncol=1)+
#   xlab("")+ ylab("Days with calls")+
#   theme(legend.title = element_blank())
# 
# 
# grid.arrange(p125,pDet,pAIS,pWind,ncol = 1)
# #Difference MONTH COMPARISION--- 2019 vs 2020, heat map
# #-----------------------------------------------------------------------------------------
# as.data.frame(colnames(tmp))
# SPLAll1 = dataA[dataA$Yr == 2019 | dataA$Yr == 2020, ]
# SPLAll1$Bal  = rowSums ( SPLAll1[,33:37]) #sum of baleen whale calls present in given day
# SPLAll1$BalP = 0 
# SPLAll1$BalP[SPLAll1$Bal> 0] = 1
# #summarize by year, month as median values or mean
# by125 = as.data.frame ( SPLAll1 %>%
#                           group_by(Mth,Yr) %>%
#                           summarise(qs125 = quantile(OL_125, .5), nDays = n() ) )
# diff125 = NULL
# for(mm in 1:12){
#   tmp1 = by125[by125$Mth == mm,]
#   if (nrow( tmp1) == 0 ) { 
#     diff125 = rbind(diff125, c( mm, NA,NA,NA)) #no date for site/month
#   }else if (nrow( tmp1) == 1 ){ 
#     diff125 = rbind(diff125, c( mm, NA,NA,NA)) #not data for both years
#   }else if (nrow(tmp1) == 2) {
#     
#     d2019 = tmp1$qs125[tmp1$Yr == 2019]
#     d2020 = tmp1$qs125[tmp1$Yr == 2020]
#     diff125 = rbind(diff125, c(mm, (d2020 - d2019), d2019, d2020 ) ) #not data for both years
#   }
# }
# colnames(diff125) = c("Mth","Diff_125","2019_125","2020_125")
# diff125 = as.data.frame(diff125)
# diff125$Mth = ordered(diff125$Mth, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11","12"))
# 
# #OL_63
# by63= as.data.frame ( SPLAll1 %>%
#                           group_by(Mth,Yr) %>%
#                           summarise(qs63 = quantile(OL_63, .5), nDays = n() ) )
# diff63 = NULL
# for(mm in 1:12){
#   tmp1 = by63[by63$Mth == mm,]
#   if (nrow( tmp1) == 0 ) { 
#     diff63 = rbind(diff63, c( mm, NA,NA,NA)) #no date for site/month
#   }else if (nrow( tmp1) == 1 ){ 
#     diff63 = rbind(diff63, c( mm, NA,NA,NA)) #not data for both years
#   }else if (nrow(tmp1) == 2) {
#     
#     d2019 = tmp1$qs63[tmp1$Yr == 2019]
#     d2020 = tmp1$qs63[tmp1$Yr == 2020]
#     diff63 = rbind(diff63, c(mm, (d2020 - d2019), d2019, d2020 ) ) #not data for both years
#   }
# }
# colnames(diff63) = c("Mth","Diff_63","2019_63","2020_63")
# diff63 = as.data.frame(diff63)
# 
# #WIND
# byWind = as.data.frame ( SPLAll1 %>%
#                           group_by(Mth,Yr) %>%
#                           summarise(avgWSPD = mean(meanWSPD,na.rm = T), sdWSPD = sd(meanWSPD,na.rm = T), nDays = n() ) )
# 
# diffW = NULL
# for(mm in 1:12){
#   tmp1 = byWind[byWind$Mth == mm,]
#   if (nrow( tmp1) == 0 ) { 
#     diffW = rbind(diffW, c( mm, NA,NA,NA)) #no date for site/month
#   }else if (nrow( tmp1) == 1 ){ 
#     diffW = rbind(diffW, c( mm, NA,NA,NA)) #not data for both years
#   }else if (nrow(tmp1) == 2) {
#     
#     d2019 = tmp1$avgWSPD[tmp1$Yr == 2019]
#     d2020 = tmp1$avgWSPD[tmp1$Yr == 2020]
#     diffW = rbind(diffW, c(mm, (d2020 - d2019), d2019, d2020 ) ) #not data for both years
#   }
# }
# colnames(diffW) = c("Mth","Diff_WIND","2019_WIND","2020_WIND")
# diffW = as.data.frame(diffW)
# 
# #days with baleen calls present
# byBio = as.data.frame ( SPLAll1 %>%
#                         group_by(Mth,Yr) %>%
#                         summarise(daysBio = sum(BalP,na.rm = T), nDays = n() ) )
# diffB = NULL
# for(mm in 1:12){
#   tmp1 = byBio[byBio$Mth == mm,]
#   if (nrow( tmp1) == 0 ) { 
#     diffB = rbind(diffB, c( mm, NA,NA,NA)) #no date for site/month
#   }else if (nrow( tmp1) == 1 ){ 
#     diffB = rbind(diffB, c( mm, NA,NA,NA)) #not data for both years
#   }else if (nrow(tmp1) == 2) {
#     
#     d2019 = tmp1$daysBio[tmp1$Yr == 2019]
#     d2020 = tmp1$daysBio[tmp1$Yr == 2020]
#     diffB = rbind(diffB, c(mm, (d2020 - d2019), d2019, d2020 ) ) #not data for both years
#   }
# }
# colnames(diffB) = c("Mth","Diff_BioDay","2019_BioDay","2020_BioDay")
# diffB = as.data.frame(diffB)
# 
# 
# byBioC = as.data.frame ( SPLAll1 %>%
#                           group_by(Mth,Yr) %>%
#                           summarise(countBio = sum(Bal,na.rm = T), nDays = n() ) )
# diffBc = NULL
# for(mm in 1:12){
#   tmp1 = byBioC[byBioC$Mth == mm,]
#   if (nrow( tmp1) == 0 ) { 
#     diffBc = rbind(diffBc, c( mm, NA,NA,NA)) #no date for site/month
#   }else if (nrow( tmp1) == 1 ){ 
#     diffBc = rbind(diffBc, c( mm, NA,NA,NA)) #not data for both years
#   }else if (nrow(tmp1) == 2) {
#     
#     d2019 = tmp1$countBio[tmp1$Yr == 2019]
#     d2020 = tmp1$countBio[tmp1$Yr == 2020]
#     diffBc = rbind(diffBc, c(mm, (d2020 - d2019), d2019, d2020 ) ) #not data for both years
#   }
# }
# colnames(diffBc) = c("Mth","Diff_BioC","2019_BioC","2020_BioC")
# diffBc = as.data.frame(diffBc)
# 
# # combine all metrics
# #-----------------------------------------------------------------------------------------
# diffALL = cbind(diff125,diff63[,2:4],diffW[,2:4],diffB[,2:4],diffBc[,2:4] )
# diffALLmelt = reshape :: melt(diffALL, id.vars = "Mth", 
#                               measure.vars = c("Diff_125","Diff_63","Diff_WIND","Diff_BioDay","Diff_BioC" ))
# 
# #PLOT: x = month, y = metric, fill is difference
# #-----------------------------------------------------------------------------------------
# #positive is higher in 2020, negative in lower in 2020
# ggplot(diffALLmelt, aes(Mth, variable ,fill=as.numeric(as.character(value)) ) ) +
#   geom_tile()+
#   xlab("Month") + ylab("")+
#   scale_fill_gradient2(low="blue", high="orange")+
#   scale_y_discrete(labels = c("125 Hz", "63 Hz", "Wind", "Baleen whale [days]","Baleen whale [calls]"))+
#   scale_x_discrete(labels = c("Jan", "Feb", "Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec"))+
#   ggtitle("Change in soundscape features (+ higher 2020, - lower 2020)")+
#   theme(legend.title = element_blank())

# process sanctuary sound vessel detection data and AIS results

#-----------------------------------------------------------------------------------------
# MAIN functions-- by site processing
#-----------------------------------------------------------------------------------------
# Reads in both vessel detection and AIS data products
# Calculates daily vessel metrics per day:
# Converts vessel detection files: for each each hour, how many unique vessels detected, and percent of hour dominated by vessel noise
# some of the time will spill into the next hour, need to keep track of this
# reformat so for each hour there is a total vessel time value and total vessel count

#outputs 
# a per site .csv file: E:\RESEARCH\SanctSound\data2\combineFiles1_SPLShips
# summary table for average of all data per site (copied here):
# https://docs.google.com/spreadsheets/d/1dw9Vy0dXKW3Q6Ter3t19cmLf_5EJWbmL9nqJsCrNXQE/edit#gid=888829761

#-----------------------------------------------------------------------------------------
# TO ADD (future versions)
#-----------------------------------------------------------------------------------------
# output table: add range and save out
# duration of quiet- hour/daily events and average duration s, using vessel detection data
# check for vessel detection data-- if not available need to reflect this in data

rm(list=ls())
#-----------------------------------------------------------------------------------------
# LOAD Libraries
#-----------------------------------------------------------------------------------------
library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)
library(scales)
library(BBmisc)
library(zoo)
library(plotly)  #https://www.r-graph-gallery.com/interactive-charts.html

#-----------------------------------------------------------------------------------------
# SETUP parameters
#-----------------------------------------------------------------------------------------
ra = 7 #days for running average
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
stdize = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

#-----------------------------------------------------------------------------------------
# OUTPUT details
#-----------------------------------------------------------------------------------------
tDir = "E:\\RESEARCH\\SanctSound\\"
outDir =  paste0(tDir,"data2\\combineFiles1_SPLShips\\")
DC = Sys.Date()

#range of dates for output graphics
eDatePlot = '2020-12-31'
sDatePlot = '2018-11-01'
flagCSV = TRUE  #true if you want to output hourly and daily csv files per site
flagPLT = FALSE #true if you want to output summary plots of data

#-----------------------------------------------------------------------------------------
# READ IN-- vessel detection data
#-----------------------------------------------------------------------------------------
dirVD     = (paste0(tDir,"data2\\SanctSound_VesselDetection_DataProducts") )
nFilesVD  = length( list.files(path=dirVD, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
inFilesVD =     ( list.files(path=dirVD, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
inFilesVD = basename(inFilesVD)
x = strsplit(inFilesVD,"_")
sitesVD = unique(sapply( x, "[", 2 ))

cat(nFilesVD, " files: ", length(sitesVD), " number of sites with vessel detection data", "\n")
rm(x)

#-----------------------------------------------------------------------------------------
# READS IN-- AIS data
#-----------------------------------------------------------------------------------------
dirAIS      = paste0(tDir,"data")
nFilesAIS   = length( list.files(path=dirAIS,pattern = "_2018_10_to_2020_11.csv", full.names=TRUE, recursive = TRUE))
inFilesAIS  =       ( list.files(path=dirAIS,pattern = "_2018_10_to_2020_11.csv", full.names=TRUE, recursive = TRUE))
x = strsplit(inFilesAIS,"_")
sanctsAIS = unique(sapply( x, "[", 2 ))
#combine AIS files
multmerge = function(path){
  filenames = list.files(path=path, pattern = "_2018_10_to_2020_11.csv", full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
}
AIS <- multmerge(dirAIS)
sitesAIS = unique(AIS$LOC_ID)
cat(length(sitesAIS), " number of sites with AIS data", "\n")
rm(x)

#-----------------------------------------------------------------------------------------
# PROCESS and COMBINE FILES by site, reads in SPL date when processing each site
#-----------------------------------------------------------------------------------------
output = NULL
output2 = NULL #truncate to a give time period
output3 = NULL

for (ss in 1:length(sitesVD) ) { #loop through each site length(sitesVD), ss = 25 (SB02) 
  
  cat("Processing site...", sitesVD[ss], ":",ss, "of", length(sitesVD),"\n")
  
  sFiles = list.files(path=dirVD, pattern = paste0(sitesVD[ss],".*hips.csv"), full.names=TRUE, recursive = TRUE)
  aData  = AIS[ AIS$LOC_ID == sitesVD[ss], ]
  aData$Day = as.Date(aData$DATE,format = "%m/%d/%Y")#AIS is daily resolution
  deply = sitesVD[ss]
  sanct = substr(sitesVD[ss],1,2)
  
  #----------------------------------------------------------------------------------------- 
  #GET SPL files-- this give us accurate date range for detections
  #---------------------------------------------------------------------------------------
  dirSPL  = paste0(tDir,"data\\",sanct,"\\", deply)
  nFiles  = length( list.files(path=dirSPL, pattern = "_OL_1h.csv", full.names=TRUE, recursive = TRUE))
  
  #CHECK: are there OL SPL files??
  if (nFiles == 0 ){
    cat("No SPL files for ", sitesVD[ss], "\n")
    #UPDATE FOR SITE WITH NO SPL DATA!!
    output = rbind(output, c(sitesVD[ss], length(sFiles), nFiles, 
                             NA, NA,NA,NA,NA, NA,NA,NA,NA, NA,NA,NA,NA, NA,NA,NA,NA, NA,NA,NA,NA, NA,NA,NA,NA,NA,NA ) )
    next
  }else { 
    cat(nFiles, "SPL files for ",sitesVD[ss],"\n") }
  
  #COMBINE ALL OL DATA TOGETHER, add deployment
  SPL = NULL
  for (ff in 1 : nFiles){
    fname  = list.files(path=dirSPL, pattern = "_OL_1h.csv", full.names=FALSE, recursive = TRUE)[ff]
    fnameF = list.files(path=dirSPL, pattern = "_OL_1h.csv", full.names=TRUE, recursive = TRUE)[ff]
    dname  = sapply (strsplit( fname, "_" ),"[",4 )
    tmp    = rbind(fread(fnameF))
    tmp$deploy = dname
    if (ff > 1) { names(tmp) <- NULL }
    
    SPL = rbind(SPL,tmp)
    rm(fname,fnameF,tmp)
  }
  
  #CHECK: fix format changes in SPL data headings
  SPL$DateF = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPL$`yyyy-mm-ddTHH:MM:SSZ`)), tz = "GMT" )
  SPL$Day   = as.Date(SPL$DateF)
  if ( is.na( SPL$DateF [1]) ){#try another format!!
    SPL$DateF = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPL$yyyy_mm_ddTHH_MM_SSZ)), tz = "GMT" )
    SPL$Day   = as.Date(SPL$DateF)
  }
  
  #CHECK: fix some sites have different OB-- SB01 for sure!! starts at 16 Hz
  #cat( sitesVD[ss], ":", colnames(SPL), "\n")
  if (sitesVD[ss]== "SB01") {
    cat("SPL data has extra column(s)... check!", "\n")
    SPL = SPL[,c(1,3:15)]
  }
  if (sitesVD[ss]== "SB02") {
    cat("SPL data has extra column(s)... check!", "\n")
    SPL = SPL[,c(1,3:15)]
  }
  if (sitesVD[ss]== "SB03") {
    cat("SPL data has extra column(s)... check!", "\n")
    SPL = SPL[,c(1,3:15)]
  }
  cat( sitesVD[ss], ":", colnames(SPL), "\n")

  #APPEND BB data to OL
  nFiles = length( list.files(path=dirSPL,pattern = "_BB_1h.csv", full.names=TRUE, recursive = TRUE))
  SPLBB = NULL
  for (ff in 1 : nFiles){
    fname  = list.files(path=dirSPL, pattern = "_BB_1h.csv", full.names=FALSE, recursive = TRUE)[ff]
    fnameF = list.files(path=dirSPL, pattern = "_BB_1h.csv", full.names=TRUE, recursive = TRUE)[ff]
    dname  = sapply (strsplit( fname, "_" ),"[",4 )
    tmp    = rbind(fread(fnameF))
    tmp$deploy = dname
    if (ff > 1) { names(tmp) <- NULL }
    
    SPLBB = rbind(SPLBB,tmp)
    rm(fname,fnameF,tmp)
  }
  
  #GRRR DIFFERENT HEADING FORMATS!!!
  colcheck = colnames(SPLBB)
  if (colcheck[1] == "yyyy_mm_ddTHH_MM_SSZ" ){
    #SPLBB$yyyy_mm_ddTHH_MM_SSZ
    SPLBB$DateF     = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPLBB$`yyyy_mm_ddTHH_MM_SSZ`)), tz = "GMT" )
    SPLBB$DateFday  = as.Date(SPL$DateF)
    
  }else if (colcheck[1] == "yyyy-mm-ddTHH:MM:SSZ" ){
    #SPLBB$`yyyy-mm-ddTHH:MM:SSZ`
    SPLBB$DateF     = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPLBB$`yyyy-mm-ddTHH:MM:SSZ`)), tz = "GMT" )
    SPLBB$DateFday  = as.Date(SPL$DateF)
  }else { (cat ('BB heading format not found',"\n")) }
 
  #only keep specific columns before combining
  SPLBB = SPLBB[,c(2,4)]
  
  
  #CHECK: duplicated rows in the SPL data!!! keep track and then remove.
  duOL = nrow(SPL) - length( unique(SPL$DateF) )
  duBB = nrow(SPLBB) - length( unique(SPLBB$DateF) )
  #cat(sitesVD[ss], ": duplicated rows = ", duOL, "OL, ", duBB, "BB", "\n")
  SPL =  SPL[!duplicated(SPL$DateF),]
  SPLBB =  SPLBB[!duplicated(SPLBB$DateF),]
  
  SPLa = merge(SPL, SPLBB, by = "DateF" ) 
  SPL = SPLa
  #change some of the colnames... because different! #as.data.frame( colnames(output))
  #as.data.frame( colnames(SPL))
  colnames(SPL)[3]  =  "OL_31.5"
  colnames(SPL)[15] = "BB_20-24000"
  
  # Check: nrow(SPL) - length( unique(SPL$DateF) )
  
  #CREATE accurate time ranges for data-- hourly
  #---------------------------------------------------------------------------------------
  beginTime =   as.POSIXct( min(SPL$DateF) )
  endTime   =   as.POSIXct( max(SPL$DateF) )
  VESSfor   =   as.data.frame( seq(from=beginTime, to=endTime, by="hour") )
  colnames(VESSfor) = "DateF"
  FullListTimes = merge(VESSfor, SPL, by="DateF", all = TRUE)
  FullListTimes$Site = sitesVD[ss]
  FullListTimes = FullListTimes[,c(16,13,14,1,3:12,15)] #reorder and truncate!
  FullListTimes$Day = as.Date(FullListTimes$DateF)
  #duplicate check: nrow(FullListTimes) - length( unique(FullListTimes$DateF) )
  
  #CREATE accurate time ranges for data-- DAY
  #---------------------------------------------------------------------------------------
  beginDay = as.Date(beginTime)
  endDay   = as.Date(endTime)
  VESSday  = as.data.frame( seq(from=beginTime, to=endTime, by="day") ) 
  colnames(VESSday) = "Day"
  VESSday$Day = as.Date(VESSday$Day)
  #process SPL data to get daily median values per octave band
  uday = unique(SPL$Day)
  dSPL = NULL 
  gg = c(as.numeric(grep("^OL", colnames(SPL) ) ), 15) #add the BB column!
  for (ii in 1:length(uday)){ # ii = 1
    dtmp = SPL[SPL$Day == uday[ii],]
    hrsample = nrow(dtmp)
    depl = dtmp$deploy[1]
    #SPL-- median per octave band
    tmpSPL = select(dtmp,c(gg))
    SPLm = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
    dSPL = rbind(dSPL, c(depl, as.character(uday[ii]),  hrsample, SPLm ) )
    
    rm(dtmp,hrsample,depl,tmpSPL,SPLm)
  }
  colnames(dSPL)[1:3] = c("deply","Day","Hrs")
  dSPL = as.data.frame(dSPL)
  dSPL$Day = as.Date(dSPL$Day )
  # check duplicates: nrow(dSPL) - length( unique(dSPL$Day) )
 
  #merge with full day list
  FullListDay= merge(VESSday, dSPL, by="Day", all = TRUE)
  FullListDay$Site = sitesVD[ss]
  FullListDay = FullListDay[,c(15,2,1,4:14,3)] #reorder and truncate!
  
  #ggplot(FullListDay, aes(Day, as.numeric(as.character(OL_31.5)), color = deply))+
    #geom_point()
 
   rm(dname, VESSday,VESSfor,SPL,dSPL,beginDay,beginTime,deply,endDay,endTime,gg,ii,ff,sanct)
  
  #----------------------------------------------------------------------------------------- 
  #PROCESS VESSEL DETECTION DATA TO daily metrics-- COMBINE deployments
  #-----------------------------------------------------------------------------------------
  VESS=NULL
  for (ff in 1:length(sFiles)){
    fname  = sFiles[ff]
    dname  = sapply (strsplit( basename(fname), "_" ),"[",3 ) #add deployment
    tmp1   = fread(fname,header = FALSE, skip=1)
    tmp    = cbind(tmp1[,1:3],dname)
    #some weird files... with empty rows CI02_04
    VESS   = rbind(VESS,tmp)
    rm(fname,dname,tmp)
  }
  
  #FORMAT data
  #-----------------------------------------------------------------------------------------
  VESS$DateFs   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VESS$V1)), tz = "GMT" )
  VESS$DateFe   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VESS$V2))  , tz = "GMT" )
  VESS$Dur_mins = VESS$DateFe  - VESS$DateFs #in minutes!
  VESS = VESS[,3:7]
  names(VESS)[1] = "label"
  names(VESS)[2] = "deply"
  VESS$DateFstart = force_tz(as.Date(VESS$DateFs), tzone="GMT")
  VESS$HrS =hour(VESS$DateFs)
  VESS$HrE =hour(VESS$DateFe)
  
  #CHECK plot/ CLEAN up VESS-- sort and remove duplicates
  #-----------------------------------------------------------------------------------------
  #pVes = ggplot(VESS, aes(DateFs, Dur_mins))+
    #geom_point() +
    #xlab("")+
    #ylab("Duration Vessel Detections (mins)") +
    #theme_minimal()+
    #ggtitle(paste( "Check vessel detections (", sitesVD[ss], ")" ))
  VESSa = arrange(VESS, by_group = VESS$DateFs )
  VESS  = VESSa
  VESSd = dplyr::distinct(VESS)
  VESS  = VESSd
  rm(VESSa,VESSd)
  
  #min(VESS$DateFstart)
  #max(VESS$DateFstart)
  
  #-----------------------------------------------------------------------------------------
  #PROCESS each vessel detection, put in correct column in FullListTimes
  #-----------------------------------------------------------------------------------------
  
  #MAKE new matrix to fill in the values for each hour
  #-----------------------------------------------------------------------------------------
  #make new columns to fill in data....
  #vessel metrics
  FullListTimes$totalVessels = 0 #count of vessel dominated perids
  FullListTimes$totalTime    = 0 #total time vessel noise dominates
  FullListTimes$checkVD      = "noDetection"
  FullListTimes$QuietDur     = 0 #duration of quiet periods within an hour- time between detections
  FullListTimes$QuietPeriods = 0 #number of quiet periods within an hour- time between detections
  FullListTimes$QuietDurSD   = 0

  #vessel dominant periods- loop through detections
  for (vv in  1:nrow(VESS) ){ # vv = 1
    
    tmp = VESS[vv,] #temp matrix
    tmp$DateFend = as.Date(tmp$DateFe) #needed end data in case it goes to the next day!
    
    #find column in with FullListTimes matching day/hour
    idx = which( as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS,":00:00"), tz = "GMT" ) == FullListTimes$DateF) 
    if (length(idx) > 0){ #some detections will extend to end of deployment
      #FullListTimes$DateF[idx] #why are there repeated hours!!!!
      # how many hours does the vessel detection dominate?
      stH = as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS,":00:00"), tz = "GMT" )
      edH = as.POSIXct ( paste0(tmp$DateFend, " "  , tmp$HrE,":00:00"), tz = "GMT" )
      hrsSpan =  as.numeric( difftime(edH , stH,units = "hours") )
      
      if ((hrsSpan) == 1 )#spans to next hour
      {
        #add info to start hour
        FullListTimes$totalVessels[idx]  = FullListTimes$totalVessels[idx] + 1 # vessel count
        FullListTimes$totalTime[idx]     = FullListTimes$totalTime[idx] + difftime(edH, tmp$DateFs, units = "secs") #time in seconds difftime(edH, tmp$DateFs, units = "mins")
        FullListTimes$checkVD[idx] = tmp$deply
        #add info to next hour-- sometimes not available SPL data because last hours of period, so do a check
        if (is.na( FullListTimes$totalVessels[idx+1] ) ) {
          
        }else {
          FullListTimes$totalVessels[idx+1]  = FullListTimes$totalVessels[idx+1] + 1 # vessel count
          FullListTimes$totalTime[idx+1]     = FullListTimes$totalTime[idx+1] + difftime(tmp$DateFe, edH, units = "secs") #time in seconds  difftime(tmp$DateFe, edH, units = "mins")
          FullListTimes$checkVD[idx+1] = tmp$deply
        }
        
        
        
      } else if ((hrsSpan) == 0 ){ #within single hour
        
        FullListTimes$totalVessels[idx]  = FullListTimes$totalVessels[idx] + 1 # vessel count
        FullListTimes$totalTime[idx]     = FullListTimes$totalTime[idx] + difftime(tmp$DateFe,tmp$DateFs,  units = "secs") #time in seconds
        FullListTimes$checkVD[idx]       = tmp$deply
        
      } else if  ((hrsSpan) >= 2 ) { #spans two or more hours
        
        midHr = as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS+1,":00:00"), tz = "GMT" )
        #add info to start hour
        FullListTimes$totalVessels[idx]  = FullListTimes$totalVessels[idx] + 1 # vessel count
        FullListTimes$totalTime[idx]     = FullListTimes$totalTime[idx] + difftime(midHr, tmp$DateFs, units = "secs")
        FullListTimes$checkVD[idx] = tmp$deply
        #add info to middle hours--- need to loop this though each hour
        for(hh in 1:((hrsSpan)-1) ) {
          FullListTimes$totalVessels[idx+hh]  = FullListTimes$totalVessels[idx+hh] + 1 # vessel count
          FullListTimes$totalTime[idx+hh]     = FullListTimes$totalTime[idx+hh] + 3600 #time in seconds, full hour
          FullListTimes$checkVD[idx+hh] = tmp$deply
        }
        #add info to last hour
        FullListTimes$totalVessels[idx+hrsSpan]  = FullListTimes$totalVessels[idx+hrsSpan] + 1 # vessel count
        FullListTimes$totalTime[idx+hrsSpan]     = FullListTimes$totalTime[idx+hrsSpan] + difftime(tmp$DateFe, edH, units = "secs") #time in seconds
        FullListTimes$checkVD[idx+hrsSpan] = tmp$deply
      }
      
      # cat("Hours for detection ", vv, ": ", hrsSpan, "hrs", "\n")
      
    }
    
  }
  
  #(NEW) Quiet periods within each hour
  for ( hh in 1:nrow(FullListTimes) ) {
    tmpHr = FullListTimes[hh,]
    if (tmpHr$totalVessels > 0 ) {
      idxDet  = which( VESS$DateFs >= tmpHr$DateF & VESS$DateFs < tmpHr$DateF + (60*60) ) 
      tmpDets = VESS[idxDet,]
      tmpDurQ = NULL
    
      #if detection exist in the hour, 
      # calculate duration between detection
      if (nrow(tmpDets) >1 ){
        for (tt in 1:nrow(tmpDets)-1){
          tmpDurQ = c( tmpDurQ, difftime(tmpDets$DateFs[tt+1] , tmpDets$DateFe[tt], units = "secs") )
        }
        # calculate duration of last detection to next hour
        tmpDurQ = c( tmpDurQ, difftime( tmpHr$DateF + (60*60), tmpDets$DateFs[nrow(tmpDets)],  units = "secs"))
        # calculate duration of first detection to top of hour
        tmpDurQ = c( tmpDurQ, difftime(tmpDets$DateFs[1], tmpHr$DateF,  units = "secs"))
        FullListTimes$QuietDur[hh]    = mean(tmpDurQ,na.rm = T) # average quiet duration
        FullListTimes$QuietDurSD[hh]  = sd(tmpDurQ,na.rm = T) # average quiet duration
        FullListTimes$QuietPeriods[hh] = length(tmpDurQ)  # one period of all quiet 
      } else if(nrow(tmpDets) == 1 ) { #only one detection for the hour
        
        # calculate duration of last detection to next hour
        tmpDurQ = c( tmpDurQ, difftime( tmpHr$DateF + (60*60), tmpDets$DateFs[nrow(tmpDets)],  units = "secs") )
        # calculate duration of first detection to top of hour
        tmpDurQ = c( tmpDurQ, difftime(tmpDets$DateFs[1], tmpHr$DateF,  units = "secs") )
        FullListTimes$QuietDur[hh]    = mean(tmpDurQ,na.rm = T) # average quiet duration
        FullListTimes$QuietDurSD[hh]  = sd(tmpDurQ,na.rm = T) # average quiet duration
        FullListTimes$QuietPeriods[hh] = length(tmpDurQ)  # one period of all quiet 
      }
      
    } else {
      FullListTimes$QuietDur[hh] = (60 *60) # full hour in seconds
      FullListTimes$QuietDurSD[hh] = 0      # one period of all quiet
      FullListTimes$QuietPeriods[hh] = 1    # one period of all quiet 
      
    }
  }
  
 #hist(FullListTimes$QuietPeriods) 
 #hist(FullListTimes$QuietDur/60) 
 
  # FILL IN NAs for missing data based on SPLs (not sure why some NAs within deplyments... length?)
  #-----------------------------------------------------------------------------------------
  indxNA = which (is.na (FullListTimes$OL_31.5))
  FullListTimes$totalVessels[indxNA] = NA
  FullListTimes$totalTime[indxNA] = NA
  FullListTimes$checkVD[indxNA] = NA
  FullListTimes$QuietDur[indxNA] = NA
  FullListTimes$QuietPeriods[indxNA] = NA
  FullListTimes$QuietDurSD[indxNA] = NA
  
  #FORMAT data-- hourly data (VESSfor)
  #-----------------------------------------------------------------------------------------
  FullListTimes$totalTimeM = FullListTimes$totalTime/(60) 
  errorC = FullListTimes[as.numeric(FullListTimes$totalTimeM) > 60, ] # CHECk: hist(FullListTimes$totalTimeM)
  
  FullListTimes$JulDay = yday(FullListTimes$DateF)
  FullListTimes$Site   = sitesVD[ss]
  FullListTimes$Day    = as.Date(FullListTimes$DateF)
  
  #remove some columns before saving... not needed
  FullListTimes2 = FullListTimes[,  which(names(FullListTimes) != c("checkVD") ) ]
  FullListTimes2 = FullListTimes2[, which(names(FullListTimes2) != c("totalTimeM") ) ]
 
  as.data.frame(colnames( FullListTimes2) )
  colnames(FullListTimes2)[16] = "TotalVesselDet"
  colnames(FullListTimes2)[17] = "TimeVesselDet"
  colnames(FullListTimes2)[2]  = "Deployment"
  colnames(FullListTimes2)[4]  = "DateTime"
  colnames(FullListTimes2)[21] = "JulianDay"
  as.data.frame ( colnames(FullListTimes2) )
  
  # EXPORT HOURLY vessel detections
  #-----------------------------------------------------------------------------------------
  fnameOut =  paste0(outDir, sitesVD[ss], "_SPLVesselDetections_Hr_",as.character(min(as.Date(FullListTimes$DateF))),"to",
                     as.character(max(as.Date(FullListTimes$DateF))), "_ver", DC, ".csv")  
  if(flagCSV == TRUE ){ write.csv(FullListTimes2,fnameOut) }
  
  #-----------------------------------------------------------------------------------------
  # CALCULATE AND EXPORT DAILY summaries
  #-----------------------------------------------------------------------------------------
  #calculate total vessels by summing detections in each day- so vessels daily detections not double counted in occur in multiple hours
  dys = unique(FullListDay$Day)
  FullListDay$totalVessels = 0
  
  for ( dd in 1:length(dys) ) {
    tmp = (VESS[VESS$DateFstart == dys[dd],]) #dd = 10
    idx = which( dys[dd]  == FullListDay$Day)
    tmp$DayE = as.Date(tmp$DateFe)
    
    if (nrow(tmp) == 0 ){ #no data on that day...
      FullListDay$totalVessels[idx] = 0
    }else {
      tmpc = nrow(tmp)
      
      #does the last one go to next day?
      hrsSpan = as.numeric(tmp$DayE[tmpc] - tmp$DateFstart[tmpc] )
    
      if (hrsSpan == 1 )#spans to next day
      {
        #add info to start day
        FullListDay$totalVessels[idx]   = FullListDay$totalVessels[idx] + tmpc    # vessel count
        #add detection to to next day
        FullListDay$totalVessels[idx+1] = FullListDay$totalVessels[idx+1] + 1     # vessel count
        
      } else { #within single day
        FullListDay$totalVessels[idx]   = FullListDay$totalVessels[idx] + tmpc     # vessel count
      }
    }
    
  }
  indxNAd = which( is.na(FullListDay$OL_31.5) )
  FullListDay$totalVessels[indxNAd] = NA  
  
  #Alternative method for calculating daily values from hourly summaries-- accurate for time metrics, NOT total vessels
  #-----------------------------------------------------------------------------------------
  DtimeSum   = aggregate(FullListTimes$totalTime,    by=list(FullListTimes$Day), sum, na.rm=T)     #total time for the day
  DtimeMean  = aggregate(FullListTimes$totalTime,    by=list(FullListTimes$Day), mean, na.rm=T)    #mean per hour for the day
  DvessMean  = aggregate(FullListTimes$totalVessels, by=list(FullListTimes$Day), mean,na.rm=T)     #average vessels per hour
  DQuietDursMean    = aggregate(FullListTimes$QuietDur, by=list(FullListTimes$Day), mean,na.rm=T)         #average duration of quiet
  DQuietPeriodsMean = aggregate(FullListTimes$QuietPeriods, by=list(FullListTimes$Day), mean,na.rm=T)         #average duration of quiet
 
  #DvessSum  = aggregate(FullListTimes$totalVessels, by=list(FullListTimes$Day), sum,na.rm=T)     #total vessels per day
  
  VESSforDay = as.data.frame( cbind(DtimeSum, DtimeMean[,2],DvessMean[,2], DQuietDursMean[,2], DQuietPeriodsMean[,2] ) )
  colnames(VESSforDay) = c("Day","sumTotalTime_sec","HRmeanTotalTime_sec","HRmeanVesselDet_sec", "meanQuietDur","meanQuietPeriods")

  
  FullListDay2 = merge(FullListDay, VESSforDay, by = "Day")
  FullListDay2$sumTotalTime_sec[indxNAd] = NA  
  FullListDay2$HRmeanTotalTime_sec[indxNAd] = NA  
  FullListDay2$HRmeanVesselDet_sec[indxNAd] = NA  
  
  # percent of the day with vessel noise dominating... divide seconds by total time in day sampled
  FullListDay2$PerDayVess = ( FullListDay2$sumTotalTime_sec/(as.numeric(as.character(FullListDay2$Hrs)) *60 *60)) *100
  FullListDay2$PerDayQuiet = 100 - as.numeric( as.character(FullListDay2$PerDayVess ))
  
  #-----------------------------------------------------------------------------------------
  #COMBINE DAILY VD WITH AIS-- so truncates AIS data in most cases
  #-----------------------------------------------------------------------------------------
  cData = merge(FullListDay2,aData,by="Day")
  cols = c("LOA_S_OPHRS","LOA_M_OPHRS","LOA_L_OPHRS","LOA_NA_OPHRS")
  cData$LOA_ALL_OPHRS = rowSums(cData[,cols])
  cols = c("LOA_S_UV","LOA_M_UV","LOA_L_UV","LOA_NA_UV")
  cData$LOA_ALL_UV    = rowSums(cData[,cols])
  as.data.frame(colnames(cData))
  
  cData = cData[,c(1:23, 26:35)] #columns to sum AIS results
  #keep format for plotting
  cDatat = cData
  
  # 
  as.data.frame(colnames(cData))
  #rename BB column so they all match!!!
  colnames(cData)[3]  = "Deployment"
  colnames(cData)[14] = "BB_20-24000"
  cData = cData[, which(names(cData) != c("DATE") ) ] #remove date column
  colnames(cData)[15] = "HoursSampled"
  colnames(cData)[16] = "TotalVesselDet"
  colnames(cData)[17] = "TimeVesselDet_sum"
  colnames(cData)[18] = "TimeVesselDet_meanHourly"
  colnames(cData)[19] = "TotalVesselDet_meanHourly"
  colnames(cData)[20] = "QuietDur_meanHourly"
  colnames(cData)[21] = "QuietPeriods_meanHourly"
  colnames(cData)[22] = "PercentDay_TimeVesselDet"
  colnames(cData)[23] = "PercentDay_TimeQuiet"
  as.data.frame(colnames(cData))
  
  fnameOut =  paste0(outDir, sitesVD[ss], "_SPLVesselDetectionsAIS_Day_",as.character(min(as.Date(cData$Day))),"to",
                     as.character(max(as.Date(cData$Day))), "_ver", DC, ".csv")  
  
  if(flagCSV == TRUE ){ write.csv(cData,fnameOut)} 
  
  #PLOT of DAILY CONDITIONS--- points with monthly running average
  #-----------------------------------------------------------------------------------------
  #A1) daily Vessel detections
  p1 = ggplot(cDatat,aes(Day,totalVessels, color = deply) ) +
    geom_point(alpha = .2)+   geom_line(aes(y=rollmean(totalVessels, ra, na.pad=TRUE)),size=1) +
    xlab("")+     ylab("Daily vessel detections") + theme_minimal()+
    ylim(c(0, round(max(cDatat$totalVessels, na.rm = TRUE)) ) ) +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot))) +
    theme(legend.position=c(.8, 0.7), axis.text.x = element_text(angle=45, hjust = 1), legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
 
  #B1) daily AIS vessels by type
  dataAISm = reshape2 :: melt(cDatat, id.vars = "Day", measure.vars = c("LOA_S_UV","LOA_M_UV","LOA_L_UV","LOA_ALL_UV" ))
  p3 =  ggplot(dataAISm,aes(x=Day,y=value,color=factor(variable)) ) +
    geom_point(alpha = .2)+   geom_line(aes(y=rollmean(value, ra, na.pad=TRUE)),size=1) +
    ylab("Daily unique vessels")+     xlab("") +     theme_minimal() + 
    ylim(c(0, round(max(cDatat$totalVessels, na.rm = TRUE)) ) ) +
    scale_colour_manual(name = "AIS vessls", values=c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00") ) +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot))) +
    theme( legend.position=c(.8, 0.7), axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
 
  #A2) % day dominated by Vessel noise, from total time dominated by vessel noise
  colnames(cDatat)
  p2 = ggplot(cDatat,aes(Day,PerDayVess, color = deply) ) +
    geom_point(alpha = .2)+  geom_line(aes(y=rollmean(PerDayVess, ra, na.pad=TRUE)),size=1) +
    xlab("")+   ylab("% day dominated by vessel noise") + theme_minimal() +
    ylim(c(0,   round(max(cDatat$PerDayVess, na.rm = TRUE)) ) ) +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"),limits = as.Date(c(sDatePlot,eDatePlot))) +
    theme(legend.position="none", axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0))) 
  
  #B2) total operational hours near site
  dataAISm = reshape2 :: melt(cDatat, id.vars = "Day", measure.vars = c("LOA_S_OPHRS","LOA_M_OPHRS","LOA_L_OPHRS","LOA_ALL_OPHRS" ))
  dataAISm$Perday = ( (dataAISm$value) / 24)*100 #calcualte percent of day ships operating, can be > 100%
  p5 = ggplot(dataAISm, aes(x=Day, y=Perday, color=factor(variable)) ) +
    geom_point(alpha = .2)+ geom_line(aes(y=rollmean(Perday, ra, na.pad=TRUE)),size=1) +
    ylab("% of day vessels operating")+     xlab("") +     theme_minimal() + 
    #ylim(c(0, round(max(cDatat$PerDay, na.rm = TRUE)) ) ) +
    scale_colour_manual(name = "AIS vessls", values=c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00") ) +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"),limits = as.Date(c(sDatePlot,eDatePlot)))+
    theme( legend.position="none", axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
 
  pALL2 = grid.arrange(p1,p3,p2,p5,nrow=2,ncol=2,top = (paste0( "Daily vessel metrics (", sitesVD[ss], ")" )))
  
  p4 = ggplot(dataAISm,aes(x=Day,y=value,color=factor(variable)) ) +
    geom_point(alpha = .2)+ geom_line(aes(y=rollmean(value, ra, na.pad=TRUE)),size=.7) +
    ylab("Daily operational hours")+     xlab("") +     theme_minimal() + 
    scale_colour_manual(name = "AIS vessls", values=c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00") ) +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot)))+
    theme( legend.position="none", axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  
  
  if(flagPLT == TRUE ){ ggsave( paste0(outDir,sitesVD[ss], "_DailyVessleMetrics_", "_ver",DC,".png") ,pALL2)}
  
  #Normalizing values.... scale 0-1
  #-----------------------
  #A1) Total vessels
  cDatat$totalVesselsS = stdize(cDatat$totalVessels,na.rm=T)
  p1 = ggplot(cDatat,aes(Day,totalVesselsS,color = deply) ) +
    geom_point(alpha = .2)+  geom_line(aes(y=rollmean(totalVesselsS, ra, na.pad=TRUE)),size=1) +
    annotate( geom = "text", x = as.Date(eDatePlot)-100, y = 0.95, label = paste0("Max = ", round(max(cDatat$totalVessels,na.rm = T)) ),fontface="bold" ) +
    xlab("")+     ylab("scaled daily vessel detections") + theme_minimal()+
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"),limits = as.Date(c(sDatePlot,eDatePlot))) +
    theme(legend.position=c(.8, 0.6), axis.text.x = element_text(angle=45, hjust = 1), legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  #B1) daily AIS vessels by type
  cDatat$LOA_S_UVS = stdize(cDatat$LOA_S_UV,na.rm=T)
  cDatat$LOA_M_UVS = stdize(cDatat$LOA_M_UV,na.rm=T)
  cDatat$LOA_L_UVS = stdize(cDatat$LOA_L_UV,na.rm=T)
  cDatat$LOA_ALL_UVS = stdize(cDatat$LOA_ALL_UV,na.rm=T)
  dataAISm = reshape2 :: melt(cDatat, id.vars = "Day", measure.vars = c("LOA_S_UVS","LOA_M_UVS","LOA_L_UVS","LOA_ALL_UVS" ))
  p3 =  ggplot(dataAISm,aes(x=Day,y=value,color=factor(variable)) ) +
    geom_point(alpha = .2)+ geom_line(aes(y=rollmean(value, ra, na.pad=TRUE)),size=1) +
    annotate( geom = "text", x = as.Date(eDatePlot)-100, y = 0.95, label = paste0("Max (ALL) = ", round(max(cDatat$LOA_ALL_UV,na.rm = T))),fontface="bold" ) +
    ylab("scaled daily unique vessels")+     xlab("") +   theme_minimal() + 
    scale_colour_manual(name = "AIS vessls", values=c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00") ) +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot))) +
    theme( legend.position=c(.8, 0.6), axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  #A2) % day dominated by Vessel noise, from total time dominated by vessel noise
  cDatat$PerDayS = stdize(cDatat$PerDayVess,na.rm=T)
  p2 = ggplot(cDatat,aes(Day,PerDayS,color = deply) ) +
    geom_point(alpha = .2)+ geom_line(aes(y=rollmean(PerDayS, ra, na.pad=TRUE)),size=1) +
    annotate( geom = "text", x = as.Date(eDatePlot)-100, y = 0.95, label = paste0("Max = ", round(max(cDatat$PerDayVess,na.rm = T)) ,"%"),fontface="bold" ) +
    xlab("")+   ylab("scaled % day dominated by vessel noise") + theme_minimal() +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"),limits = as.Date(c(sDatePlot,eDatePlot))) +
    theme( legend.position="none", axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  #B2) total operational hours near site AND percent of day
  #how do I scale percent of the day???
  cDatat$LOA_S_OPHRSS   = stdize( ((cDatat$LOA_S_OPHRS/24)*100)   ,na.rm=T)
  cDatat$LOA_M_OPHRSS   = stdize( ((cDatat$LOA_M_OPHRS/24)*100)   ,na.rm=T)
  cDatat$LOA_L_OPHRSS   = stdize( ((cDatat$LOA_L_OPHRS/24)*100)   ,na.rm=T)
  cDatat$LOA_ALL_OPHRSS = stdize( ((cDatat$LOA_ALL_OPHRS/24)*100) ,na.rm=T)
  dataAISm = reshape2 :: melt(cDatat, id.vars = "Day", measure.vars = c("LOA_S_OPHRSS","LOA_M_OPHRSS","LOA_L_OPHRSS","LOA_ALL_OPHRSS" ))
  mx = round(max(((cDatat$LOA_ALL_OPHRS/24)*100),na.rm = T))
  #which.max(cDatat$LOA_ALL_OPHRS)
  
  p4 = ggplot(dataAISm,aes(x=Day,y=value,color=factor(variable)) ) +
    geom_point(alpha = .2)+ geom_line(aes(y=rollmean(value, ra, na.pad=TRUE)),size=1) +
    annotate( geom = "text", x = as.Date(eDatePlot)-100, y = 0.95, label = paste0("Max (ALL) = ", mx ,"%"), fontface="bold" ) +
    ylab("scaled % of day vessels operating")+     xlab("") +     theme_minimal() + 
    scale_colour_manual(name = "AIS vessls", values=c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00") ) +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot)))+
    theme( legend.position="none", axis.text.x = element_text(angle=45, hjust = 1), legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  
  pALLS = grid.arrange(p1,p3,p2,p4,nrow=2,ncol=2,top = (paste0( "scaled Daily vessel metrics (", sitesVD[ss], ")" )))
  
  if(flagPLT == TRUE ){ ggsave( paste0(outDir,sitesVD[ss], "_DailyVessleMetricsScaled_", "_ver",DC,".png"), pALLS) }
  
  #-----------------------
  #SUMMARY of metrics by site
  #-----------------------
 
  a0  = nrow( cDatat[!is.na(cDatat$OL_31.5),] ) #need to not count NA rows even though AIS data on all days, total days = length( unique(cDatat$Day) ), number of AIS days with data
  aDR = ( c( as.character(min( cDatat$Day )), as.character(max( cDatat$Day ))) )
  
  #AIS averages (use all the data,  remove days without SPL??)
  a1 = mean(as.numeric(as.character(cDatat$LOA_S_UV)),na.rm = T)
  a2 = mean(as.numeric(as.character(cDatat$LOA_M_UV)),na.rm = T)
  a3 = mean(as.numeric(as.character(cDatat$LOA_L_UV)) ,na.rm = T)
  a4 = mean(as.numeric(as.character(cDatat$LOA_ALL_UV)),na.rm = T)
  
  a5 = mean(as.numeric(as.character(cDatat$LOA_S_OPHRS)),na.rm = T)
  a6 = mean(as.numeric(as.character(cDatat$LOA_M_OPHRS)),na.rm = T)
  a7 = mean(as.numeric(as.character(cDatat$LOA_L_OPHRS)),na.rm = T)
  a8 = mean(as.numeric(as.character(cDatat$LOA_ALL_OPHRS)),na.rm = T)
  
  #percent of day OPHRS
  a13 = mean(as.numeric(as.character( ((cDatat$LOA_S_OPHRS/24)*100))), na.rm = T) 
  a14 = mean(as.numeric(as.character( ((cDatat$LOA_M_OPHRS/24)*100))), na.rm = T)
  a15 = mean(as.numeric(as.character( ((cDatat$LOA_L_OPHRS/24)*100))), na.rm = T)
  a16 = mean(as.numeric(as.character( ((cDatat$LOA_ALL_OPHRS/24)*100))), na.rm = T)
  
  #vessel detection averages-- new variables, not include the hourly summaries
  #cDatat$totalVessels,   cDatat$sumTotalTime_sec,   cDatat$HRmeanTotalTime_sec,   cDatat$HRmeanVesselDet_sec,   cDatat$PerDay
  a12 = mean(as.numeric(as.character(cDatat$totalVessels)),na.rm = T) #average vessel events per day
  a11 = mean(as.numeric(as.character(cDatat$PerDayVess)),na.rm = T)   #average percent of day with vessel present
  a9  = mean(as.numeric(as.character(cDatat$sumTotalTime_sec)),na.rm = T) #average total time with vessels per day
  # a10 = mean(as.numeric(as.character(cDatat$sumTotalVessels)) ,na.rm = T) #no longer used so NA
  a22  = mean(as.numeric(as.character(cDatat$meanQuietDur)),na.rm = T)/60
  a23  = mean(as.numeric(as.character(cDatat$meanQuietPeriods)),na.rm = T)
  
  #median SPLs
  gg = c( as.numeric(grep("^OL", colnames(cDatat) ) ), 14)
  tmpSPL = ( cDatat[,gg] )
  w <- which( sapply( tmpSPL, class ) == 'factor' )
  tmpSPL[w] <- lapply( tmpSPL[w], function(x) as.numeric(as.character(x)) )
  SPLm = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
  
  SPLnames = colnames(tmpSPL)
  
  #median SPLs with NO or YES vessels present from hourly data
  #remove rows with no SPL data, so not included in the proporiton of hours!
  FullListTimes2narm = FullListTimes2[! is.na(FullListTimes2$OL_31.5),] 
  
  SPLvesY = FullListTimes2narm[FullListTimes2narm$TotalVesselDet > 0,]
  gg = c( as.numeric(grep("^OL", colnames(FullListTimes2narm) ) ), 15)
  tmpSPL = ( SPLvesY[,gg] )
  w <- which( sapply( tmpSPL, class ) == 'factor' )
  tmpSPL[w] = lapply( tmpSPL[w], function(x) as.numeric(as.character(x)) )
  SPLmVessY = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
  a21 = nrow(SPLvesY)/nrow(FullListTimes2narm) #proportion of hours with no vessel detection
  
  SPLvesN = FullListTimes2narm[FullListTimes2narm$TotalVesselDet == 0,]
  gg = c( as.numeric(grep("^OL", colnames(FullListTimes2narm) ) ), 15)
  tmpSPL = ( SPLvesN[,gg] )
  w <- which( sapply( tmpSPL, class ) == 'factor' )
  tmpSPL[w] = lapply( tmpSPL[w], function(x) as.numeric(as.character(x)) )
  SPLmVessN = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
  a20 = nrow(SPLvesN)/nrow(FullListTimes2narm) #proportion of hours with no vessel detection
  
  SPLnamesN = sub("OL_", "NoVess_OL_",  SPLnames)
  SPLnamesY = sub("OL_", "YesVess_OL_", SPLnames)
  
  #combine summaries for all data
  tmpOUT = t(as.data.frame( c(cDatat$Site[1], as.character(aDR), length(sFiles), nFiles, a0, a1,a2,a3,a4,a5,a6,a7,a8, a13,a14,a15,a16, 
    a9,a11,a12, a20,a21, a22,a23, SPLm, SPLmVessY,SPLmVessN ) ))

   colnames(tmpOUT) = c("Site","Start Day","End Day",
                       "shipFiles", "SPLfiles", "TotalDays",
                       "mAIS_S_UV",      "mAIS_M_UV",      "mAIS_L_UV",     "mAIS_A_UV",
                       "mAIS_S_OPHRS",   "mAIS_M_OPHRS",   "mAIS_L_OPHRS",  "mAIS_A_OPHRS",
                       "mAIS_S_PerOPHRS","mAIS_M_PerOPHRS","mAIS_L_PerOPHRS","mAIS_A_PerOPHRS",
                       "mTotalTime","mPerDay","mTotalVesselsday",
                       "PercentDaysNo_VesselDet","PercentDaysYes_VesselDet",
                       "mQuietDur","mQuietPerids",
                       SPLnames,SPLnamesY,SPLnamesN)

  output = as.data.frame(output, stringsAsFactors = FALSE)
  output = rbind (output, tmpOUT )
  
  #SUMMARY of metrics by site-- truncated time period, 2019
  #-----------------------
  cDatatKeep = cDatat
  cDatat$YR = year(cDatat$Day)
  cDatat    = cDatat[cDatat$YR == 2019,]
  
  a0 = nrow( cDatat[!is.na(cDatat$OL_31.5),] )#length(unique(cDatat$Day)) # number of AIS days with data....
  aDR = c( min( cDatat$Day ), max( cDatat$Day ))
  
  #AIS averages
  a1 = mean(as.numeric(as.character(cDatat$LOA_S_UV)),na.rm = T)
  a2 = mean(as.numeric(as.character(cDatat$LOA_M_UV)),na.rm = T)
  a3 = mean(as.numeric(as.character(cDatat$LOA_L_UV)) ,na.rm = T)
  a4 = mean(as.numeric(as.character(cDatat$LOA_ALL_UV)),na.rm = T)
  
  a5 = mean(as.numeric(as.character(cDatat$LOA_S_OPHRS)),na.rm = T)
  a6 = mean(as.numeric(as.character(cDatat$LOA_M_OPHRS)),na.rm = T)
  a7 = mean(as.numeric(as.character(cDatat$LOA_L_OPHRS)),na.rm = T)
  a8 = mean(as.numeric(as.character(cDatat$LOA_ALL_OPHRS)),na.rm = T)
  
  #percent of day OPHRS
  a13 = mean(as.numeric(as.character( ((cDatat$LOA_S_OPHRS/24)*100))), na.rm = T) 
  a14 = mean(as.numeric(as.character( ((cDatat$LOA_M_OPHRS/24)*100))), na.rm = T)
  a15 = mean(as.numeric(as.character( ((cDatat$LOA_L_OPHRS/24)*100))), na.rm = T)
  a16 = mean(as.numeric(as.character( ((cDatat$LOA_ALL_OPHRS/24)*100))), na.rm = T)
  
  #vessel detection averages-- new variables, not include the hourly summaries
  #cDatat$totalVessels,   cDatat$sumTotalTime_sec,   cDatat$HRmeanTotalTime_sec,   cDatat$HRmeanVesselDet_sec,   cDatat$PerDay
  a12 = mean(as.numeric(as.character(cDatat$totalVessels)),na.rm = T)
  a11 = mean(as.numeric(as.character(cDatat$PerDayVess)),na.rm = T)
  a9  = mean(as.numeric(as.character(cDatat$sumTotalTime_sec)),na.rm = T)
  # a10 = mean(as.numeric(as.character(cDatat$sumTotalVessels)) ,na.rm = T) #no longer used so NA
  a22  = mean(as.numeric(as.character(cDatat$meanQuietDur)),na.rm = T)/60
  a23  = mean(as.numeric(as.character(cDatat$meanQuietPeriods)),na.rm = T)
   
  #median SPLs
  as.data.frame( colnames(cDatat) )
  gg = c( as.numeric(grep("^OL", colnames(cDatat) ) ), 14)
  tmpSPL = ( cDatat[,gg] )
  w <- which( sapply( tmpSPL, class ) == 'factor' )
  tmpSPL[w] <- lapply( tmpSPL[w], function(x) as.numeric(as.character(x)) )
  SPLm = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
  SPLnames = colnames(tmpSPL)
  
  #median SPLs with no vessels present from hourly data
  
  FullListTimes2narm$Yr = year( FullListTimes2narm$Day )
  FullListTimes2t = FullListTimes2narm[FullListTimes2narm$Yr == 2019,]
  
  SPLvesY = FullListTimes2t[FullListTimes2t$TotalVesselDet > 0,]
  as.data.frame( colnames(FullListTimes2t) )
  gg = c( as.numeric(grep("^OL", colnames(FullListTimes2t) ) ), 15)
  tmpSPL = ( SPLvesY[,gg] )
  w <- which( sapply( tmpSPL, class ) == 'factor' )
  tmpSPL[w] = lapply( tmpSPL[w], function(x) as.numeric(as.character(x)) )
  SPLmVessY = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
  
  
  SPLvesN = FullListTimes2t[FullListTimes2t$TotalVesselDet == 0,]
  gg = c( as.numeric(grep("^OL", colnames(FullListTimes2t) ) ), 15)
  tmpSPL = ( SPLvesN[,gg] )
  w <- which( sapply( tmpSPL, class ) == 'factor' )
  tmpSPL[w] = lapply( tmpSPL[w], function(x) as.numeric(as.character(x)) )
  SPLmVessN = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
  
 #check NAs removed: sum( is.na(FullListTimes2t$OL_31.5)  )
  a20 = nrow(SPLvesN)/nrow(FullListTimes2t) #proportion of hours with no vessel detection
  a21 = nrow(SPLvesY)/nrow(FullListTimes2t) #proportion of hours with vessel detection
  

  tmpOUT = t(as.data.frame( c(cDatat$Site[1], as.character(aDR), length(sFiles), nFiles, a0, a1,a2,a3,a4, a5,a6,a7,a8, a13,a14,a15,a16, 
                              a9,a11,a12, (a20), (a21),a22, a23, SPLm, SPLmVessY, SPLmVessN  ) ) )
  colnames(tmpOUT) = c("Site","Start Day","End Day",
                       "shipFiles", "SPLfiles", "TotalDays",
                       "mAIS_S_UV",      "mAIS_M_UV",      "mAIS_L_UV",     "mAIS_A_UV",
                       "mAIS_S_OPHRS",   "mAIS_M_OPHRS",   "mAIS_L_OPHRS",  "mAIS_A_OPHRS",
                       "mAIS_S_PerOPHRS","mAIS_M_PerOPHRS","mAIS_L_PerOPHRS","mAIS_A_PerOPHRS",
                       "mTotalTime","mPerDay","mTotalVesselsday",
                       "PercentDaysNo_VesselDet","PercentDaysYes_VesselDet",
                       "mQuietDur","mQuietPerids",
                       SPLnames,SPLnamesY,SPLnamesN)
  output2 = as.data.frame(output2,stringsAsFactors = FALSE)
  output2 = rbind (output2, tmpOUT )
  
  #if SBO2== calculate values for 2020 data
  #-----------------------
  if (ss == 25) {
   
    cDatatKeep$YR = year(cDatatKeep$Day)
    cDatat    = cDatatKeep[cDatatKeep$YR == 2020,]
    
    a0  = nrow( cDatat[!is.na(cDatat$OL_31.5),] )#length(unique(cDatat$Day)) # number of AIS days with data....
    aDR = c( min( cDatat$Day ), max( cDatat$Day ))
    
    #AIS averages
    a1 = mean(as.numeric(as.character(cDatat$LOA_S_UV)),na.rm = T)
    a2 = mean(as.numeric(as.character(cDatat$LOA_M_UV)),na.rm = T)
    a3 = mean(as.numeric(as.character(cDatat$LOA_L_UV)) ,na.rm = T)
    a4 = mean(as.numeric(as.character(cDatat$LOA_ALL_UV)),na.rm = T)
    
    a5 = mean(as.numeric(as.character(cDatat$LOA_S_OPHRS)),na.rm = T)
    a6 = mean(as.numeric(as.character(cDatat$LOA_M_OPHRS)),na.rm = T)
    a7 = mean(as.numeric(as.character(cDatat$LOA_L_OPHRS)),na.rm = T)
    a8 = mean(as.numeric(as.character(cDatat$LOA_ALL_OPHRS)),na.rm = T)
    
    #percent of day OPHRS
    a13 = mean(as.numeric(as.character( ((cDatat$LOA_S_OPHRS/24)*100))), na.rm = T) 
    a14 = mean(as.numeric(as.character( ((cDatat$LOA_M_OPHRS/24)*100))), na.rm = T)
    a15 = mean(as.numeric(as.character( ((cDatat$LOA_L_OPHRS/24)*100))), na.rm = T)
    a16 = mean(as.numeric(as.character( ((cDatat$LOA_ALL_OPHRS/24)*100))), na.rm = T)
    
    #vessel detection averages-- new variables, not include the hourly summaries
    #cDatat$totalVessels,   cDatat$sumTotalTime_sec,   cDatat$HRmeanTotalTime_sec,   cDatat$HRmeanVesselDet_sec,   cDatat$PerDay
    a12 = mean(as.numeric(as.character(cDatat$totalVessels)),na.rm = T)
    a11 = mean(as.numeric(as.character(cDatat$PerDayVess)),na.rm = T)
    a9  = mean(as.numeric(as.character(cDatat$sumTotalTime_sec)),na.rm = T)
    # a10 = mean(as.numeric(as.character(cDatat$sumTotalVessels)) ,na.rm = T) #no longer used so NA
    a22  = mean(as.numeric(as.character(cDatat$meanQuietDur)),na.rm = T)/60
    a23  = mean(as.numeric(as.character(cDatat$meanQuietPeriods)),na.rm = T)
    
    #median SPLs
    as.data.frame( colnames(cDatat) )
    gg = c( as.numeric(grep("^OL", colnames(cDatat) ) ), 14)
    tmpSPL = ( cDatat[,gg] )
    w <- which( sapply( tmpSPL, class ) == 'factor' )
    tmpSPL[w] <- lapply( tmpSPL[w], function(x) as.numeric(as.character(x)) )
    SPLm = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
    SPLnames = colnames(tmpSPL)
    
    #median SPLs with no vessels present from hourly data
    
    FullListTimes2narm$Yr = year( FullListTimes2narm$Day )
    FullListTimes2t = FullListTimes2narm[FullListTimes2narm$Yr == 2020,]
    
    SPLvesY = FullListTimes2t[FullListTimes2t$TotalVesselDet > 0,]
    as.data.frame( colnames(FullListTimes2t) )
    gg = c( as.numeric(grep("^OL", colnames(FullListTimes2t) ) ), 15)
    tmpSPL = ( SPLvesY[,gg] )
    w <- which( sapply( tmpSPL, class ) == 'factor' )
    tmpSPL[w] = lapply( tmpSPL[w], function(x) as.numeric(as.character(x)) )
    SPLmVessY = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
    
    
    SPLvesN = FullListTimes2t[FullListTimes2t$TotalVesselDet == 0,]
    gg = c( as.numeric(grep("^OL", colnames(FullListTimes2t) ) ), 15)
    tmpSPL = ( SPLvesN[,gg] )
    w <- which( sapply( tmpSPL, class ) == 'factor' )
    tmpSPL[w] = lapply( tmpSPL[w], function(x) as.numeric(as.character(x)) )
    SPLmVessN = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
    
    #check NAs removed: sum( is.na(FullListTimes2t$OL_31.5)  )
    a20 = nrow(SPLvesN)/nrow(FullListTimes2t) #proportion of hours with no vessel detection
    a21 = nrow(SPLvesY)/nrow(FullListTimes2t) #proportion of hours with vessel detection
    
    
    tmpOUT = t(as.data.frame( c(cDatat$Site[1], as.character(aDR), length(sFiles), nFiles, a0, a1,a2,a3,a4, a5,a6,a7,a8, a13,a14,a15,a16, 
                                a9,a11,a12, (a20), (a21), a22,a23, SPLm, SPLmVessY, SPLmVessN  ) ) )
    
    colnames(tmpOUT) = c("Site","Start Day","End Day",
                         "shipFiles", "SPLfiles", "TotalDays",
                         "mAIS_S_UV",      "mAIS_M_UV",      "mAIS_L_UV",     "mAIS_A_UV",
                         "mAIS_S_OPHRS",   "mAIS_M_OPHRS",   "mAIS_L_OPHRS",  "mAIS_A_OPHRS",
                         "mAIS_S_PerOPHRS","mAIS_M_PerOPHRS","mAIS_L_PerOPHRS","mAIS_A_PerOPHRS",
                         "mTotalTime","mPerDay","mTotalVesselsday",
                         "PercentDaysNo_VesselDet","PercentDaysYes_VesselDet",
                         "mQuietDur","mQuietPerids",
                         SPLnames,SPLnamesY,SPLnamesN)
    output3 = as.data.frame(output3,stringsAsFactors = FALSE)
    output3 = rbind (output3, tmpOUT )
    
  }
  
  # CLEAN UP
  rm(edH,errorC,pVes,pVESStime,pVESSvess, tmp, VESS, VESSfor, VESSforDay, x, 
     edB,ff,fnameOut,HrS,hrsSpan,idx,inFiles,inFilesB, vv,stH,DtimeMean,DtimeSum,DvessSum,DvessMean, HRsampled, pALL2)
  
  
}

fnameOut =  paste0(outDir, "Summary2019_ver", DC, ".csv")  
write.csv(output2,fnameOut) 

fnameOut =  paste0(outDir, "SummaryALL_ver", DC, ".csv")  
write.csv(output,fnameOut) 

fnameOut =  paste0(outDir, "Summary2020_ver", DC, ".csv")  
write.csv(output3,fnameOut) 

# colnames(output) = c("Site","Start Day","End Day",
#                      "shipFiles", "SPLfiles", "TotalDays",
#                      "mAIS_S_UV",      "mAIS_M_UV",      "mAIS_L_UV",     "mAIS_A_UV",
#                      "mAIS_S_OPHRS",   "mAIS_M_OPHRS",   "mAIS_L_OPHRS",  "mAIS_A_OPHRS",
#                      "mAIS_S_PerOPHRS","mAIS_M_PerOPHRS","mAIS_L_PerOPHRS","mAIS_A_PerOPHRS",
#                      "mTotalTime","mPerDay","mTotalVesselsday","PercentDaysNo_VesselDet","PercentDaysYes_VesselDet",
#                      SPLnames,SPLnamesY,SPLnamesN)
# 
# colnames(output2) = c("Site","Start Day","End Day",
#                      "shipFiles", "SPLfiles", "TotalDays",
#                      "mAIS_S_UV",      "mAIS_M_UV",      "mAIS_L_UV",     "mAIS_A_UV",
#                      "mAIS_S_OPHRS",   "mAIS_M_OPHRS",   "mAIS_L_OPHRS",  "mAIS_A_OPHRS",
#                      "mAIS_S_PerOPHRS","mAIS_M_PerOPHRS","mAIS_L_PerOPHRS","mAIS_A_PerOPHRS",
#                      "mTotalTime","mPerDay","mTotalVesselsday","PercentDaysNo_VesselDet","PercentDaysYes_VesselDet",
#                      SPLnames,SPLnamesY,SPLnamesN)

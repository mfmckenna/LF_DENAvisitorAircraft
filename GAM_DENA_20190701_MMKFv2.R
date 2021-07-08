#UPDATE!!! This code was updated by Lauren Abbott (Ferguson) on 6/10/2018--The team aims to test the model and try to account for the non-randomness in clip order



rm(list = ls())

library(ggplot2)
library(AICcmodavg)
library(lme4)
library(nlme)
library(AICcmodavg) # model averaging, perfered over MuMin
library(MuMIn)
library(mgcv)

# created by mfm and DJ 2012.10.31

#-------------------------------------------------------------------------------------------------------
# CHOOSE DATA FILE- opens a window to choose the file (will go to directory where the R script is saved)
myFile <- "I:\\RESEARCH\\DENA_NOBi/SurveyB_RestructuredforR.csv" #choose.files() #merged_DENA_nobi_data_20121017_reformat.csv
infile <- basename(myFile)
inpath <- dirname(myFile)
# READ IN DATA
dat <- read.csv(myFile)
#-------------------------------------------------------------------------------------------------------
#clean up missing data- 999
dat[dat==999] = NA
#get rid of rows with out all of the data 
dat = dat[complete.cases(dat$Lmax1sec1_5),]
#-------------------------------------------------------------------------------------------------------
#read in file with clip information
# CHOOSE DATA FILE- opens a window to choose the file (will go to directory where the R script is saved)
myFile <- "I:\\RESEARCH\\DENA_NOBi\\Denali_2017_AudioClipDescriptions.csv" #choose.files() #merged_DENA_nobi_data_20121017_reformat.csv
infile <- basename(myFile)
inpath <- dirname(myFile)
# READ IN DATA
ClipInf <- read.csv(myFile)
#get rid of rows with NA
ClipInf  = ClipInf[complete.cases(ClipInf),]

# merge clip info with each observation
ALL = NULL
for (cc in 1:dim(dat)[1]) # cc = 1
{
  idx = dat[cc,"Clip_ID"]
  ifo = ClipInf[ClipInf$ID == idx,] 
  ALL = rbind(ALL, cbind(dat[cc,],ifo[5]))
}

x = colnames(ALL[94])
colnames(ALL)[colnames(ALL) == x] = "LAeq30s"
rm(dat, ifo)
#------------------------------------------------------------------------------------------------------
#create maxtix for clip 6- accept min and day models
clip6 = ALL[ALL$ClipSeqence == 1,]
ALL6 = NULL
for (cc in 1:dim(clip6)[1]) # cc = 1
{
  idx = clip6[cc,"SC6"]
  ifo = ClipInf[ClipInf$ID == idx,] 
  ALL6 = rbind(ALL6, cbind(clip6[cc,],ifo[5]))
}
x = colnames(ALL6[95])
colnames(ALL6)[colnames(ALL6) == x] = "LAeq30s6"
rm(idx, ifo, clip6,x,cc)

#-------------------------------------------------------------------------------------------------------
# calculate the DIFFERENCE between the dB LEVELS of the DIFFERENT CLIPS heard by a given subject
# differnce from the previous

for (cc in 1:dim(ALL)[1]) # cc = 1
{
  if (ALL$ClipSeqence[cc] == 1) 
    {ALL$datClipPrev[cc] = NA }
  else {ALL$datClipPrev[cc] = ALL$LAeq30s[cc] - ALL$LAeq30s[cc-1]}
}

#difference from the max/min & first last
invd = unique(ALL$ID)
ALL2 = NULL
for ( ii in 1:length(invd) ) { # ii = 1
  
  tmp = ALL[ALL$ID == invd[ii],]
  tmp$datClipMax = tmp$LAeq30s - max(tmp$LAeq30s)
  tmp$datClipMin = tmp$LAeq30s - min(tmp$LAeq30s)
  
  tmp$datClipLast =  tmp$LAeq30s - (tmp$LAeq30s[5])
  tmp$datClipFirst = tmp$LAeq30s - max(tmp$LAeq30s[1])
  #if positive- louder than first and last
  
  ALL2 = rbind(ALL2,tmp)
  rm(tmp)
}

rm (ALL, ii, invd)     

#-------------------------------------------------------------------------------------------------------
# find the columns with the subject's responses to the sound clips
#acceptability of each sound heard 
ALL2$Acceptablity = as.numeric( ALL2$Acceptablity )  # unique(ALL2$Acceptablity)
ALL2$Interp = as.numeric( ALL2$Interp ) # unique(ALL2$Interp)  pleasing to annoying score (1-9)

#-------------------------------------------------------------------------------------------------------
# combine data into one big matrix to read into the statistical model- so need to repeat values 5 times for
# each acoustic clip for a given subject...
ALL6$accept_min= NA
ALL6$min_length = NA
ALL6$accept_day = NA
ALL6$day_length = NA

ALL6min = NULL
for (ii in 1:dim(ALL6)[1]){
  tmp = rbind(ALL6[ii,],ALL6[ii,],ALL6[ii,],ALL6[ii,]) #repeat the lines for an individual
  tmp$min_length = c(3,9,15,30) 
  tmp$accept_min = c(tmp$SC6_3min[1],tmp$SC6_9min[1],tmp$SC6_15min[1],tmp$SC6_30min[1]  )
  ALL6min = rbind(ALL6min,tmp)
  rm(tmp)
}


ALL6day = NULL
for (ii in 1:dim(ALL6)[1]){
  tmp = rbind(ALL6[ii,],ALL6[ii,],ALL6[ii,],ALL6[ii,],ALL6[ii,]) #repeat the lines for an individual
  tmp$day_length = c(1,10,25,50,100)  
  tmp$accept_day = c(tmp$SC6_1perday[1],tmp$SC6_10perday[1],tmp$SC6_25perday[1],tmp$SC6_50perday[1], tmp$SC6_100perday[1] )
  ALL6day = rbind(ALL6day,tmp)
  rm(tmp)
}

rm(ALL6)



#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
# Data Exploration
#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------

# Distribution of the response variables...
par(mfcol=c(2,2), mar=c(5,4,2,0.5), oma=c(0.5,0.5,0.5,0.5))
#all clips model
hist(ALL2$Acceptablity,main = "All Clips Acceptance Responses",xlab = "", freq = FALSE)
hist(ALL2$Interp,main = "All Clips Interp Responses", xlab = "")

#clip 6 minute models
hist(ALL6min$accept_min, main = "SC6 Minute Acceptability Responses", xlab = "")

#clip 6 day models
hist(ALL6day$accept_day, main = "SC6 PerDay Acceptability Responses", xlab = "")

boxplot(ALL2$LAeq30s ~ factor(ALL2$ClipSeqence))

#-------------------------------------------------------------------------------------------------------
ALL6min$accept_min[ALL6min$accept_min == 1 ] <- -4
ALL6min$accept_min[ALL6min$accept_min == 2 ] <- -3
ALL6min$accept_min[ALL6min$accept_min == 3 ] <- -2
ALL6min$accept_min[ALL6min$accept_min == 4 ] <- -1
ALL6min$accept_min[ALL6min$accept_min == 5 ] <-  0
ALL6min$accept_min[ALL6min$accept_min == 6 ] <-  1
ALL6min$accept_min[ALL6min$accept_min == 7 ] <-  2
ALL6min$accept_min[ALL6min$accept_min == 8 ] <-  3
ALL6min$accept_min[ALL6min$accept_min == 9 ] <-  4


ALL6day$accept_day[ALL6day$accept_day == 1 ] <- -4
ALL6day$accept_day[ALL6day$accept_day == 2 ] <- -3
ALL6day$accept_day[ALL6day$accept_day == 3 ] <- -2
ALL6day$accept_day[ALL6day$accept_day == 4 ] <- -1
ALL6day$accept_day[ALL6day$accept_day == 5 ] <-  0
ALL6day$accept_day[ALL6day$accept_day == 6 ] <-  1
ALL6day$accept_day[ALL6day$accept_day == 7 ] <-  2
ALL6day$accept_day[ALL6day$accept_day == 8 ] <-  3
ALL6day$accept_day[ALL6day$accept_day == 9 ] <-  4

#--Recode Interest (dummy)-1=Yes 0=NO NA=Don't know/not sure------------------------------------------------------------------------------------------
ALL2$Flight_interest[ALL2$Flight_interest == 2 ] <- 0
ALL2$Flight_interest[ALL2$Flight_interest == 3 ] <- NA


#-------------------------------------------------------------------------------------------------------
# characterize variables correctly... as.numeric, as.factor 
# Note for GAM's categorical variables need to be coded as factors
ALL2$Flight_interest = as.factor( ALL2$Flight_interest)
ALL2$Race_Comb = as.factor ( ALL2$Race_Comb)
ALL2$Gender_select = as.factor( ALL2$Gender_select)
ALL2$GrooupType = as.factor( ALL2$GrooupType)
ALL2$US_Resident = as.factor ( ALL2$US_Resident)
ALL2$YearBorn = as.numeric( ALL2$YearBorn)
ALL2$ClipSeqence = as.numeric ( ALL2$ClipSeqence)
ALL2$Location = as.factor(ALL2$Location)
ALL2$RC_Acceptability = as.numeric(ALL2$RC_Acceptability)
ALL2$datClipMax = as.numeric( ALL2$datClipMax)
ALL2$datClipMin = as.numeric( ALL2$datClipMin)
ALL2$datClipFirst = as.numeric( ALL2$datClipFirst)
ALL2$datClipPrev = as.numeric( ALL2$datClipPrev)

ALL6min$accept_min = as.numeric( ALL6min$accept_min)
ALL6min$Location = as.factor( ALL6min$Location)
ALL6min$min_length = as.numeric( ALL6min$min_length)
ALL6min$LAeq30s6 = as.numeric( ALL6min$LAeq30s6)

ALL6day$accept_day = as.numeric( ALL6day$accept_day)
ALL6day$Location = as.factor( ALL6day$Location)
ALL6day$day_length = as.numeric( ALL6day$day_length)
ALL6day$LAeq30s6 = as.numeric( ALL6day$LAeq30s6)

#-------------------------------------------------------------------------------------------------------
# GAM Model building *Clip Sequence is treated as a factor 
library(mgcv)
library(MuMIn)

gam1 <-	gam(RC_Acceptability ~   s(LAeq30s), data = ALL2, method = "REML")
gam2 <-	gam(RC_Acceptability ~   s(LAeq30s) + ClipSeqence, data = ALL2, method = "REML")# Categorical variables should not have smoothing terms because they are inherently linear
gam3 <-	gam(RC_Acceptability ~   s(LAeq30s, ClipSeqence, bs = "fs"), data = ALL2, method = "REML") 
#"fs" = Factor Smooth for the categorical/continuous interaction term
#"fs" controls for the effects of categories that are not our main variable of interest
# gp for auto correlated data
gam4 <-	gam(RC_Acceptability ~   s(LAeq30s, ClipSeqence, bs = "fs") + Flight_interest, data = ALL2, method = "REML")
gam5 <-	gam(RC_Acceptability ~   s(LAeq30s, ClipSeqence, bs = "fs") + Flight_interest + Location, data = ALL2, method = "REML")


AIC(gam1, gam2, gam3, gam4, gam5)

summary(gam5)
plot(gam5, pages = 1, all.terms = TRUE)
plot(gam5, rug = TRUE, residuals = TRUE)



#additional testing *Clip sequence treated as numberic 
ALL2$ClipSeqence = as.numeric ( ALL2$ClipSeqence)

gam6 <-	gam(RC_Acceptability ~   s(LAeq30s, ClipSeqence) + Flight_interest, data = ALL2, method = "REML")
gam7 <-	gam(RC_Acceptability ~   s(LAeq30s, ClipSeqence, bs = "gp") + Flight_interest + Location, data = ALL2, method = "REML")

plot(gam6, residuals = TRUE, pch = 1)
summary(gam7)

# GAMM Model Building (with ID as the random effect)- the random effect was specified with a smoothing spline (specified random with "re")

gamm1 <-	gam(RC_Acceptability ~   s(LAeq30s, ClipSeqence, bs = "fs") + Flight_interest + s(ID, bs = "re"), data = ALL2, method = "REML")
gamm2 <-	gam(RC_Acceptability ~   s(LAeq30s, ClipSeqence, bs = "fs") + Flight_interest + Location + s(ID, bs = "re"), data = ALL2, method = "REML")

summary(gamm1)
summary(gamm2)

# Random effect of ID is not significant



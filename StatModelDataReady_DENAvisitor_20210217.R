# Analysis of DENA vistor survey data set- acceptibility of overflights
# started by mfm and DJ 2012.10.31

# update 2019-10-08: full model sets GAM, GLMM, CLMM


rm(list = ls())

library(lme4) # linera models
library(stringr)
library(AICcmodavg) # model averaging, perfered over MuMin library(MuMIn)
library(mgcv) # GAM models
library(ordinal)
library(MASS)

#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
# STEP 1: DATA LOAD, CLEAN UP, AND ORGANIZE
#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
# CHOOSE DATA FILE- opens a window to choose the file (will go to directory where the R script is saved)
infileData =  "E:\\RESEARCH\\NSNSD_Projects\\LF_DENAanalysis\\SurveyB_RestructuredforR.csv" #C:\\Users\\mmckenna\\Desktop\\LF_DENAanalysis/SurveyB_RestructuredforR.csv" 
infileData = read.csv( infileData )#choose.files() 
#infile <- basename(myFile)
#inpath <- dirname(myFile)
# READ IN DATA
dat <- as.data.frame( (infileData) )
#-------------------------------------------------------------------------------------------------------
#clean up missing data- 999
dat[dat==999] = NA
#get rid of rows without all of the data 
dat = dat[complete.cases(dat$Lmax1sec1_5),]
#remove rows with NA as response variables- only acceptibility!
# unique (as.factor((dat$Acceptablity)))
dat = dat[complete.cases(dat$Acceptablity), ]
#-------------------------------------------------------------------------------------------------------
#read in file with clip information
# CHOOSE DATA FILE- opens a window to choose the file (will go to directory where the R script is saved)
infileClip =  "E:\\RESEARCH\\NSNSD_Projects\\LF_DENAanalysis\\Denali_2017_AudioClipDescriptions.csv" #choose.files() #merged_DENA_nobi_data_20121017_reformat.csv
infileClip <- infileClip
#infile <- basename(myFile)
#inpath <- dirname(myFile)
# READ IN DATA
ClipInf <- read.csv(infileClip)
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
rm(dat, ifo,x,cc,idx)

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

rm (ALL, ii, invd,cc)     

#-------------------------------------------------------------------------------------------------------
# find the columns with the subject's responses to the sound clips
#acceptability of each sound heard 
ALL2$Acceptablity  = as.numeric( ALL2$Acceptablity )  # unique(ALL2$Acceptablity)
ALL2$AcceptablityF = factor(  ALL2$Acceptablity,ordered=TRUE )  # unique(ALL2$AcceptablityF)
# is.ordered(ALL2$AcceptablityF)
ALL2$Interp        = as.numeric( ALL2$Interp )        # pleasing to annoying score (1-9)
ALL2$InterpF       = as.factor(  ALL2$Interp )

#Recode Interest- 0=NO, 1=Yes, 3=Don't know/not sure
# unique(ALL2$Flight_interest)
# summary(as.factor((ALL2$Flight_interest)))
ALL2$Flight_interest[ALL2$Flight_interest == 2 ] <- 0
# is.ordered(ALL2$Flight_interest)
# ALL2$Flight_interest[ALL2$Flight_interest == 3 ] <- NA

#Recode Acceptibility.. already done and no NAs!
# unique(ALL2$RC_Acceptability)
save(ALL2,file = "E:\\RESEARCH\\NSNSD_Projects\\LF_DENAanalysis\\DENA_ModelInputData.RData")


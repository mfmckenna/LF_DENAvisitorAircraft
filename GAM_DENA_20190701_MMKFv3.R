# Analysis of DENA vistor survey data set- acceptibility of overflights
# started by mfm and DJ 2012.10.31
# update 2019-10-08: full model sets GAM, GLMM, CLMM


rm(list = ls())

library(lme4) # linera models
library(AICcmodavg) # model averaging, perfered over MuMin library(MuMIn)
library(mgcv) # GAM models
library(ordinal)

#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
# STEP 1: DATA LOAD, CLEAN UP, AND ORGANIZE
#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
# CHOOSE DATA FILE- opens a window to choose the file (will go to directory where the R script is saved)
myFile <- "C:\\Users\\mmckenna\\Desktop\\working_tmp\\LF_DENAanalysis/SurveyB_RestructuredforR.csv" #choose.files() #merged_DENA_nobi_data_20121017_reformat.csv
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
myFile <- "C:\\Users\\mmckenna\\Desktop\\working_tmp\\LF_DENAanalysis/Denali_2017_AudioClipDescriptions.csv" #choose.files() #merged_DENA_nobi_data_20121017_reformat.csv
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
ALL2$Interp       = as.numeric( ALL2$Interp ) # unique(ALL2$Interp)  pleasing to annoying score (1-9)

#Recode Interest (dummy) 1=Yes 0=NO, NA=Don't know/not sure
ALL2$Flight_interest[ALL2$Flight_interest == 2 ] <- 0
ALL2$Flight_interest[ALL2$Flight_interest == 3 ] <- NA

#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
# STEP 2: DATA EXPLORATION AND PLOTTING
#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
# Distribution of the response variables...
par(mfcol=c(2,3), mar=c(2,2,2,0.5), oma=c(0.5,0.5,0.5,0.5))
hist(ALL2$RC_Acceptability,main = "Acceptibility",xlab = "", freq = FALSE)
hist(ALL2$LAeq30s, main = "Sound level", xlab = "")
hist(ALL2$ClipSeqence, main = "Sequence", xlab = "")
hist(ALL2$Flight_interest, main = "Flight Interest", xlab = "")
hist(ALL2$Location, main = "Location", xlab = "")
boxplot(ALL2$LAeq30s ~ factor(ALL2$ClipSeqence))

#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
# MODEL BUILDING AND EVALUTATION
#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
# QUESTION: What variables effect how visitors rate the acceptibility of an overflight event?
# RESPONSE VARIABLES
# sound level of event (continuous variable)
# clip sequence (factor, 1-5)
# flight interest (factor, 0 or 1)
# location (two trail heads)
# individual (random effect)


# characterize variables correctly
ALL2$RC_Acceptability = as.numeric( ALL2$RC_Acceptability)
ALL2$LAeq30s          = as.numeric(ALL2$LAeq30s)
ALL2$ClipSeqence      = as.factor (ALL2$ClipSeqence)
ALL2$Flight_interest  = as.factor (ALL2$Flight_interest)
ALL2$Location         = as.factor (ALL2$Location)
ALL2$ID               = as.factor(ALL2$ID)
# not used
ALL2$Race_Comb       = as.factor (ALL2$Race_Comb)
ALL2$Gender_select   = as.factor (ALL2$Gender_select)
ALL2$GrooupType      = as.factor (ALL2$GrooupType)
ALL2$US_Resident     = as.factor (ALL2$US_Resident)
ALL2$YearBorn        = as.factor (ALL2$YearBorn)
ALL2$datClipMax      = as.numeric(ALL2$datClipMax)
ALL2$datClipMin      = as.numeric(ALL2$datClipMin)
ALL2$datClipFirst    = as.numeric(ALL2$datClipFirst)
ALL2$datClipPrev     = as.numeric(ALL2$datClipPrev)


# (1) Generalized Linear Mixed-Effects Models
#candicate models
modlGLMM <- c(
           # one variable
           RC_Acceptability  ~ LAeq30s + (1 | ID),
           RC_Acceptability  ~ ClipSeqence + (1 | ID),
           RC_Acceptability  ~ Flight_interest + (1 | ID),
           RC_Acceptability  ~ Location + (1 | ID),
           # two variables
           RC_Acceptability  ~ LAeq30s + ClipSeqence + (1 | ID),
           RC_Acceptability  ~ LAeq30s + Flight_interest + (1 | ID),
           RC_Acceptability  ~ LAeq30s + Location + (1 | ID),
           RC_Acceptability  ~ ClipSeqence + Flight_interest + (1 | ID),
           RC_Acceptability  ~ ClipSeqence + Location + (1 | ID),
           RC_Acceptability  ~ Flight_interest + Location + (1 | ID),
           #three variables
           RC_Acceptability  ~ LAeq30s + ClipSeqence + Flight_interest +  (1 | ID),
           RC_Acceptability  ~ LAeq30s + ClipSeqence + Location +  (1 | ID),
           RC_Acceptability  ~ LAeq30s + Flight_interest + Location +  (1 | ID),
           RC_Acceptability  ~ ClipSeqence + Flight_interest + Location +  (1 | ID),
           # four variables
           RC_Acceptability  ~ LAeq30s + ClipSeqence + Flight_interest + Location  + (1 | ID)
           )
#model
hmodsGLMM = lapply(modlGLMM, function(mm) lmer(mm, data=ALL2) )
#summary
summary((hmodsGLMM))

# (2) Generalized additive models with integrated smoothness estimation
#candicate models
#model
hmodsGAMM = lapply(modls, function(mm) gam(mm, data=ALL2, method = "REML") )
#summary

# (3) Cumulative Link Mixed Models
#candidate models
#model
ALL2$RC_Acceptability = as.factor( ALL2$RC_Acceptability)
hmods <- lapply(modlGLMM,clmm,data=ALL2,nAGQ=10)
#summary


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
infileData =  "C:\\Users\\mmckenna\\Desktop\\LF_DENAanalysis/SurveyB_RestructuredforR.csv" 
infileData <- infileData #choose.files() 
#infile <- basename(myFile)
#inpath <- dirname(myFile)
# READ IN DATA
dat <- as.data.frame( read.csv(infileData) )
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
infileClip =  "C:\\Users\\mmckenna\\Desktop\\LF_DENAanalysis/Denali_2017_AudioClipDescriptions.csv" #choose.files() #merged_DENA_nobi_data_20121017_reformat.csv
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

#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
# STEP 2: DATA EXPLORATION AND PLOTTING
#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
# Distribution of the response variables...
par(mfcol=c(2,3), mar=c(2,2,2,0.5), oma=c(0.5,0.5,0.5,0.5))
hist(ALL2$RC_Acceptability, main = "Acceptibility", xlab = "", freq = FALSE)
hist(ALL2$LAeq30s, main = "Sound level", xlab = "")
hist(ALL2$ClipSeqence, main = "Sequence", xlab = "")
hist(ALL2$Flight_interest, main = "Flight Interest", xlab = "")
hist(ALL2$Location, main = "Location", xlab = "")
boxplot(ALL2$LAeq30s ~ factor(ALL2$ClipSeqence), main = "Seq vs level")

#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
# MODEL BUILDING AND EVALUTATION
#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------
# QUESTION: What variables effect how visitors rate the acceptibility of an overflight event?
# 5 Predictor VARIABLES
# sound level of event (continuous variable)
# clip sequence (factor, 1-5)
# flight interest (factor, 0, 1, 3)
# location (two trail heads)
# individual (random effect)


# characterize predictor variables correctly
ALL2$RC_Acceptability = as.numeric( ALL2$RC_Acceptability) # for GLM and GAM models
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

#------------------------------------------------------------------
# (1) Generalized Linear Mixed-Effects Models (done)
#candicate models
ALL2$RC_Acceptability = as.numeric( ALL2$RC_Acceptability) 
modlGLMM <- list()
# one variable
modlGLMM[[1]] = lmer(RC_Acceptability  ~ LAeq30s + (1 | ID), data=ALL2,na.action = na.omit )
modlGLMM[[2]] = lmer(RC_Acceptability  ~ ClipSeqence + (1 | ID), data=ALL2 )
modlGLMM[[3]] = lmer(RC_Acceptability  ~ Flight_interest + (1 | ID), data=ALL2 )
modlGLMM[[4]] = lmer(RC_Acceptability  ~ Location + (1 | ID), data=ALL2 )
# two variables
modlGLMM[[5]] = lmer(RC_Acceptability  ~ LAeq30s + ClipSeqence + (1 | ID), data=ALL2 )
modlGLMM[[6]] = lmer(RC_Acceptability  ~ LAeq30s + Flight_interest + (1 | ID), data=ALL2 )
modlGLMM[[7]] = lmer(RC_Acceptability  ~ LAeq30s + Location + (1 | ID), data=ALL2 )
modlGLMM[[8]] = lmer(RC_Acceptability  ~ ClipSeqence + Flight_interest + (1 | ID), data=ALL2 )
modlGLMM[[9]] = lmer(RC_Acceptability  ~ ClipSeqence + Location + (1 | ID), data=ALL2 )
modlGLMM[[10]] = lmer(RC_Acceptability  ~ Flight_interest + Location + (1 | ID), data=ALL2 )
#three variables
modlGLMM[[11]] = lmer(RC_Acceptability  ~ LAeq30s + ClipSeqence + Flight_interest +  (1 | ID), data=ALL2 )
modlGLMM[[12]] = lmer(RC_Acceptability  ~ LAeq30s + ClipSeqence + Location +  (1 | ID), data=ALL2 )
modlGLMM[[13]] = lmer(RC_Acceptability  ~ LAeq30s + Flight_interest + Location +  (1 | ID), data=ALL2 )
modlGLMM[[14]] = lmer(RC_Acceptability  ~ ClipSeqence + Flight_interest + Location +  (1 | ID), data=ALL2 )
# four variables
modlGLMM[[15]] = lmer(RC_Acceptability  ~ LAeq30s + ClipSeqence + Flight_interest + Location  + (1 | ID), data=ALL2 )

#summary
AIC_GLMM = NULL
for (ii in 1:length(modlGLMM)){
  cat("model ",ii,": AIC=", AIC(modlGLMM[[ii]]),"\n" )
  AIC_GLMM = rbind(AIC_GLMM, c(ii,AIC(modlGLMM[[ii]])))
}
modOrder = as.data.frame( AIC_GLMM[order(AIC_GLMM[,2]) ,] )
modOrder[2:15,3] = diff(modOrder[,2])
summary( modlGLMM[[modOrder[1,1]]] )
bestModGLMM = ( modlGLMM[[modOrder[1,1]]] )
summary(bestModGLMM)
#r2.corr.mer(bestModGLMM)
1-var(residuals(bestModGLMM))/(var(model.response(model.frame(bestModGLMM))))


p13 = predict(bestModGLMM)# predicted values for new observations
idx<- apply(is.na(ALL2[,c("ClipSeqence","LAeq30s","Flight_interest","Location")]),1,any) #remove rows with NA
# PLOT
par(mfcol=c(1,2), mar=c(5,4,2,0.5), oma=c(0.5,0.5,0.5,0.5))
hist(ALL2$LAeq30s, main= "",xlab = "Sound Levels")
plot(jitter(ALL2$LAeq30s[!idx]), p13,  cex=.3, xlab ="Sound level ", 
     ylab = "Acceptibility ",xlim = c(0,75), colors = ALL2$Flight_interest[!idx])
points(60,0)
abline(0,0,lty=2,col="green")
abline(5.957955, -0.113624,col="red") #get these values from the model output


library(ggplot2)
unique(ALL2$Flight_interest)
ALL2$Flight_interest1 =ALL2$Flight_interest 
ALL2$Flight_interest = as.factor(ALL2$Flight_interest )
# ALL2$Flight_interest =ALL2$Flight_interest1 
levels(ALL2$Flight_interest)[3] = "not sure"
levels(ALL2$Flight_interest)[1] = "no"
levels(ALL2$Flight_interest)[2] = "yes"
df = ALL2[!idx,]
ggplot(df, aes(x = LAeq30s, y = p13,  colour =Flight_interest) ) + 
  geom_point() +
  geom_abline(intercept = 5.957955, slope = -0.113624, col="black") +
  labs(x = "Sound level", y = "Predicted Acceptibility" ) +
  scale_x_continuous(limits = c(40, 80)) +
  ylim (c(-5, 5))
# how do I add the CI on the predicted line
plot(p13,)
# Values to use to predict 
# https://stats.stackexchange.com/questions/233063/make-prediction-equation-from-logistic-regression-coefficients
# prediction for clip 2 (high level), want to take flight, location 2
Leq = c(30,35,40,45,50,55,60,65,70,75)
5.9580 + (-0.113624 * Leq) + (-0.510077) + (0.650992) + (0.332845)
# prediction for clip 2 (high level), not sure about flight, location 2
PREDglm = 5.9580 + (-0.113624 * Leq) + (-0.510077) + (0.203826) + (0.332845)
PROglm  = exp(PREDglm)/(1+exp(PREDglm))



#------------------------------------------------------------------
# (2) Generalized additive models with integrated smoothness estimation (done, need to validate,graph)
#candicate models
modlGAMM <- list()
# one variable
modlGAMM[[1]] = gam(RC_Acceptability  ~ s(LAeq30s), method = "REML", data=ALL2, na.action = na.omit )
modlGAMM[[2]] = gam(RC_Acceptability  ~ ClipSeqence, method = "REML", data=ALL2 )
modlGAMM[[3]] = gam(RC_Acceptability  ~ Flight_interest, method = "REML", data=ALL2 )
modlGAMM[[4]] = gam(RC_Acceptability  ~ Location, method = "REML", data=ALL2 )
# two variables
modlGAMM[[5]] = gam(RC_Acceptability  ~ s(LAeq30s) + ClipSeqence, method = "REML", data=ALL2 )
modlGAMM[[6]] = gam(RC_Acceptability  ~ s(LAeq30s) + Flight_interest, method = "REML", data=ALL2 )
modlGAMM[[7]] = gam(RC_Acceptability  ~ s(LAeq30s) + Location , method = "REML", data=ALL2 )
modlGAMM[[8]] = gam(RC_Acceptability  ~ ClipSeqence + Flight_interest , method = "REML", data=ALL2 )
modlGAMM[[9]] = gam(RC_Acceptability  ~ ClipSeqence + Location , method = "REML", data=ALL2 )
modlGAMM[[10]] = gam(RC_Acceptability  ~ Flight_interest + Location , method = "REML", data=ALL2 )
#three variables
modlGAMM[[11]] = gam(RC_Acceptability  ~ s(LAeq30s) + ClipSeqence + Flight_interest , method = "REML", data=ALL2 )
modlGAMM[[12]] = gam(RC_Acceptability  ~ s(LAeq30s) + ClipSeqence + Location , method = "REML", data=ALL2 )
modlGAMM[[13]] = gam(RC_Acceptability  ~ s(LAeq30s) + Flight_interest + Location , method = "REML", data=ALL2 )
modlGAMM[[14]] = gam(RC_Acceptability  ~ ClipSeqence + Flight_interest + Location , method = "REML", data=ALL2 )
# four variables
modlGAMM[[15]] = gam(RC_Acceptability  ~ s(LAeq30s) + ClipSeqence + Flight_interest + Location , method = "REML", data=ALL2 )
#summary
AIC_GAMM = NULL
for (ii in 1:15){
  cat("model ",ii,": AIC=", AIC(modlGAMM[[ii]]),"\n" )
  AIC_GAMM = rbind(AIC_GAMM, c(ii,AIC(modlGAMM[[ii]])))
}
modOrder = as.data.frame( AIC_GAMM[order(AIC_GAMM[,2]) ,] )
modOrder[2:15,3] = diff(modOrder[,2])
bestModGAM = ( modlGAMM[[modOrder[1,1]]] )
summary(bestModGAM)
plot(bestModGAM)

 # NOT SURE HOW TO DO THE PREDICTION EQUATION???

#------------------------------------------------------------------
# (3) Cumulative Link Models, response variable as an ordered factor!
# good for data with ordered categorical response variables
# https://rcompanion.org/handbook/G_01.html
ALL2$RC_Acceptability = ordered( ( ALL2$RC_Acceptability) )
plot(ALL2$RC_Acceptability)
is.ordered(ALL2$RC_Acceptability)

#candidate models
modlCLM2 <- list()
# one variable
modlCLM2[[1]] = clm(RC_Acceptability  ~ (LAeq30s) , data=ALL2, nAGQ=10, na.action = na.omit )
modlCLM2[[2]] = clm(RC_Acceptability  ~ ClipSeqence , data=ALL2, nAGQ=10, na.action = na.omit )
modlCLM2[[3]] = clm(RC_Acceptability  ~ Flight_interest , data=ALL2, nAGQ=10, na.action = na.omit )
modlCLM2[[4]] = clm(RC_Acceptability  ~ Location , data=ALL2, nAGQ=10, na.action = na.omit )
# two variables
modlCLM2[[5]] = clm(RC_Acceptability  ~ (LAeq30s) , data=ALL2, nAGQ=10, na.action = na.omit )
modlCLM2[[6]] = clm(RC_Acceptability  ~ (LAeq30s) + Flight_interest , data=ALL2, nAGQ=10, na.action = na.omit )
modlCLM2[[7]] = clm(RC_Acceptability  ~ (LAeq30s) + Location , data=ALL2, nAGQ=10, na.action = na.omit )
modlCLM2[[8]] = clm(RC_Acceptability  ~ ClipSeqence + Flight_interest , data=ALL2, nAGQ=10, na.action = na.omit )
modlCLM2[[9]] = clm(RC_Acceptability  ~ ClipSeqence + Location , data=ALL2, nAGQ=10, na.action = na.omit )
modlCLM2[[10]] = clm(RC_Acceptability  ~ Flight_interest + Location , data=ALL2, nAGQ=10, na.action = na.omit )
#three variables
modlCLM2[[11]] = clm(RC_Acceptability  ~ (LAeq30s) + ClipSeqence + Flight_interest , data=ALL2, nAGQ=10, na.action = na.omit )
modlCLM2[[12]] = clm(RC_Acceptability  ~ (LAeq30s) + ClipSeqence + Location , data=ALL2, nAGQ=10, na.action = na.omit )
modlCLM2[[13]] = clm(RC_Acceptability  ~ (LAeq30s) + Flight_interest + Location , data=ALL2, nAGQ=10, na.action = na.omit )
modlCLM2[[14]] = clm(RC_Acceptability  ~ ClipSeqence + Flight_interest + Location , data=ALL2, nAGQ=10, na.action = na.omit )
# four variables
modlCLM2[[15]] = clm(RC_Acceptability  ~ (LAeq30s) + ClipSeqence + Flight_interest + Location , data=ALL2, nAGQ=10, na.action = na.omit )

#summary
AIC_CLM = NULL
for (ii in 1:15){
  cat("model ",ii,": AIC=", AIC(modlCLM2[[ii]]),"\n" )
  AIC_CLM = rbind(AIC_CLM, c(ii,AIC(modlCLM2[[ii]])))
}


modOrder = as.data.frame( AIC_CLM[order(AIC_CLM[,2]) ,] )
modOrder[2:15,3] = diff(modOrder[,2])
bestModCLM = ( modlCLM2[[modOrder[1,1]]] )
simpleModCLM = ( modlCLM2[[modOrder[6,1]]] )
summary(simpleModCLM)
summary(bestModCLM)

tstmod = clm(RC_Acceptability  ~ (LAeq30s) + Flight_interest , data=ALL2, nAGQ=10, na.action = na.omit )
# proportional odds assumption
#The proportional odds assumption means that for each term included in the model,
#the 'slope' estimate between each pair of outcomes across two response levels are 
#assumed to be the same regardless of which partition we consider
nominal_test(tstmod)
scale_test(bestModCLM)
#EXAMPLE DATASET
#fm <- clm(rating ~ temp + contact, data=wine)
#summary(fm)
#nominal_test(fm)

slice.fm1 <- slice(bestModCLM, lambda = 5)
par(mfrow = c(2,3))
plot(slice.fm1)

#candidate MIXED EFFECT models
modlCLMM <- list()
# one variable
modlCLMM[[1]] = clmm(RC_Acceptability  ~ (LAeq30s) + (1 | ID), data=ALL2, nAGQ=10, na.action = na.omit )
modlCLMM[[2]] = clmm(RC_Acceptability  ~ ClipSeqence + (1 | ID), data=ALL2, nAGQ=10, na.action = na.omit )
modlCLMM[[3]] = clmm(RC_Acceptability  ~ Flight_interest + (1 | ID), data=ALL2, nAGQ=10, na.action = na.omit )
modlCLMM[[4]] = clmm(RC_Acceptability  ~ Location + (1 | ID), data=ALL2, nAGQ=10, na.action = na.omit )
# two variables
modlCLMM[[5]] = clmm(RC_Acceptability  ~ (LAeq30s) + (1 | ID), data=ALL2, nAGQ=10, na.action = na.omit )
modlCLMM[[6]] = clmm(RC_Acceptability  ~ (LAeq30s) + Flight_interest+ (1 | ID), data=ALL2, nAGQ=10, na.action = na.omit )
modlCLMM[[7]] = clmm(RC_Acceptability  ~ (LAeq30s) + Location + (1 | ID), data=ALL2, nAGQ=10, na.action = na.omit )
modlCLMM[[8]] = clmm(RC_Acceptability  ~ ClipSeqence + Flight_interest + (1 | ID), data=ALL2, nAGQ=10, na.action = na.omit )
modlCLMM[[9]] = clmm(RC_Acceptability  ~ ClipSeqence + Location + (1 | ID), data=ALL2, nAGQ=10, na.action = na.omit )
modlCLMM[[10]] = clmm(RC_Acceptability  ~ Flight_interest + Location + (1 | ID), data=ALL2, nAGQ=10, na.action = na.omit )
#three variables
modlCLMM[[11]] = clmm(RC_Acceptability  ~ (LAeq30s) + ClipSeqence + Flight_interest + (1 | ID), data=ALL2, nAGQ=10, na.action = na.omit )
modlCLMM[[12]] = clmm(RC_Acceptability  ~ (LAeq30s) + ClipSeqence + Location + (1 | ID), data=ALL2, nAGQ=10, na.action = na.omit )
modlCLMM[[13]] = clmm(RC_Acceptability  ~ (LAeq30s) + Flight_interest + Location + (1 | ID), data=ALL2, nAGQ=10, na.action = na.omit )
modlCLMM[[14]] = clmm(RC_Acceptability  ~ ClipSeqence + Flight_interest + Location + (1 | ID), data=ALL2, nAGQ=10, na.action = na.omit )
# four variables
modlCLMM[[15]] = clmm(RC_Acceptability  ~ (LAeq30s) + ClipSeqence + Flight_interest + Location + (1 | ID), data=ALL2, nAGQ=10, na.action = na.omit )

#summary
AIC_CLMM = NULL
for (ii in 1:15){
  cat("model ",ii,": AIC=", AIC(modlCLMM[[ii]]),"\n" )
  AIC_CLMM = rbind(AIC_CLMM, c(ii,AIC(modlCLMM[[ii]])))
}


modOrder = as.data.frame( AIC_CLMM[order(AIC_CLMM[,2]) ,] )
modOrder[2:15,3] = diff(modOrder[,2])
bestModCLM = ( modlCLM2[[modOrder[1,1]]] )
simpleModCLM = ( modlCLM2[[modOrder[6,1]]] )
summary(simpleModCLM)
summary(bestModCLM)

hist( predict(bestModCLM)[[1]])






XT = xtabs( ~ Flight_interest + RC_Acceptability,
       data = ALL2)


prop.table(XT,
           margin = 1)

library(lattice)

histogram(~ RC_Acceptability | Flight_interest,
          data=ALL2,
          layout=c(1,3)      #  columns and rows of individual plots
)

model = clm(RC_Acceptability ~ Flight_interest,
            data = ALL2)
nominal_test(model)
scale_test(model)

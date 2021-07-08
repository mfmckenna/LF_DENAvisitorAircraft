# Analysis of DENA vistor survey data set- acceptibility of overflights
# started by mfm and DJ 2012.10.31 
rm(list = ls())

# data organize/format using: StatModelDataReady_DENAvisitor_20210217.R

# READ IN DATA FRAME
#-------------------------------------------------------------------------------------------------------
inDir = "E:\\RESEARCH\\NSNSD_Projects\\LF_DENAanalysis\\" # SET TO LOCAL DIRECTORY
setwd(inDir)
load("DENA_ModelInputData.RData")
dataNames = as.data.frame(colnames(ALL2))
dataNames #NOTE LOTS OF VARIABLES NOT USED!!!

#simplify input variables for precidicting "acceptibility"
# acceptibility ~ LAeq30s + ClipSeqence + Flight_interest + Location
dataIn = ALL2[c(1:5,83,94,75,37,6)]
dataNames = as.data.frame(colnames(dataIn))
dataNames

# LOAD STATISTICAL PACKAGES
#-------------------------------------------------------------------------------------------------------
library(lme4) # linear models
library(mgcv) # GAM models
library(AICcmodavg) # model averaging, perfered over MuMin library(MuMIn)
library(ordinal)
library(stringr)
library(MASS)

# PLOT VARIABLES
#-------------------------------------------------------------------------------------------------------
# Distribution of the response variables
par(mfcol=c(2,3), mar=c(2,2,2,0.5), oma=c(0.5,0.5,0.5,0.5))
hist(dataIn$RC_Acceptability, main = "Acceptibility", xlab = "", freq = FALSE)
hist(dataIn$LAeq30s, main = "Sound level", xlab = "")
hist(dataIn$ClipSeqence, main = "Sequence", xlab = "")
hist(dataIn$Flight_interest, main = "Flight Interest", xlab = "")
hist(dataIn$Location, main = "Location", xlab = "")
boxplot(dataIn$LAeq30s ~ factor(dataIn$ClipSeqence), main = "Seq vs level")

# BUILD STATISTICAL MODELS
# CLMM: Cumulative Link Models, response variable as an ordered factor!
# good for data with ordered categorical response variables: https://rcompanion.org/handbook/G_01.html
#-------------------------------------------------------------------------------------------------------
#MAKE response variable as an ordered factor
#testing: #ALL2$RC_Acceptability[1:5]  #ordered( ( ALL2$RC_Acceptability[1:5] ) )
dataIn$RC_Acceptability = ordered( ( ALL2$RC_Acceptability ) )
#did it work?
par(mfcol=c(1,1))
plot(dataIn$RC_Acceptability)
is.ordered(dataIn$RC_Acceptability) #YES!
#!!! fix location to unordered factor

#BUILD candidate models-- MIXED EFFECT models, ID as random effect
modlCLMM <- list()
# global
modlCLMM[[1]]  = clmm(RC_Acceptability  ~ (LAeq30s) + ClipSeqence + Flight_interest + Location + (1 | ID), data=dataIn, nAGQ=10, na.action = na.omit )
# one variable
modlCLMM[[2]]  = clmm(RC_Acceptability  ~ (LAeq30s)                                            + (1 | ID), data=dataIn, nAGQ=10, na.action = na.omit )
modlCLMM[[3]]  = clmm(RC_Acceptability  ~ ClipSeqence                                          + (1 | ID), data=dataIn, nAGQ=10, na.action = na.omit )
modlCLMM[[4]]  = clmm(RC_Acceptability  ~ Flight_interest                                      + (1 | ID), data=dataIn, nAGQ=10, na.action = na.omit )
modlCLMM[[5]]  = clmm(RC_Acceptability  ~ Location                                             + (1 | ID), data=dataIn, nAGQ=10, na.action = na.omit )
# two variables
modlCLMM[[6]]  = clmm(RC_Acceptability  ~ (LAeq30s) + Flight_interest                          + (1 | ID), data=dataIn, nAGQ=10, na.action = na.omit )
modlCLMM[[7]]  = clmm(RC_Acceptability  ~ (LAeq30s) + Location                                 + (1 | ID), data=dataIn, nAGQ=10, na.action = na.omit )
modlCLMM[[8]]  = clmm(RC_Acceptability  ~ ClipSeqence + Flight_interest                        + (1 | ID), data=dataIn, nAGQ=10, na.action = na.omit )
modlCLMM[[9]] = clmm(RC_Acceptability   ~ ClipSeqence + Location                               + (1 | ID), data=dataIn, nAGQ=10, na.action = na.omit )
modlCLMM[[10]] = clmm(RC_Acceptability  ~ Flight_interest + Location                           + (1 | ID), data=dataIn, nAGQ=10, na.action = na.omit )
#three variables
modlCLMM[[11]] = clmm(RC_Acceptability  ~ (LAeq30s) + ClipSeqence + Flight_interest            + (1 | ID), data=dataIn, nAGQ=10, na.action = na.omit )
modlCLMM[[12]] = clmm(RC_Acceptability  ~ (LAeq30s) + ClipSeqence + Location                   + (1 | ID), data=dataIn, nAGQ=10, na.action = na.omit )
modlCLMM[[13]] = clmm(RC_Acceptability  ~ (LAeq30s) + Flight_interest + Location               + (1 | ID), data=dataIn, nAGQ=10, na.action = na.omit )
modlCLMM[[14]] = clmm(RC_Acceptability  ~ ClipSeqence + Flight_interest + Location             + (1 | ID), data=dataIn, nAGQ=10, na.action = na.omit )


# EVALUATE STATISTICAL MODELS
#-------------------------------------------------------------------------------------------------------
AIC_CLMM = NULL
for (ii in 1:length(modlCLMM)){
  cat("model ",ii,": AIC=", AIC(modlCLMM[[ii]]),"\n" )
  AIC_CLMM = rbind(AIC_CLMM, c(ii,AIC(modlCLMM[[ii]])))
}

modOrder = as.data.frame( AIC_CLMM[order(AIC_CLMM[,2]) ,] )
modOrder[2:length(modlCLMM),3] = diff(modOrder[,2])
bestModCLM   = ( modlCLMM[[modOrder[1,1]]] )
simpleModCLM = ( modlCLMM[[modOrder[12,1]]] )
summary(simpleModCLM)
summary(bestModCLM)

exp(bestModCLM$coefficients[-1])
#to move from -3 to - 2 rating- in dB change
(bestModCLM$coefficients[2] - bestModCLM$coefficients[1])  / -0.171661
(bestModCLM$coefficients[2] - bestModCLM$coefficients[1])  / -0.171661

predict(bestModCLM)

# PLOT STATISTICAL MODEL RESULTS
#-------------------------------------------------------------------------------------------------------

# OUTPUT TABLES OF STATISTICAL MODELS RESULTS
#-------------------------------------------------------------------------------------------------------

# Predictions
#-------------------------------------------------------------------------------------------------------
# Highest rated experience


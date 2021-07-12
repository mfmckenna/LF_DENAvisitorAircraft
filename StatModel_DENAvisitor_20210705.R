# Analysis of DENA vistor survey data set- acceptability of overflights
# started by mfm and DJ 2012.10.31 
rm(list = ls())

# data organize/format using: StatModelDataReady_DENAvisitor_20210217.R

# READ IN DATA FRAME
#-------------------------------------------------------------------------------------------------------
#created Rproject to avoid this step...
#inDir = "G:\\My Drive\\ActiveProjects\\LF_DENAvisitorAircraft\\code" # SET TO LOCAL DIRECTORY
#setwd(inDir)
 
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

## FORMAT VARIABLES
#ACCEPTIBILITY as ordered factor
dataIn$RC_Acceptability = ordered( ( ALL2$RC_Acceptability ) )
# CHECK: is.ordered(dataIn$RC_Acceptability) #YES!
#LOCATION as factor
dataIn$Location   =  as.factor(dataIn$Location)
dataIn$Flight_interest = as.factor( dataIn$Flight_interest)

#CLIP SEQUENCE- ordered factor b/c happened in order vs numeric value??
#dataIn$ClipSeqence = ordered((dataIn$ClipSeqence))
   
#ID- as factor
dataIn$ID = as.factor( dataIn$ID)

## CLEAN UP DATA TABLE
dataIn2 = na.omit(dataIn) # CK: nrow(dataIn) - nrow(dataIn2)
dataIn3 = dataIn2[,5:10]  #remove unused columns

## BUILD candidate models-- MIXED EFFECT models, ID as random effect
modlCLMM <- list()
# global
modlCLMM[[1]]  = clmm2(RC_Acceptability  ~ LAeq30s + ClipSeqence + Flight_interest + Location , random = ID, data=dataIn3, nAGQ=10, na.action = na.omit, Hess =TRUE )
# one variable
modlCLMM[[2]]  = clmm2(RC_Acceptability  ~ LAeq30s                                            , random = ID, data=dataIn3, nAGQ=10, na.action = na.omit, Hess =TRUE )
modlCLMM[[3]]  = clmm2(RC_Acceptability  ~ ClipSeqence                                        , random = ID, data=dataIn3, nAGQ=10, na.action = na.omit, Hess =TRUE )
modlCLMM[[4]]  = clmm2(RC_Acceptability  ~ Flight_interest                                    , random = ID, data=dataIn3, nAGQ=10, na.action = na.omit, Hess =TRUE )
modlCLMM[[5]]  = clmm2(RC_Acceptability  ~ Location                                           , random = ID, data=dataIn3, nAGQ=10, na.action = na.omit, Hess =TRUE )
# two variables
modlCLMM[[6]]  = clmm2(RC_Acceptability  ~ LAeq30s + Flight_interest                          , random = ID, data=dataIn3, nAGQ=10, na.action = na.omit, Hess =TRUE )
modlCLMM[[7]]  = clmm2(RC_Acceptability  ~ LAeq30s + Location                                 , random = ID, data=dataIn3, nAGQ=10, na.action = na.omit, Hess =TRUE )
modlCLMM[[8]]  = clmm2(RC_Acceptability  ~ LAeq30s + ClipSeqence                              , random = ID, data=dataIn3, nAGQ=10, na.action = na.omit, Hess =TRUE )
modlCLMM[[9]]  = clmm2(RC_Acceptability  ~ ClipSeqence + Flight_interest                      , random = ID, data=dataIn3, nAGQ=10, na.action = na.omit, Hess =TRUE )
modlCLMM[[10]] = clmm2(RC_Acceptability  ~ ClipSeqence + Location                             , random = ID, data=dataIn3, nAGQ=10, na.action = na.omit, Hess =TRUE )
modlCLMM[[11]] = clmm2(RC_Acceptability  ~ Flight_interest + Location                         , random = ID, data=dataIn3, nAGQ=10, na.action = na.omit, Hess =TRUE )
#three variables
modlCLMM[[12]] = clmm2(RC_Acceptability  ~ LAeq30s + ClipSeqence + Flight_interest            , random = ID, data=dataIn3, nAGQ=10, na.action = na.omit, Hess =TRUE )
modlCLMM[[13]] = clmm2(RC_Acceptability  ~ LAeq30s + ClipSeqence + Location                   , random = ID, data=dataIn3, nAGQ=10, na.action = na.omit, Hess =TRUE )
modlCLMM[[14]] = clmm2(RC_Acceptability  ~ LAeq30s + Flight_interest + Location               , random = ID, data=dataIn3, nAGQ=10, na.action = na.omit, Hess =TRUE )
modlCLMM[[15]] = clmm2(RC_Acceptability  ~ ClipSeqence + Flight_interest + Location           , random = ID, data=dataIn3, nAGQ=10, na.action = na.omit, Hess =TRUE )

# https://stat.ethz.ch/pipermail/r-sig-mixed-models/2011q2/016165.html
# to deal with convergence warning)

# EVALUATE STATISTICAL MODELS
#-------------------------------------------------------------------------------------------------------
AIC_CLMM = NULL
for (ii in 1:length(modlCLMM)){
  cat("model ",ii,": AIC=", AIC(modlCLMM[[ii]]),"\n" )
  AIC_CLMM = rbind(AIC_CLMM, c(ii,AIC(modlCLMM[[ii]])))
}

modOrder = as.data.frame( AIC_CLMM[order(AIC_CLMM[,2]) ,] )
#modOrder[2:length(modlCLMM),3] = round( diff(modOrder[,2]))
modOrder

bestModCLM   = ( modlCLMM[[modOrder[1,1]]] )
summary(bestModCLM)
#bestModCLM2   = ( modlCLMM[[modOrder[2,1]]] )
#summary( bestModCLM2 )

# INTERPRETING STATISTICAL MODEL RESULTS
#clmm2_tutorial.pdf follow example using our data
#-------------------------------------------------------------------------------------------------------
#just run "best" model
fm2  = clmm2(RC_Acceptability  ~ LAeq30s + ClipSeqence + Flight_interest, random = ID, 
                   data=dataIn3, nAGQ=10, na.action = na.omit, Hess =TRUE )
summary(fm2)
# copy MODEL SUMMARY in Table 2, G:\My Drive\ActiveProjects\LF_DENAvisitorAircraft\FiguresTables
# standard deviations of visitor response: 2.2
# Hessian value high
# Sign of coefficients

#The odds of rating in category j or above with an interest in air tour flight is 1.2. (??)
( exp(coef(fm2)[11]) )
( exp(coef(fm2)[10]) )
( exp(coef(fm2)[9]) )

#SIGNIFICANCE OF VISITOR term: 
# use p-value using likelihood ratio tests using ANOVA methods- testing to see if ID (visitor) significant?
fm3 = clmm2(RC_Acceptability  ~ (LAeq30s), random = ID, 
             data=dataIn3, nAGQ=10, na.action = na.omit, Hess =TRUE )
anova(fm2,fm3)
fm4 = clmm2(RC_Acceptability  ~ LAeq30s + Flight_interest, data=dataIn3)
anova(fm2,fm4)

#confidence intervals and profile likelihood of visitor term
pr2 = profile(fm2, range = c(.1,4), nSteps = 30, trace=0)
confint(pr2)
plot(pr2)
#variance for visitor
ci = fm2$ranef + qnorm(0.975) * sqrt(fm2$condVar) %o% c(-1,1)
ord.re = order(fm2$ranef )
ci = ci[order(fm2$ranef),]

plot(1:length( unique(dataIn3$ID) ), fm2$ranef[ord.re], axes = FALSE, ylim=range(ci), 
     xlab="visitor",ylab = "Visitor effect")
axis(1,at=1:length( unique(dataIn3$ID) ),labels = ord.re)
axis(2)
for(i in 1:length(unique(dataIn2$ID) ) )  { segments(i, ci[i,1], i, ci[i,2])  }
abline(h=0,lty = 2)
#the fitted or predicted probability for average visitor
fit1 = (cbind(dataIn2,fitted(bestModCLM)) )
plot(fit1$ID,fit1$`fitted(bestModCLM)`)
#RESULT:  visitors perceived audio clips differently

#PREDICTED PROBABILITIES:
dataIn4 = (cbind(dataIn3,fitted(fm2))) # with visitor effect
dataIn4 = cbind(dataIn4,pred = predict(fm2,newdata = dataIn3)) #AVERAGE VISITOR
head(dataIn4)

hist(dataIn4$pred)
# probability of rating from neutral to acceptible is (with what conditions???):
plogis(fm2$Theta[5] - fm2$Theta[4]) - plogis(fm2$Theta[4] - fm2$Theta[4])

# extreme visitor effect, average judge is 0 b/c random and normally distributed
# @baseline experimental conditions (???)-- average sound level and no flight interest
qnorm(0.95) * c(-1,1) * fm2$stDev
pred = function (eta, theta, cat=1:length(theta)+1, inv.link = plogis) {
  Theta = c(-1e3,theta,1e3)
  sapply(cat, function(j)
    inv.link(Theta[j+1]- eta) - inv.link(Theta[j]- eta) )
}
#cumualtive probability of extreme visitor...
round( pred(qnorm(0.05) * fm2$stDev, fm2$Theta), digits = 4)

#COMPUTE probabilities for average, 5th, and 95th percentile visitors two conditions
# How do I do this for continuous variables??? example on has categorical variables?? (START HERE)
SPLquant = as.data.frame( quantile( dataIn4$LAeq30s, c(.05,0.25, 0.5,0.75,0.95) ) )

mat = expand.grid(ID = qnorm(0.95) * c(-1,0,1) * fm2$stDev, 
                  ClipSeqence      = (1*fm2$beta[2]), #clip sequence 1
                  LAeq30s          = c(SPLquant[3,1]*fm2$beta[1], SPLquant[5,1] * fm2$beta[1]), 
                  Flight_interest  = c(0, fm2$beta[3]) ) #no interst = 0, intested = 1

pred.mat = pred(eta=rowSums(mat),theta=fm2$Theta)
lab = c("Median noise level (64 dBA), No flight interest", 
        "95th noise level (78 dBA), No flight interest",
        "Median noise level (64 dBA), Yes flight interest",
        "95th noise level (78 dBA), Yes flight interest")

par(mfrow=c(2,2))
for( k in c(1,4,7,10) ) {
  plot(seq(from = -3, to = 4, by =1), pred.mat[k,], lty=2, type="l", ylim=c(0,1),
  xlab = "Acceptability rating",axes=FALSE,
  ylab = "Probability", main =lab[ceiling(k/3)])
  axis(1);axis(2)
  lines(seq(from = -3, to = 4, by =1), pred.mat[k+1,],lty=1)
  lines(seq(from = -3, to = 4, by =1), pred.mat[k+2,],lty=3)
  legend("toprigh", c("avg visitor","5th percentile","95th percentile"), lty = 1:3, bty="n")
}

#FIGURE4: predict probability of 0|1 (acceptability) for sound levels 40-80, average visitor, no flight interest
mat2 = expand.grid(ID = 0, #average visitor
                  ClipSeqence      = 1*fm2$beta[2], #clip sequence 2
                  LAeq30s          = seq(from= 20, to= 80,by=1) * fm2$beta[1], #possible sound levels 
                  Flight_interest  = 0 ) #NO fight interest
pred.mat2 = as.data.frame (pred(eta=rowSums(mat2),theta=fm2$Theta))
colnames(pred.mat2) = rownames(as.data.frame( fm2$Theta) )
DBs = as.data.frame(seq(from= 20, to= 80,by=1))
colnames(DBs)="LAEQ"
pred.mat3 = cbind(pred.mat2,DBs)
#pred.mat2$`-1|0`- probability of rating as unacceptable (<=0)
pred.mat3 = as.data.frame(pred.mat3)
#TABLE 3-- copy values
inMx = pred.mat3$`-4|-3`
max(inMx)
idx = which.max( (inMx) )
pred.mat3$LAEQ[idx]

inMx = pred.mat3$`-3|-2`
max(inMx)
idx = which.max( (inMx) )
pred.mat3$LAEQ[idx]

inMx = pred.mat3$`-2|-1`
max(inMx)
idx = which.max( (inMx) )
pred.mat3$LAEQ[idx]

inMx = pred.mat3$`-1|0`
max(inMx)
idx = which.max( (inMx) )
pred.mat3$LAEQ[idx]

inMx = pred.mat3$`0|1`
max(inMx)
idx = which.max( (inMx) )
pred.mat3$LAEQ[idx]

inMx = pred.mat3$`1|2`
max(inMx)
idx = which.max( (inMx) )
pred.mat3$LAEQ[idx]

inMx = pred.mat3$`2|3`
max(inMx)
idx = which.max( (inMx) )
pred.mat3$LAEQ[idx]

inMx = pred.mat3$`3|4`
max(inMx)
idx = which.max( (inMx) )
pred.mat3$LAEQ[idx]


library(ggplot2)
ggplot(pred.mat3, aes(LAEQ,`-1|0`))+
  geom_line()+
  ggtitle("Probability of rating as unacceptable (0 or less) (Average visitor, NO flight interest)")
rowSums(pred.mat2) #do sum of prob equal 1? YES!

pred.mat2m = reshape:: melt(pred.mat2)
DBsm = rbind(DBs,DBs,DBs,DBs,DBs,DBs,DBs,DBs)
pred.mat2m = cbind(pred.mat2m,DBsm)
ggplot(pred.mat2m, aes(LAEQ,value,color=variable))+
  geom_line()+
  ylab("") + xlab("")
 
    
# OLD: At what sound level does average respondents change to acceptability? (use management plan to word this question)
#dB change
bestModCLM = fm2 
(bestModCLM$coefficients[2] - bestModCLM$coefficients[1])  / -0.171661
(bestModCLM$coefficients[3] - bestModCLM$coefficients[2])  / -0.171661
(bestModCLM$coefficients[4] - bestModCLM$coefficients[3])  / -0.171661
(bestModCLM$coefficients[5] - bestModCLM$coefficients[4])  / -0.171661
(bestModCLM$coefficients[6] - bestModCLM$coefficients[5])  / -0.171661
(bestModCLM$coefficients[7] - bestModCLM$coefficients[6])  / -0.171661
(bestModCLM$coefficients[8] - bestModCLM$coefficients[7])  / -0.171661

# Can we predict acceptability from modeled sound levels and map acceptability around different sites?
predict(bestModCLM)


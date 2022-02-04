### CLEAN WORKSPACE AND SOURCE FUNCTIONS ####
# Clear all objects
rm(list=ls())

# Clear all plots
dev.off()


probVals= runif(100000, min= 0, max= 1)

resp= rbinom(length(probVals), size = 1, prob= probVals)

plot(resp ~ probVals)

myDat= data.frame(probVals, resp)

# split into training and testing
set.seed(23489)

# Define the partition (e.g. 75% of the data for training)
trainIndex <- createDataPartition(myDat$resp, p = .75, 
                                  list = FALSE, 
                                  times = 1)

# Split the dataset using the defined partition
trainDat <- myDat[trainIndex, ,drop=FALSE]
calibPlusTestDat<- myDat[-trainIndex, ,drop=FALSE]

# Define a new partition to split the remaining 25%
calibPlusTestIndex <- createDataPartition(calibPlusTestDat$resp,
                                          p = .5,
                                          list = FALSE,
                                          times = 1)

# Split the remaining ~25% of the data: 40% (tune) and 60% (val)
calibDat<- calibPlusTestDat[-calibPlusTestIndex, ,drop=FALSE]
testDat<- calibPlusTestDat[calibPlusTestIndex, ,drop=FALSE]

nrow(trainDat) + nrow(calibDat) + nrow(testDat)
c(nrow(trainDat), nrow(calibDat), nrow(testDat))/nrow(myDat)

# Fit to data
fit= glm(resp~ probVals, data= myDat, family = binomial())

predictDF= as.data.frame(as.vector(predict(fit, type= "response")))
names(predictDF)= 'prob'

fit$coefficients
aVal= fit$coefficients[[1]]
bVal= fit$coefficients[[2]]

probResponse= 1/(1+exp(-bVal * myDat$probVals - aVal))

predictDF$logit= probResponse

predictDF$equalVal= predictDF$prob== predictDF$logit

#Boxplot
boxplot(with(predictDF, prob - logit))

# Fit on train
fitGLM= glm(resp~ probVals, data= trainDat, family = binomial())
fitRF= randomForest(as.factor(resp) ~ probVals, data = trainDat, ntrees = 1000, mtry = 1)

predsGLM= predict(fitGLM, newdata= testDat, type= "response")
predsRF= as.data.frame(predict(fitRF, newdata = testDat, type= 'prob'))

names(predsRF)= c('noProb', 'yesProb')

testDat= cbind(testDat, predsGLM, predsRF)

library(SpecsVerification)

# Original probs
ReliabilityDiagram(testDat$probVals, testDat$resp, plot=TRUE)

# GLM predictions
ReliabilityDiagram(testDat$preds, testDat$resp, plot=TRUE)

# RF predictions
ReliabilityDiagram(testDat$yesProb, testDat$resp, plot=TRUE)

### USE CALIBRATION DATA

# Predict on calibration data
predsRCalibF= as.data.frame(predict(fitRF, newdata = calibDat, type= 'prob'))
names(predsRCalibF)= c('noProb', 'yesProb')

# Train GLM on calibration data

calibDat= cbind(calibDat, predsRCalibF)
fitGLMCalibration= glm(resp~ yesProb, data= calibDat, family = binomial())
summary(fitGLMCalibration)


predsGLMCalibration= predict(fitGLMCalibration, newdata= testDat, type= "response")
testDat= cbind(testDat, predsGLMCalibration)

ReliabilityDiagram(testDat$predsGLMCalibration, testDat$resp, plot=TRUE)



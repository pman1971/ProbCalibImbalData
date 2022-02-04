### CLEAN WORKSPACE AND SOURCE FUNCTIONS ####
# Clear all objects
rm(list=ls())

# Clear all plots
dev.off()

# https://rdrr.io/github/etlundquist/eRic/man/prCalibrate.html
# 
# library("devtools")
# install_github("etlundquist/eRic")

# Many popular machine learning algorithms produce inaccurate predicted probabilities, 
# especially for extreme observations [i.e. pr(y|x) near (0, 1)]. Examples of this 
# include tree ensembles (RF, GBM) and SVM models, which tend to produce predicted 
# probabilites biased towards (0.5) because of row/col bootstrap sampling bias and 
# a reliance on difficult-to-classify observations respectively. Platt (1999) proposed 
# an adjustment in these cases where the original probabilities are used as a 
# predictor in a single-variable logistic regression (optimized for log-loss) to 
# produce more accurate adjusted predicted probabilities. This adjustment will have 
# no effect on model AUC (sigmoid transformations are monotonic), minimal effect on 
# standard classification accuracy, but should yield a significant improvement in 
# log-loss/cross-entropy, which is mainly used to evaluate the quality of predicted 
# probabilities. To get a fair estimate of the effect of calibration the data should 
# be split into 3 parts:
  
# 1. model-building - data used to train the main ML model
# 2. calibration - data used to train the calibration LR classifier on out-of-sample ML predictions
# 3. validation - data used to assess the effects of calibration using dual out-of-sample predictions

library(pROC)
library(caret)
library(Information)
library(randomForest)
set.seed(1492)
data("train")
data("valid")

library(eRic)

training    <- train[,setdiff(names(train), c('UNIQUE_ID', 'TREATMENT'))]
training    <- training[,-nearZeroVar(training)]
partition   <- createDataPartition(training$PURCHASE, p = 0.8, times = 1, list = FALSE)
building    <- training[ partition,] # use this data to train the main model
calibration <- training[-partition,] # use this data to calibrate predicted probabilities

fit     <- randomForest(as.factor(PURCHASE) ~ ., data = building, ntrees = 1000, mtry = 5)
p.calib <- predict(fit, calibration, type = 'prob')[,2] # predicted probabilities for calibration
p.valid <- predict(fit, valid,       type = 'prob')[,2] # predicted probabilities for testing
res     <- prCalibrate(calibration$PURCHASE, p.calib, valid$PURCHASE, p.valid, nbins = 10)

res$raw.logloss
res$cal.logloss
# decrease in validation log-loss

sum((res$raw.probs > 0.5) == res$responses)/length(res$responses)
sum((res$cal.probs > 0.5) == res$responses)/length(res$responses)
# accuracy essentially unchanged

roc(res$responses, res$raw.probs, auc = TRUE)
roc(res$responses, res$cal.probs, auc = TRUE)
# AUC exactly the same (monotonic transformation)

# Try bank data
library(readr)
myDat<- read_delim("Data/bank-full.csv", ";", escape_double = FALSE, trim_ws = TRUE)


# Create numeric resp
myDat$resp= ifelse(myDat$y== 'yes', 1, 0)
myDat$y= NULL

# Set aside validation set
set.seed(123)
sampleID= sample(1:nrow(myDat), size= 0.8 * nrow(myDat))

training= myDat[sampleID,]
valid= myDat[-sampleID,]

partition   <- createDataPartition(training$y, p = 0.8, times = 1, list = FALSE)
building    <- training[ partition,] # use this data to train the main model
calibration <- training[-partition,] # use this data to calibrate predicted probabilities

fit     <- randomForest(as.factor(resp) ~ ., data = building, ntrees = 100, 
                        mtry = round(sqrt(ncol(building))))

p.calib <- predict(fit, calibration, type = 'prob')[,2] # predicted probabilities for calibration
p.valid <- predict(fit, valid,       type = 'prob')[,2] # predicted probabilities for testing
res     <- prCalibrate(calibration$resp, p.calib, valid$resp, p.valid, nbins = 10)

?prCalibrate

res$raw.probs
res$cal.probs


resultsDF= data.frame(rawProbs= res$raw.probs,
                      calProbs= res$cal.probs)
library(tidyr)
resultsDF <- pivot_longer(resultsDF, 
                          cols=1:2, names_to = "Model", values_to = "Prob")

boxplot(Prob~ Model, data= resultsDF)

# Confirm calibratin plot

validDF= cbind(valid, 
               rawProbs= res$raw.probs, 
               calibProbs= res$cal.probs)

testDatDeciles= 
  validDF %>%
  mutate(decile = ntile(rawProbs, 10))

testDatDeciles$decile= as.factor(testDatDeciles$decile)

# Summary stats by decile
calibrationDeciles=
  testDatDeciles %>%
  group_by(decile) %>%
  summarise(totalRows= n(),
            posResp= sum(resp),
            avgPred= mean(rawProbs),
            avgPredCalib= mean(calibProbs)) %>%
  mutate(actualRate= posResp/totalRows)

xyplot(actualRate + avgPred + avgPredCalib~  decile, data= calibrationDeciles, 
       type = "l", pch=20,
       as.table= TRUE,
       main = "Calibration plot", ylab = "%", xlab = "Decile",
       auto.key=list(space="top", columns= 3))

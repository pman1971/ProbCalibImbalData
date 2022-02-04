### CLEAN WORKSPACE AND SOURCE FUNCTIONS ####
# Clear all objects
rm(list=ls())

# Clear all plots
dev.off()

library(caret)
library(ranger)
library(pROC)
library(tidyverse)

# The data is related with direct marketing campaigns (phone calls) of a Portuguese banking institution
# http://archive.ics.uci.edu/ml/datasets/Bank+Marketing
library(readr)
myDat<- read_delim("Data/bank-full.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Examine label
myDat$Class= as.factor(myDat$y)

table(myDat$Class)
prop.table(table(myDat$Class))
barplot(prop.table(table(myDat$Class)))

myDat %>% 
  count(Class) %>% 
  mutate(proportion = round(n / sum(n), 3)) %>% 
  knitr::kable()

# Remove y variable
myDat$y= NULL

# split into training and testing
set.seed(23489)

inTrain <- createDataPartition(
  y = myDat$Class,
  ## the outcome data are needed
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

trainDat <- myDat[inTrain, ]
testDat <- myDat[-inTrain, ]

round(sqrt(ncol(trainDat)))

ctrl <- trainControl(method = "none",
                     classProbs= TRUE)

tgrid <- expand.grid(
  .mtry = round(sqrt(ncol(trainDat))),
  .splitrule = "gini",
  .min.node.size = 10
)

# fit a random forest model (using ranger)
rf_fit <- train(Class ~ ., 
                data = trainDat, 
                trControl = ctrl,
                tuneGrid = tgrid,
                num.trees = 100,
                method = "ranger")

predProbs <- predict(rf_fit, newdata = testDat, type= 'prob')

# Merge with preds
testDat= cbind(testDat, predProbs)

roc(testDat$Class, testDat$yes,
    smoothed = TRUE,
    # arguments for ci
    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
    # arguments for plot
    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
    print.auc=TRUE, show.thres=TRUE)


testDatDeciles= 
  testDat %>%
  mutate(decile = ntile(yes, 10))

testDatDeciles$decile= as.factor(testDatDeciles$decile)

# Summary stats by decile
calibrationDeciles=
  testDatDeciles %>%
  group_by(decile) %>%
  summarise(totalRows= n(),
            posResp= sum(ifelse(Class== 'yes', 1, 0)),
            avgPred= mean(yes)) %>%
  mutate(actualRate= posResp/totalRows)

xyplot(avgPred + actualRate~  decile, data= calibrationDeciles, type = "l", pch=20,
       as.table= TRUE,
       main = "Calibration plot", ylab = "%", xlab = "Decile",
       auto.key=list(space="top", columns= 2))

RFpreds= data.frame(model= 'RF',
                    preds= predProbs$yes)

boxplot(preds~ model, data= RFpreds)

# Balance dataset

majoritySizeOrg <- sum(trainDat$Class== 'no')
minority_class_size <- sum(trainDat$Class== 'yes')

set.seed(1234)

trainDatDS <- trainDat %>% 
  group_by(Class) %>% 
  sample_n(minority_class_size) %>% 
  ungroup()

trainDatDS %>% 
  count(Class) %>% 
  mutate(proportion = round(n / sum(n), 3)) %>% 
  knitr::kable()

majoritySizeDS <- sum(trainDatDS$Class== 'no')


# fit a random forest model (using ranger)
rf_fitDS <- train(Class ~ ., 
                data = trainDatDS, 
                trControl = ctrl,
                tuneGrid = tgrid,
                num.trees = 100,
                method = "ranger")

predProbsDS <- predict(rf_fitDS, newdata = testDat, type= 'prob')

names(predProbsDS)

names(predProbsDS)= paste0(names(predProbsDS), 'DS')

testDat= cbind(testDat, predProbsDS)

roc(testDat$Class, testDat$yesDS,
    smoothed = TRUE,
    # arguments for ci
    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
    # arguments for plot
    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
    print.auc=TRUE, show.thres=TRUE)


testDatDeciles= 
  testDat %>%
  mutate(decile = ntile(yes, 10))

testDatDeciles$decile= as.factor(testDatDeciles$decile)

# Summary stats by decile
calibrationDeciles=
  testDatDeciles %>%
  group_by(decile) %>%
  summarise(totalRows= n(),
            posResp= sum(ifelse(Class== 'yes', 1, 0)),
            avgPred= mean(yes),
            avgPredDS= mean(yesDS)) %>%
  mutate(actualRate= posResp/totalRows)

xyplot(actualRate + avgPred + avgPredDS~  decile, data= calibrationDeciles, 
       type = "l", pch=20,
       as.table= TRUE,
       main = "Calibration plot", ylab = "%", xlab = "Decile",
       auto.key=list(space="top", columns= 2))



testDatCT= 
  testDat %>%
  select(Class, yes, yesDS)
  
library(tidyr)
testDatMT <- pivot_longer(testDatCT, 
                          cols=2:3, names_to = "Model", values_to = "Prob")

boxplot(Prob~ Model, data= testDatMT)

# Calibrate probabilities using scaling

# p = beta * p_s / ((beta-1) * p_s + 1)

# where beta is the ratio of the number majority class instances after undersampling 
# over the number majority class ones in the original training set.


beta= majoritySizeDS/ majoritySizeOrg

testDat$yesDSCal= beta * testDat$yesDS / ((beta-1) * testDat$yesDS + 1)

roc(testDat$Class, testDat$yesDSCal,
    smoothed = TRUE,
    # arguments for ci
    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
    # arguments for plot
    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
    print.auc=TRUE, show.thres=TRUE)

testDatDeciles= 
  testDat %>%
  mutate(decile = ntile(yes, 10))

testDatDeciles$decile= as.factor(testDatDeciles$decile)

# Summary stats by decile
calibrationDeciles=
  testDatDeciles %>%
  group_by(decile) %>%
  summarise(totalRows= n(),
            posResp= sum(ifelse(Class== 'yes', 1, 0)),
            avgPred= mean(yes),
            avgPredDS= mean(yesDS),
            avgPredDSCal= mean(yesDSCal)) %>%
  mutate(actualRate= posResp/totalRows)

lineColors= c('black', 'red', 'blue', 'green')
xyplot(actualRate + avgPred + avgPredDS + avgPredDSCal~  decile, data= calibrationDeciles, 
       type = "l", pch=20,
       as.table= TRUE,
       main = "Calibration plot", ylab = "%", xlab = "Decile",
       auto.key=list(space="top", columns= 4),
       #auto.key = T,
       par.settings = list(superpose.line = list(lwd = c(2,1,1,1))))

# Reliability plot

calibrationDeciles$avgPredIndex= calibrationDeciles$avgPred/calibrationDeciles$actualRate
calibrationDeciles$avgPredDSIndex= calibrationDeciles$avgPredDS/calibrationDeciles$actualRate
calibrationDeciles$avgPredDSCalIndex= calibrationDeciles$avgPredDSCal/calibrationDeciles$actualRate


testDatCT= 
  testDat %>%
  select(Class, yes, yesDS, yesDSCal)

library(tidyr)
testDatMT <- pivot_longer(testDatCT, 
                          cols=2:4, names_to = "Model", values_to = "Prob")

boxplot(Prob~ Model, data= testDatMT)

###################################
### REDO USING CALIBRATION DATA ###
###################################

### CLEAN WORKSPACE AND SOURCE FUNCTIONS ####
# Clear all objects
rm(list=ls())

# Clear all plots
dev.off()

library(caret)
library(ranger)
library(pROC)
library(tidyverse)

# The data is related with direct marketing campaigns (phone calls) of a Portuguese banking institution
# http://archive.ics.uci.edu/ml/datasets/Bank+Marketing
library(readr)
myDat<- read_delim("Data/bank-full.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Examine label
myDat$Class= as.factor(myDat$y)

table(myDat$Class)
prop.table(table(myDat$Class))
barplot(prop.table(table(myDat$Class)))

myDat %>% 
  count(Class) %>% 
  mutate(proportion = round(n / sum(n), 3)) %>% 
  knitr::kable()

# Remove y variable
myDat$y= NULL

# split into training and testing
set.seed(23489)

inTrain <- createDataPartition(
  y = myDat$Class,
  ## the outcome data are needed
  p = .9,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

trainDat <- myDat[inTrain, ]
testDat <- myDat[-inTrain, ]

# Take calibration
inTrain <- createDataPartition(
  y = trainDat$Class,
  ## the outcome data are needed
  p = .9,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

trainDat <- trainDat[inTrain, ]
calibDat <- trainDat[-inTrain, ]

round(sqrt(ncol(trainDat)))

ctrl <- trainControl(method = "none",
                     classProbs= TRUE)

tgrid <- expand.grid(
  .mtry = round(sqrt(ncol(trainDat))),
  .splitrule = "gini",
  .min.node.size = 10
)

# fit a random forest model (using ranger)
rf_fit <- train(Class ~ ., 
                data = trainDat, 
                trControl = ctrl,
                tuneGrid = tgrid,
                num.trees = 100,
                method = "ranger")

predProbs <- predict(rf_fit, newdata = testDat, type= 'prob')

# Merge with preds
testDat= cbind(testDat, predProbs)

roc(testDat$Class, testDat$yes,
    smoothed = TRUE,
    # arguments for ci
    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
    # arguments for plot
    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
    print.auc=TRUE, show.thres=TRUE)


testDatDeciles= 
  testDat %>%
  mutate(decile = ntile(yes, 10))

testDatDeciles$decile= as.factor(testDatDeciles$decile)

# Summary stats by decile
calibrationDeciles=
  testDatDeciles %>%
  group_by(decile) %>%
  summarise(totalRows= n(),
            posResp= sum(ifelse(Class== 'yes', 1, 0)),
            avgPred= mean(yes)) %>%
  mutate(actualRate= posResp/totalRows)

xyplot(avgPred + actualRate~  decile, data= calibrationDeciles, type = "l", pch=20,
       as.table= TRUE,
       main = "Calibration plot", ylab = "%", xlab = "Decile",
       auto.key=list(space="top", columns= 2))

RFpreds= data.frame(model= 'RF',
                    preds= predProbs$yes)

boxplot(preds~ model, data= RFpreds)

# Calibrate probs

# Add propensities to caibration data
predProbs <- predict(rf_fit, newdata = calibDat, type= 'prob')
calibDat= cbind(calibDat, predProbs)


glmCalib= glm(Class ~ yes, data= calibDat, family= binomial())

# Apply calibration model to test set
predProbs <- predict(glmCalib, newdata = testDat, type= 'response')

testDat= cbind(testDat, predProbs)

roc(testDat$Class, testDat$predProbs,
    smoothed = TRUE,
    # arguments for ci
    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
    # arguments for plot
    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
    print.auc=TRUE, show.thres=TRUE)


roc(testDat$Class, testDat$yes,
    smoothed = TRUE,
    # arguments for ci
    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
    # arguments for plot
    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
    print.auc=TRUE, show.thres=TRUE)


testDatDeciles= 
  testDat %>%
  mutate(decile = ntile(yes, 10))

testDatDeciles$decile= as.factor(testDatDeciles$decile)

# Summary stats by decile
calibrationDeciles=
  testDatDeciles %>%
  group_by(decile) %>%
  summarise(totalRows= n(),
            posResp= sum(ifelse(Class== 'yes', 1, 0)),
            avgPred= mean(yes),
            avgPredCalib= mean(predProbs)) %>%
  mutate(actualRate= posResp/totalRows)

xyplot(avgPred + avgPredCalib + actualRate~  decile, data= calibrationDeciles, type = "l", pch=20,
       as.table= TRUE,
       main = "Calibration plot", ylab = "%", xlab = "Decile",
       auto.key=list(space="top", columns= 2))



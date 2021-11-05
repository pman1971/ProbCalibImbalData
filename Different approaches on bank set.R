### CLEAN WORKSPACE AND SOURCE FUNCTIONS ####
# Clear all objects
rm(list=ls())

# Clear all plots
dev.off()

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

library(caret)
library(ranger)
library(pROC)

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

# fit a random forest model (using ranger)
rf_fitDS <- train(Class ~ ., 
                data = trainDatDS, 
                trControl = ctrl,
                tuneGrid = tgrid,
                num.trees = 100,
                method = "ranger")

predProbs <- predict(rf_fitDS, newdata = testDat, type= 'prob')

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

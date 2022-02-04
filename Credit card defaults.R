### CLEAN WORKSPACE AND SOURCE FUNCTIONS ####
# Clear all objects
rm(list=ls())

# Clear all plots
dev.off()

library(caret)
library(ranger)
library(pROC)
library(tidyverse)

myDat <- read_excel("Data/default of credit card clients.xls", skip= 1)

removeCols= c("ID", "SEX", "EDUCATION", "MARRIAGE")

myDat= myDat[, !(colnames(myDat) %in% removeCols), drop = FALSE]

names(myDat)[length(names(myDat))]<-"resp" 

myDat$resp= as.factor(ifelse(myDat$resp==1, 'Yes', 'No'))

myDat %>% 
  count(resp) %>% 
  mutate(proportion = round(n / sum(n), 3)) %>% 
  knitr::kable()

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

# Outcome of this section is that the data (100%) is split into:
# training (~75%)
# tuning (~10%)
# validation (~15%)

nrow(trainDat) + nrow(calibDat) + nrow(testDat)

round(sqrt(ncol(trainDat)))

ctrl <- trainControl(method = "none",
                     classProbs= TRUE)

tgrid <- expand.grid(
  .mtry = round(sqrt(ncol(trainDat))),
  .splitrule = "gini",
  .min.node.size = 10
)

# fit a random forest model (using ranger)
rf_fit <- train(resp ~ ., 
                data = trainDat, 
                trControl = ctrl,
                tuneGrid = tgrid,
                num.trees = 100,
                method = "ranger")

# Apply fitted RF to calib and test set

calibProbs <- predict(rf_fit, newdata = calibDat, type= 'prob')
testProbs <- predict(rf_fit, newdata = testDat, type= 'prob')

# Train GLM on calib data
calibDat= cbind(calibDat, calibProbs$Yes)
names(calibDat)[length(names(calibDat))]<-"probYes" 

testDat= cbind(testDat, testProbs$Yes)
names(testDat)[length(names(testDat))]<-"probYes" 

# Build calibrator
glmCalib= glm(resp ~ probYes, data= calibDat, family= binomial())

# Apply to test set
# Apply calibration model to test set
probYesCalib <- predict(glmCalib, newdata = testDat, type= 'response')

testDat= cbind(testDat, probYesCalib)

roc(testDat$resp, testDat$probYes,
    smoothed = TRUE,
    # arguments for ci
    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
    # arguments for plot
    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
    print.auc=TRUE, show.thres=TRUE)

roc(testDat$resp, testDat$probYesCalib,
    smoothed = TRUE,
    # arguments for ci
    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
    # arguments for plot
    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
    print.auc=TRUE, show.thres=TRUE)


testDat$respNum= ifelse(testDat$resp== 'Yes', 1, 0)

testDatDeciles= 
  testDat %>%
  mutate(decile = ntile(respNum, 10))

testDatDeciles$decile= as.factor(testDatDeciles$decile)

# Summary stats by decile
calibrationDeciles=
  testDatDeciles %>%
  group_by(decile) %>%
  summarise(totalRows= n(),
            posResp= sum(respNum),
            avgPred= mean(probYes),
            avgPredCalib= mean(probYesCalib)) %>%
  mutate(actualRate= posResp/totalRows)

xyplot(avgPred + actualRate~  decile, data= calibrationDeciles, type = "l", pch=20,
       as.table= TRUE,
       main = "Calibration plot", ylab = "%", xlab = "Decile",
       auto.key=list(space="top", columns= 2))

xyplot(avgPredCalib + actualRate~  decile, data= calibrationDeciles, type = "l", pch=20,
       as.table= TRUE,
       main = "Calibration plot", ylab = "%", xlab = "Decile",
       auto.key=list(space="top", columns= 2))

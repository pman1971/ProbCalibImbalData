### CLEAN WORKSPACE AND SOURCE FUNCTIONS ####
# Clear all objects
rm(list=ls())

# Clear all plots
dev.off()

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

ReliabilityDiagram(res$raw.probs, res$responses, plot=TRUE)
ReliabilityDiagram(res$cal.probs, res$responses, plot=TRUE)


res$raw.logloss
res$cal.logloss
# decrease in validation log-loss

sum((res$raw.probs > 0.5) == res$responses)/length(res$responses)
sum((res$cal.probs > 0.5) == res$responses)/length(res$responses)
# accuracy essentially unchanged

roc(res$responses, res$raw.probs, auc = TRUE)
roc(res$responses, res$cal.probs, auc = TRUE)
# AUC exactly the same (monotonic transformation)

### REPEAT WITH BANKSET ###

# The data is related with direct marketing campaigns (phone calls) of a Portuguese banking institution
# http://archive.ics.uci.edu/ml/datasets/Bank+Marketing
library(readr)
myDat<- read_delim("Data/bank-full.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Examine label
myDat$resp= ifelse(myDat$y== 'yes', 1, 0)

table(myDat$resp)
prop.table(table(myDat$resp))
barplot(prop.table(table(myDat$resp)))

myDat %>% 
  count(resp) %>% 
  mutate(proportion = round(n / sum(n), 3)) %>% 
  knitr::kable()

# Remove y variable
myDat$y= NULL

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

building    <- trainDat # use this data to train the main model
calibration <- calibDat # use this data to calibrate predicted probabilities
valid= testDat

fit     <- randomForest(as.factor(resp) ~ ., data = building, ntrees = 1000, mtry = 5)
p.calib <- predict(fit, calibration, type = 'prob')[,2] # predicted probabilities for calibration
p.valid <- predict(fit, valid,       type = 'prob')[,2] # predicted probabilities for testing
res     <- prCalibrate(calibration$resp, p.calib, valid$resp, p.valid, nbins = 10)

ReliabilityDiagram(p.valid, valid$resp, plot=TRUE)
ReliabilityDiagram(res$cal.probs, valid$resp, plot=TRUE)


res$cal.probs


res$raw.logloss
res$cal.logloss
# decrease in validation log-loss

sum((res$raw.probs > 0.5) == res$responses)/length(res$responses)
sum((res$cal.probs > 0.5) == res$responses)/length(res$responses)
# accuracy essentially unchanged

roc(res$responses, res$raw.probs, auc = TRUE)
roc(res$responses, res$cal.probs, auc = TRUE)
# AUC exactly the same (monotonic transformation)

# Do it manually

# Predit RF on calibration and valid set

fitRF= randomForest(as.factor(resp) ~ ., data = building, ntrees = 1000, mtry = 5)

predsRFCalibration= as.data.frame(predict(fit, newdata = calibration, type= 'prob'))
predsRFValid= as.data.frame(predict(fit, newdata = valid, type= 'prob'))

names(predsRFCalibration)= c('noProb', 'yesProb')
names(predsRFValid)= c('noProb', 'yesProb')

# Add predictions to calibration set
calibration= cbind(calibration, predsRFCalibration)
valid= cbind(valid, predsRFValid)

# RF predictions
ReliabilityDiagram(predsRFValid$yesProb, valid$resp, plot=TRUE)

# Fit GLM on calibration set
# Add RF resp to calibration set
fitGLM= glm(resp~ yesProb, data= calibration, family = binomial())

# Predict on valid set
predsGLM= predict(fitGLM, newdata= valid, type= "response")


# RF predictions
ReliabilityDiagram(predsGLM, valid$resp, plot=TRUE)

ReliabilityDiagram(res$raw.probs, res$responses, plot=TRUE)
ReliabilityDiagram(res$cal.probs, res$responses, plot=TRUE)

# Write out data for Jupyter notebook on DS box


library(data.table)
fwrite(building, file= 'building.csv')
fwrite(calibration, file= 'calibration.csv')
fwrite(valid, file= 'valid.csv')

fwrite(myDat, file= "bankmarketing.csv")
myDat





library(readr)
myDat<- read.csv("Data/bank-full.csv", sep= ";", stringsAsFactors = T)

library(CatEncoders)

# Saving names of categorical variables
factors <- names(which(sapply(myDat, is.factor)))

# Label Encoder
for (i in factors)
{
  encode <- LabelEncoder.fit(myDat[, i])
  myDat[, i] <- transform(encode, myDat[, i])
}


# Convert variable to factor
myDat$job= as.factor(myDat$job)



characters <- names(which(sapply(myDat, is.character)))

factors= c(factors, characters)
factors= characters

factors <- factors[!factors %in% 'y']

factorsTry= factors[2:3]

i= 1
# Label Encoder
for (i in factors){
  encode <- LabelEncoder.fit(myDat[, i])
  myDat[, i] <- transform(encode, myDat[, i])
}

myDat





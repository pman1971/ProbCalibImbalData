https://community.datarobot.com/t5/knowledge-base/platt-scaling/ta-p/163

library(caret)
library(kernlab)

data(spam)

inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE) # creates test/training partitions # returns Training Set Indeces
training <- spam[inTrain,] # Training Set
testing <- spam[-inTrain,] # Test Set
dim(training) 


set.seed(32343) # to allow reproducibility of results
modelFit <- train(type ~.,data=training, method="svmLinear") # Use the 'type' variable as labels; 'training' data to train
modelFit

modelFit <- train(type ~.,data=training, 
                  method="svmLinear", 
                  trControl = trainControl(method = "repeatedcv", 
                                           repeats = 2, 
                                           classProbs =  TRUE))
modelFit$finalModel

predictProbs <- predict(modelFit,newdata=testing, type="prob")
head(predictProbs)


labels <- testing$type
labels <- as.numeric(labels)-1
processed_data <- data.frame(predictProbs[,2],labels)

LOGISTIC_model <- train(labels ~., data=processed_data, method="glm",family=binomial(logit))
LOGISTIC_model$finalModel

LOGISTIC_model$finalModel$coefficients

a= LOGISTIC_model$finalModel$coefficients[[1]]
b= LOGISTIC_model$finalModel$coefficients[[2]]

P= 1/(1 + exp(a + b * x))





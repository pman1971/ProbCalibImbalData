https://stackoverflow.com/questions/63256553/train-validation-test-split-model-in-caret-in-r

### CLEAN WORKSPACE AND SOURCE FUNCTIONS ####
# Clear all objects
rm(list=ls())

# Clear all plots
dev.off()

library(readr)
library(caret)
library(mlr)



data<- read_delim("Data/bank-full.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Examine label
data$response= as.factor(data$y)

data$y= NULL

# Define the partition (e.g. 75% of the data for training)
trainIndex <- createDataPartition(data$response, p = .75, 
                                  list = FALSE, 
                                  times = 1)

# Split the dataset using the defined partition
train_data <- data[trainIndex, ,drop=FALSE]
tune_plus_val_data <- data[-trainIndex, ,drop=FALSE]

# Define a new partition to split the remaining 25%
tune_plus_val_index <- createDataPartition(tune_plus_val_data$response,
                                           p = .6,
                                           list = FALSE,
                                           times = 1)

# Split the remaining ~25% of the data: 40% (tune) and 60% (val)
tune_data <- tune_plus_val_data[-tune_plus_val_index, ,drop=FALSE]
val_data <- tune_plus_val_data[tune_plus_val_index, ,drop=FALSE]

# Outcome of this section is that the data (100%) is split into:
# training (~75%)
# tuning (~10%)
# validation (~15%)

lrn_tune <- setHyperPars(lrn, par.vals = mytune$x)
params2 <- list(booster = "gbtree",
                objective = lrn_tune$par.vals$objective,
                eta=lrn_tune$par.vals$eta, gamma=0,
                max_depth=lrn_tune$par.vals$max_depth,
                min_child_weight=lrn_tune$par.vals$min_child_weight,
                subsample = 0.8,
                colsample_bytree=lrn_tune$par.vals$colsample_bytree)

xgb2 <- xgb.train(params = params2,
                  data = dtrain, nrounds = 50,
                  watchlist = list(val=dtune, train=dtrain),
                  print_every_n = 10, early_stopping_rounds = 50,
                  maximize = FALSE, eval_metric = "error")




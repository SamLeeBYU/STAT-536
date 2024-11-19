source("setup.R")
library(xgboost)
library(ranger)

credit$Class <- factor(credit$Class, levels=0:1)

xgb.params <- read_csv("xgb-best_params.csv")
rf.params <- read_csv("rf-best_params.csv")

#Split the data 70-30 for out-of-sample comparison
p <- 0.7
train.indices <- sample(1:nrow(credit))[1:round(p*nrow(credit))]

#For xgboost
X <- as.matrix(credit[, -ncol(credit)])
Y <- as.numeric(credit$Class) - 1
X.train <- X[train.indices,]
X.test <- X[-train.indices,]

Y.train <- Y[train.indices]
Y.test <- Y[-train.indices]

dtrain <- xgb.DMatrix(data = X.train, label = Y.train)
dtest <- xgb.DMatrix(data = X.test, label = Y.test)

c <- seq(0.01, 0.99, by = 0.01)

xgb.model.train <- xgb.train(
  params = list(
    eta = xgb.params$eta,
    max_depth = xgb.params$max_depth,
    subsample = xgb.params$subsample,
    colsample_bytree = xgb.params$colsample_bytree,
    gamma = xgb.params$gamma,
    objective = "binary:logistic"
  ),
  data = dtrain,
  nrounds = xgb.params$nrounds
)

#In sample prediction
xgb.y.hat.insample <- predict(xgb.model.train, dtrain)

f2.scores <- sapply(c, function(threshold) {
  print(threshold)
  classification.hat <- ifelse(xgb.y.hat.insample > threshold, 1, 0)
  MLmetrics::FBeta_Score(Y.train, classification.hat, beta = 2)
})
c.star <- c[which.max(f2.scores)]
classification.hat.insample <- ifelse(xgb.y.hat.insample > c.star, 1, 0)
xgb.f.score.insample <- MLmetrics::FBeta_Score(Y.train, classification.hat.insample, beta = 2)
#In sample accuracy
xgb.accuracy.insample <- MLmetrics::Accuracy(classification.hat.insample, Y.train)

#Out of sample prediction
xgb.y.hat <- predict(xgb.model.train, dtest)

#Out of sample f-beta score
classification.hat <- ifelse(xgb.y.hat > c.star, 1, 0)
xgb.f.score <- MLmetrics::FBeta_Score(Y.test, classification.hat, beta = 2)
#Out of sample accuracy
xgb.accuracy <- MLmetrics::Accuracy(classification.hat, Y.test)

#For RF
credit.train <- credit[train.indices,]
credit.test <- credit[-train.indices,]

rf.model.train <- ranger(Class ~ ., data = credit.train, num.trees = rf.params[2,]$num.trees, mtry = rf.params[1,]$mtry, min.node.size = rf.params[2,]$min.node.size,
                     sample.fraction = rf.params[2,]$sample.fraction, max.depth = rf.params[2,]$max.depth,
                     importance = "impurity")

rf.y.hat <- predict(rf.model.train, data=credit.test)$predictions

#In-sample f-score
rf.f.score.insample <- MLmetrics::FBeta_Score(credit.train$Class, rf.model.train$predictions, beta=2)
#In-sample accuracy
rf.accuracy.insample <- MLmetrics::Accuracy(rf.model.train$predictions, credit.train$Class)

#Out of sample f-score
rf.f.score <- MLmetrics::FBeta_Score(credit.test$Class, rf.y.hat, beta=2)
#Out of sample accuracy
rf.accuracy <- MLmetrics::Accuracy(rf.y.hat, credit.test$Class)


###############################################################################

#Full XGBoost Model
dX <- xgb.DMatrix(data = X, label = Y)
xgb.model <- xgb.train(
  params = list(
    eta = xgb.params$eta,
    max_depth = xgb.params$max_depth,
    subsample = xgb.params$subsample,
    colsample_bytree = xgb.params$colsample_bytree,
    gamma = xgb.params$gamma,
    objective = "binary:logistic"
  ),
  data = dX,
  nrounds = xgb.params$nrounds
)
xgb.y.hat <- predict(xgb.model, dX)
f2.scores <- sapply(c, function(threshold) {
  print(threshold)
  classification.hat <- ifelse(xgb.y.hat > threshold, 1, 0)
  MLmetrics::FBeta_Score(Y, classification.hat, beta = 2)
})
c.star <- c[which.max(f2.scores)]

#Predict the unknown entries
unknown <- read_csv("IsFraudulent.csv")[,-1] 
dunknown <- xgb.DMatrix(data = as.matrix(unknown))
unknown.predictions <- predict(xgb.model, dunknown)
unknown.classification <- ifelse(unknown.predictions > c.star, 1, 0)
unknown.classification

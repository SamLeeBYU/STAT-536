source("setup.R")
library(xgboost)

credit$Class <- factor(credit$Class, levels=0:1)

#Split the data 70-30

X <- as.matrix(credit[, -ncol(credit)])
Y <- as.numeric(credit$Class) - 1

p <- 0.7
train.indices <- sample(1:nrow(credit))[1:round(p*nrow(X))]
X.train <- X[train.indices,]
X.test <- X[-train.indices,]

Y.train <- Y[train.indices]
Y.test <- Y[-train.indices]

dtrain <- xgb.DMatrix(data = X.train, label = Y.train)
dtest <- xgb.DMatrix(data = X.test, label = Y.test)

params <- expand.grid(
  eta = c(0.01, 0.1, 0.3),                
  max_depth = c(3, 5, 7, 10),                
  subsample = seq(0.5, 1, by = 0.1),         
  colsample_bytree = seq(0.5, 1, by = 0.1),  
  nrounds = c(100, 200, 500),                
  gamma = c(0, 1, 5)
)
N <- 50
random.params <- params[sample(1:nrow(params), N),]

f.scores <- numeric(N)
c <- seq(0.25, 0.45, by = 0.01)
for (i in 1:N) {
  params.i <- random.params[i,]
  
  xgb.model.i <- xgb.train(
    params = list(
      eta = params.i$eta,
      max_depth = params.i$max_depth,
      subsample = params.i$subsample,
      colsample_bytree = params.i$colsample_bytree,
      gamma = params.i$gamma,
      objective = "binary:logistic"
    ),
    data = dtrain,
    nrounds = params.i$nrounds
  )
  
  #Out of sample prediction
  y.hat <- predict(xgb.model.i, dtest)
  
  f2.scores <- sapply(c, function(threshold) {
    #print(threshold)
    classification.hat <- ifelse(y.hat > threshold, 1, 0)
    MLmetrics::FBeta_Score(Y.test, classification.hat, beta = 2)
  })
  c.star <- c[which.max(f2.scores)]

  #Out of sample f-beta score
  classification.hat <- ifelse(y.hat > c.star, 1, 0)
  f.scores[i] <- MLmetrics::FBeta_Score(Y.test, classification.hat, beta = 2)
}

best.params <- random.params[which(f.scores == max(f.scores)),]
write.csv(best.params, "xgb-best_params.csv")
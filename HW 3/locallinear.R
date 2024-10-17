library(caret)
source("partial-regression.R")

#Perform Dimension Reduction via Partialling Out so we can do Loess with these Factors
loc.factors <- c("Income")
partialled <- partial.out(X, Y, loc.factors)

X.p <- partialled$X
Y.p <- as.matrix(partialled$Y)

colnames(X.p) <- loc.factors
colnames(Y.p) <- "Score"

local.linear.cv <- function(X, Y, k.folds=5){
  #Grid of span parameters we want to search
  s = seq(from=0.1, to=1, length.out = 1000)
  
  k.spans = 100
  spans = sample(s, k.spans)
  
  rmse.spans <- matrix(nrow = k.folds, ncol=k.spans)
  folds <- createFolds(Y, k = k.folds, list = TRUE, returnTrain = TRUE)
  for(i in 1:length(spans)){
    span = spans[i]
    for (k in 1:k.folds) {
      
      train_index <- folds[[k]]
      test_index <- setdiff(1:nrow(X), train_index)
      
      X_train <- X[train_index, ]
      Y_train <- Y[train_index]
      X_test <- X[test_index, ]
      Y_test <- Y[test_index]
      
      train.model <- loess(Y_train ~ X_train, degree = 2, span=span)
      rmse.spans[k, i] = sqrt(mean((Y_test-predict(train.model, X_test))^2, na.rm = T))
    }
  }
  
  span.min = median(spans[which(colMeans(rmse.spans) == min(colMeans(rmse.spans)))])
  return(span.min)
}
span.optimal = local.linear.cv(X.p, Y.p)
#Best Span = 0.309
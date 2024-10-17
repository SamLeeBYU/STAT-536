library(caret)

covariates = get_covariates()
X <- covariates$X
Y <- covariates$Y

partialled <- partial.out(X, Y, loc.factors)

X.p <- partialled$X
Y.p <- as.matrix(partialled$Y)

#Compare the three models: Linear Regression, GAM, Local Linear
k.folds = 20
folds <- createFolds(Y, k = k.folds, list = TRUE, returnTrain = TRUE)
rmse <- matrix(nrow=k.folds, ncol=3)
for (k in 1:k.folds) {
  
  train_index <- folds[[k]]
  test_index <- setdiff(1:nrow(X), train_index)
  
  X_train <- X[train_index, ]
  Y_train <- Y[train_index]
  X_test <- X[test_index, ]
  Y_test <- Y[test_index]
  
  #For local linear regression
  X_train.p <- X.p[train_index, ]
  Y_train.p <- Y.p[train_index, ]
  X_test.p <- X.p[test_index, ]
  Y_test.p <- Y.p[test_index, ]
  
  #For GAM
  Y_train.GAM <- Y_train
  X_train.GAM <- X[train_index, -6]
  X_test.GAM <- X[test_index, -6]
  
  #Linear Regression
  train.model.linear <- lm(Y_train ~ X_train)
  predictions.linear <- cbind(1,X_test)%*%coef(train.model.linear)
  
  #Local Linear Regression
  train.model.loc <- loess(Y_train.p ~ X_train.p, degree=2, span=span.optimal)
  predictions.loc <- predict(train.model.loc, X_test.p)
  
  #GAM
  train.model.gam <- gam(gam.optimal.train)
  X.basis <- build_X_test_basis(train.model.gam, X_test.GAM)
  predictions.gam <- cbind(1, X.basis)%*%coef(train.model.gam)
  
  rmse[k, 1] = sqrt(mean((Y_test-predictions.linear)^2, na.rm = T))
  rmse[k, 2] = sqrt(mean((Y_test.p-predictions.loc)^2, na.rm = T))
  rmse[k, 3] = sqrt(mean((Y_test-predictions.gam)^2, na.rm = T))
}
models.RMSE <- colMeans(rmse)

#GAM has marginally better out of sample performance than the linear model fit, but the linear model is more parsimonious

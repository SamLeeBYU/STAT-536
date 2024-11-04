#Use K-fold cross validation to measure the out-of-sample fit of the three models

K <- 5
#Since we have 3 models
models.f1 <- models.auc <- matrix(nrow=K, ncol=3)
colnames(models.f1) <- colnames(models.auc) <- c("LASSO", "Probit", "PCR")
folds <- createFolds(Y, k = K, list = TRUE, returnTrain = TRUE)

for(k in 1:K){
  train.index <- folds[[k]]
  test.index <- setdiff(1:nrow(X), train.index)
  
  X.train <- X[train.index,]
  Y.train <- Y[train.index]
  X.test <- X[test.index,]
  Y.test <- Y[test.index]
  
  #For PCR
  X.train.pcr <- X.scaled[train.index,]
  X.test.pcr <- X.scaled[test.index,]
  
  #Fit the lasso model
  lasso.model <- glmnet(X.train, Y.train, lambda = lambda, alpha = 1,
                  family="binomial",
                  thresh = 1e-5,
                  maxit = 1e+02)
  
  lasso.y.hat <- as.numeric(invlogit(predict(lasso.model, newx = X.test)) > lasso.c.star)
  
  #Fit the probit model
  probit.model <- glm(Y.train ~ X.train, family=binomial(link="probit"))
  
  probit.predictions <- pnorm(cbind(1,X.test)%*%coef(probit.model))
  
  probit.y.hat <- as.numeric(probit.predictions > probit.c.star)
  
  #Fit the PCR model
  Z.train <- X.train.pcr %*% V[,1:3]
  gamma.hat <- matrix(coef(glm(Y.train ~ Z.train - 1, family="binomial")))
  Z.test <- X.test.pcr%*%V[,1:3]
  
  pcr.y.hat <- as.numeric(invlogit(Z.test%*%gamma.hat) > pcr.c.star)
  
  #Evaluate the out-of-sample fits
  models.f1[k,1] <- F1_Score(Y.test, lasso.y.hat)
  models.f1[k,2] <- F1_Score(Y.test, probit.y.hat)
  models.f1[k,3] <- F1_Score(Y.test, pcr.y.hat)
  
  models.auc[k,1] <- roc(as.vector(Y.test), lasso.y.hat)$auc
  models.auc[k,2] <- roc(as.vector(Y.test), probit.y.hat)$auc
  models.auc[k,3] <- roc(as.vector(Y.test), pcr.y.hat)$auc
}
# colMeans(models.f1)
# colMeans(models.auc)


out_of_sample <- data.frame(
  Model = c("LASSO", "Probit", "PCR"),
  `Out-of-Sample F1` = colMeans(models.f1, na.rm=T),
  `Out-of-Sample AUC` = colMeans(models.auc)
)

in_sample <- data.frame(
  Model = c("LASSO", "Probit", "PCR"),
  `In-Sample F1` = c(lasso.insample["F1"], probit.insample["F1"], pcr.insample["F1"]),
  `In-Sample AUC` = c(lasso.insample["AUC"], probit.insample["AUC"], pcr.insample["AUC"])
)

library(MLmetrics)

model.logit <- cv.glmnet(X, Y, family = "binomial", alpha = 1)
lambda <- model.logit$lambda.1se

cutoffs <- seq(0, 1, by=0.01)
lasso.predicted <- predict(model.logit, newx=X, type="response")
youden.lasso <- sapply(cutoffs, get_youden, Y, lasso.predicted)

lasso.c.star <- cutoffs[which.max(youden.lasso)]

y.hat.lasso <- as.numeric(lasso.predicted > lasso.c.star)

lasso.insample <- c(
  "F1" = F1_Score(Y, y.hat.lasso),
  "AUC" = roc(as.numeric(Y), y.hat.lasso)$auc
)


lasso.f1.validate <- function(){
  
  #Cross validate to find best cutoff value, c
  c = seq(0, 1, length.out=1000)
  n.c <- 50
  r.c <- sample(c, n.c)
  lasso.f1 <- numeric(n.c)
  
  K <- 5
  folds <- createFolds(Y, k = K, list = TRUE, returnTrain = TRUE)
  
  for(i in 1:n.c){
    print(i)
    f1.fold <- numeric(K)
    for(k in 1:K){
      train.index <- folds[[k]]
      test.index <- setdiff(1:nrow(X), train.index)
      
      X.train <- X[train.index,]
      Y.train <- Y[train.index]
      X.test <- X[test.index,]
      Y.test <- Y[test.index]
      
      model <- glmnet(X.train, Y.train, lambda = lambda, alpha = 1,
                      family="binomial",
                      thresh = 1e-5,
                      maxit = 1e+02)
      
      y.hat <- as.numeric(invlogit(predict(model, newx = X.test)) > r.c[i])
      f1.fold[k] <- F1_Score(Y.test, y.hat)
    }
    lasso.f1[i] <- mean(f1.fold, na.rm=T)
  }
  c.star <- r.c[which(lasso.f1 == max(lasso.f1))]
  
  y.hat <- as.numeric(invlogit(predict(model.logit, newx = X)) > c.star)
  F1_Score(Y, y.hat)
}
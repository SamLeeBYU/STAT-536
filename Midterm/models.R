library(caret)

calculate.r.squared <- function(model, Y, X=NULL, adjusted=F, lasso=F){
  Y.bar <- mean(Y)
  TSS <- sum((Y-Y.bar)^2)
  if(lasso){
    Y.hat = predict(model, newx=X)
  } else {
    Y.hat = predict(model)
  }
  RSS <- sum((Y.hat-Y.bar)^2)
  r.squared <- RSS/TSS
  k = length(coef(model))
  if(adjusted){
    return(1 - (1-r.squared)*(length(Y)-1)/(length(Y)-k))
  } else{
    return(r.squared)
  }
}

rmse <- function(X, Y, lambda=lasso.lambda, W=NULL, model=c("lasso", "lm", "spatial")){
  n <- nrow(Y)
  y.hat <- numeric(n)
  for(i in 1:n){
    indices <- (1:n)[-i]
    Y.train <- matrix(Y[indices])
    X.train <- as.matrix(X[indices,])
    Y.test <- Y[i]
    X.test <- as.matrix(X[i,,drop=F])
    
    if(model == "lasso"){
      train.model <- glmnet(X.train, Y.train, lambda=lambda, alpha=1)
      y.hat[i] = predict(train.model, newx=X.test)
    }
    
    if(model == "lm"){
      train.model <- lm(Y.train ~ X.train)
      y.hat[i] = cbind(1,X.test)%*%coef(train.model)
    }
    
    if(model == "spatial"){
      W.train <- W[indices, indices]
      W.test <- W[i, i, drop=F]
      train.model <- lagsarlm(Y.train ~ X.train - 1, listw=mat2listw(W.train, style = "W"))

      rho_hat <- coef(train.model)["rho"]
      beta_hat <- coef(train.model)[-1]
      
      y.hat[i] <- rho_hat * W[i,-i]%*%Y.train + X.test %*% beta_hat
    }
  }
  return(sqrt(mean((y.hat-Y)^2)))
}

lasso.r.squared <- calculate.r.squared(lasso.model, log(Y.cleaned), X=lasso.X, adjusted=T, lasso=T)
lasso.rmse <- rmse(lasso.X, log(Y.cleaned), model="lasso")
x.star <- lasso.run$X.full[115:nrow(trees),]
lasso.predictions <- exp(predict(lasso.model, newx=x.star))

lasso.r.squared
lasso.rmse

spatial.r.squared <- calculate.r.squared(model.spatial, log(Y.cleaned), adjusted=T)
spatial.rmse <- rmse(model.spatial$X, log(Y.cleaned), model="spatial", W=listw2mat(W))

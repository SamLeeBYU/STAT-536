library(caret)

X.scaled <- cbind(1, scale(X)[,-1])
svd.X <- svd(X.scaled)
U <- svd.X$u; D <- diag(svd.X$d); V <- svd.X$v

K <- 5
folds <- createFolds(Y, k = K, list = TRUE, returnTrain = TRUE)
n <- nrow(X.scaled)

cutoff <- seq(0, 1, length.out=100)
max.k <- min(dim(X.scaled))

params <- expand.grid(
  cutoff = cutoff,
  k = 1:20
)

N = 40

r.params <- params[sample(1:nrow(params), N),]

f1.cv <- numeric(N)

for(i in 1:N){
  print(i)
  f1.fold <- numeric(K)
  for(k in 1:K){
    train.index <- folds[[k]]
    test.index <- setdiff(1:nrow(X.scaled), train.index)
    
    X.train <- X.scaled[train.index,]
    Y.train <- Y[train.index]
    X.test <- X.scaled[test.index,]
    Y.test <- Y[test.index]
    
    Z.train <- X.train %*% V[,1:r.params[i,]$k]
    gamma.hat <- matrix(coef(glm(Y.train ~ Z.train - 1, family="binomial")))
    Z.test <- X.test%*%V[,1:r.params[i,]$k]
    Y.hat <- as.numeric(invlogit(Z.test%*%matrix(gamma.hat)) > r.params[i,]$cutoff)
    
    tp = sum(Y.hat[which(Y.test==1)])
    fp = sum(Y.hat[which(Y.test==0)])
    fn = length(Y.hat[which(Y.test==1)])-tp
    
    f1.fold[k] = 2*tp/(2*tp + fp + fn)
  }
  f1.cv[i] <- mean(f1.fold)
}


r.params[which(f1.cv == max(f1.cv)),]

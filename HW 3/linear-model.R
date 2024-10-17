covariates = get_covariates()
X <- covariates$X
Y <- covariates$Y

model.full <- lm(Y ~ X)

X.reduced <- X[,colnames(X) != "Income.squared"]
model.reduced <- lm(Y ~ X.reduced)

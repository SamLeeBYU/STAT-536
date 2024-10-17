library(tidyverse)
library(glmnet)

get_covariates <- function(){
  
  schools <- read_table("SchoolResults.txt")
  
  n <- nrow(schools)
  Y <- matrix(schools$Score, nrow=n, ncol=1)
  schools$Income.squared <- schools$Income^2
  X <- scale(schools[,2:ncol(schools)])
  
  schools.lasso <- cv.glmnet(X, Y, alpha=1, nfolds = 10)
  
  covariates <- names(coef(schools.lasso)[which(coef(schools.lasso) != 0),])
  X.covariates <- X[,colnames(X) %in% covariates]
  
  return(list(
    covariates = covariates,
    X = X.covariates,
    Y = Y
  ))
}

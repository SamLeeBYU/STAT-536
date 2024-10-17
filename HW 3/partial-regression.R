schools <- read_table("SchoolResults.txt")

n <- nrow(schools)
Y <- matrix(schools$Score, nrow=n, ncol=1)
schools$Income.squared <- schools$Income^2
X <- scale(schools[,2:ncol(schools)])

partial.out <- function(X, Y, factors){
  # Covariates (excluding the factors to be partialled out)
  Z <- X[, !(colnames(X) %in% factors)]
  
  # Partial out Y
  Y.tilde <- Y - Z %*% (solve(t(Z) %*% Z) %*% t(Z) %*% Y)
  
  # Partial out each factor
  factors.tilde <- NULL  # Initialize an empty matrix to store partialled out factors
  
  for(j in 1:length(factors)){
    factor <- as.matrix(X[, factors[j]])
    factor.tilde <- factor - Z %*% (solve(t(Z) %*% Z) %*% t(Z) %*% factor)
    factors.tilde <- cbind(factors.tilde, factor.tilde)  # Combine the partialled-out factors
  }
  
  # Return the partialled out Y and factors
  return(list(
    Y = Y.tilde,
    X = factors.tilde
  ))
}

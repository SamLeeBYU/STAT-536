library(glmnet)

get_covariates <- function(data, y, factors=c("LON", "LAT", "Slope", "Aspect", "ELEV")){
  
  n <- nrow(data)
  
  X.full <- model.matrix(as.formula(str_c(" ~ (poly(", factors[1], ", 2)", paste0(str_c(" + poly(", factors[2:length(factors)], ", 2)"), collapse = ""), ")^2")), data=data)[,-1]
  X.truncated <- X.full[1:114,]
  
  model.lasso <- cv.glmnet(X.truncated, y, alpha=1, nfolds = 10)
  
  clean_poly_terms <- function(term) {
    term <- gsub("poly\\(([^,]+),\\s*([0-9]+)\\)([0-9]+)", 
                 "poly(\\1, \\2, raw = TRUE)[,\\3]", 
                 term)
    return(term)
  }
  covariates <- names(coef(model.lasso)[which(coef(model.lasso) != 0),])
  cleaned.covariates <- sapply(covariates[2:length(covariates)], clean_poly_terms)
  lasso.formula <- as.formula(str_c("log(Lodgepole) ~ ", cleaned.covariates[1], paste0(str_c(" + ", cleaned.covariates[2:length(cleaned.covariates)]), collapse = "")))
  
  
  return(list(
    X.full = X.full,
    X.features = X.truncated,
    formula = lasso.formula,
    model = model.lasso,
    lasso.covariates = covariates
  ))
}
lasso.run <- get_covariates(trees, log(Y.cleaned))
lasso.formula <- lasso.run$formula
lasso.X <- lasso.run$X.features
lasso.model <- lasso.run$model
lasso.lambda = lasso.model$lambda.1se
lasso.covariates = lasso.run$lasso.covariates

#Bootstrap
lasso.bootstrap <- function(B=10000, K=nrow(lasso.X), lambda=lasso.lambda, X=lasso.X, Y=log(Y.cleaned), model=lasso.model){
  beta.b <- matrix(nrow=(1+ncol(X)), ncol=B)
  for(b in 1:B){
    observations.b <- sample(1:length(Y), size=K, replace = T)
    Y.b <- Y[observations.b]
    X.b <- X[observations.b,]
    lasso.b <- glmnet(X.b, Y.b, lambda=lambda, alpha=1)
    beta.b[,b] <- as.vector(coef(lasso.b))
  }
  #How often each covariate is included
  beta.importance <<- (abs(beta.b) > 0) %>% ifelse(1, 0) %>% rowMeans()
  
  beta.bar <- beta.b %>% rowMeans()
  se.bar <- numeric(nrow(beta.b))
  for(j in 1:nrow(beta.b)){
    se.bar[j] <- sum((beta.b[j,]-beta.bar[j])^2)
  }
  #Bootstrap standard errors
  lasso.se <- sqrt((1/(B-1))*se.bar)
  
  covariates.indices <<- c(1, 1+which(colnames(X) %in% lasso.covariates))
  
  #How often coefficients were included in B bootstraps
  #beta.importance[covariates.indices]
  
  #95% Centered Confidence Intervals
  lasso.beta <<- as.vector(coef(model))
  beta.ci.centered.lower <- 2 * lasso.beta - apply(beta.b, 1, function(x) quantile(x, probs = 0.975))
  beta.ci.centered.upper <- 2 * lasso.beta - apply(beta.b, 1, function(x) quantile(x, probs = 0.025))
  
  beta.ci.centered <<- cbind(beta.ci.centered.lower, beta.ci.centered.upper)
  #beta.ci.centered[covariates.indices,]
}

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

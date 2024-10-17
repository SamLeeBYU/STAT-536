library(ggplot2)
library(splines)

# Read data
data <- read.table("AgricultureWater.txt", header=T)
n <- nrow(data)
Y = data[,1]
#This is just a vector
X = data[,2:ncol(data)]

# Degree of polynomial for linear terms
X.poly <- matrix(1, nrow=n, ncol=1)

# Add polynomial terms (for the linear and quadratic part)
for(i in 1:3){
  X.poly <- cbind(X.poly, X^i)
}

# Choose number of interior knots for the spline
K <- 12
# Create knots uniformly over the domain of X
split <- rep(round(max(X) - min(X), 1)/(K-3), K-3)
l <- cumsum(split)
l <- l[1:(length(l) - 1)] # These are the internal knots

# Create the cubic spline basis like before
cubic.splines <- t(sapply(X.poly[,2], function(x.0){
  x.0 > l
})) *
  t(sapply(X.poly[,2], function(x.0){
    (x.0 - l)^3
  }))


natural.splines <- ns(X, df=K-4)
model.natural <- lm(Y ~ natural.splines)

P.cubic <- cbind(X.poly, cubic.splines)
# Estimate coefficients using least squares
beta.hat.cubic <- solve(t(P.cubic) %*% P.cubic) %*% t(P.cubic) %*% Y

# Predict Y values
Y.hat.natural <- model.natural$fitted.values
Y.hat.cubic <- P.cubic %*% beta.hat.cubic

data$Predicted.natural <- Y.hat.natural
data$Predicted.cubic <- Y.hat.cubic

# Plot actual data and predictions
ggplot(data, aes(x = cwsi, y = swc)) +
  geom_point() +
  geom_line(aes(y = Predicted.cubic), color = "red")+
  geom_line(aes(y = Predicted.natural), color = "blue")

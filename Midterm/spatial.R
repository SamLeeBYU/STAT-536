library(geosphere)
library(spdep)
library(spatialreg)

n <- nrow(trees)
d.matrix <- matrix(NA, n, n)

for (i in 1:n) {
  for (j in 1:n) {
    if (i != j) {
      d.matrix[i, j] <- distHaversine(
        c(trees$LON[i], trees$LAT[i]),
        c(trees$LON[j], trees$LAT[j])
      )
    } else {
      d.matrix[i, j] <- 0
    }
  }
}

weights = 1/d.matrix
weights[is.infinite(weights)] = 0
 
# spatial.cor = numeric(length(colnames(trees))-2)
# 
# for(v in 3:nrow(spatial.matrix)){
#   x = na.omit(pull(trees, v))
#   n = length(x)
#   x.bar = mean(x, na.rm=T)
#   x.dev = matrix(x-x.bar)
#   W.M = weights[1:n,1:n]
#   W = sum(W.M)
# 
#   cor <- n/W*t(x.dev)%*%W.M%*%x.dev/sum(x.dev^2)
# 
#   spatial.cor[v-2] <- cor
# }
# names(spatial.cor) <- colnames(trees)[3:6]
# spatial.cor
# 
# moran.test(trees$ELEV, mat2listw(weights, style = "W"))

trees.cleaned <- trees %>% na.omit()
n = nrow(Y.cleaned)
weights.cleaned <- weights[1:n, 1:n]
W = mat2listw(weights.cleaned, style = "W")
model.spatial <- lagsarlm(log(Lodgepole) ~ Slope + Aspect + ELEV + I(Aspect^2) +
                                           I(Slope^2) + I(ELEV^2), data=trees.cleaned,
                          listw = W)
rho <- coef(model.spatial)[1]
Sigma.hat <- model.spatial$s2*solve(diag(n) - rho*listw2mat(W))%*%solve(diag(n) - rho*t(listw2mat(W)))

# rho = coef(model.spatial)[1]
# X.spatial = cbind(1, as.matrix(trees[1:n,c("Slope", "Aspect", "ELEV")]))
# beta.hat = coef(model.spatial)[2:5]
# Sigma.hat = model.spatial$s2*solve(diag(n) - rho*listw2mat(W))%*%solve(diag(n) - rho*listw2mat(W))
# y.hat = solve(diag(n) - rho * listw2mat(W)) %*% (X.spatial %*% beta.hat)

# hist(y.hat-predict(model.spatial))
# 
# Calculate the mean of the observed (log-transformed) values
# Y.bar <- mean(log(Y.cleaned))
# 
# # Total Sum of Squares (TSS)
# TSS <- sum((log(Y.cleaned) - Y.bar)^2)
# 
# # Residual Sum of Squares (RSS)
# RSS <- sum((log(Y.cleaned) - predict(model.spatial))^2)
# 
# # R^2
# r.squared <- 1 - RSS / TSS
# r.squared


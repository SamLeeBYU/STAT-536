library(MLmetrics)

model.probit <- glm(Y ~ X, family=binomial(link="probit"))

cutoffs <- seq(0, 1, by=0.01)
youden.probit <- sapply(cutoffs, get_youden, Y, predict(model.probit, type="response"))

probit.c.star <- cutoffs[which.max(youden.probit)]

y.hat.probit <- as.numeric(model.probit$fitted.values > probit.c.star)

probit.insample <- c(
  "F1" = F1_Score(Y, y.hat.probit),
  "AUC" = roc(as.numeric(Y), y.hat.probit)$auc
)

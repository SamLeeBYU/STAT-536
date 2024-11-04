library(glmnet)
library(arm)

bank <- read_delim("TargetedMarketing.csv", delim=";")

bank <- mutate(bank,
               y = y == "yes",
               contacted = pdays < 999
)

X <- model.matrix( ~ I(pdays*(contacted)) + I(1-contacted) +
                     age + job + marital + education + default + housing + loan + 
                     contact + month + day_of_week + campaign + previous + poutcome, data=bank)[,-1]
X <- X[,!(colnames(X) %in% "loanunknown")]

Y <- matrix(as.numeric(bank$y), nrow=nrow(bank))

get_youden <- function(cutoff, Y, y.hat) {
  predicted_labels <- ifelse(y.hat > cutoff, 1, 0)
  
  tp <- sum(predicted_labels == 1 & Y == 1)  # True positives
  tn <- sum(predicted_labels == 0 & Y == 0)  # True negatives
  fp <- sum(predicted_labels == 1 & Y == 0)  # False positives
  fn <- sum(predicted_labels == 0 & Y == 1)  # False negatives
  
  sensitivity <- tp / (tp + fn)
  specificity <- tn / (tn + fp)
  
  # Youden's Index
  youden_index <- sensitivity + specificity - 1
  return(youden_index)
}

#Youden, W.J. (1950), Index for rating diagnostic tests. Cancer, 3: 32-35. https://doi.org/10.1002/1097-0142(1950)3:1<32::AID-CNCR2820030106>3.0.CO;2-3
library(glmnet)
library(arm)

bank <- read_delim("TargetedMarketing.csv", delim=";")

bank <- mutate(bank,
               y = y == "yes",
               contacted = pdays < 999
)

X <- model.matrix( ~ I(pdays*(contacted)) + I(1-contacted) +
                     age + job + marital + education + default + housing + loan + 
                     contact + month + day_of_week + campaign + previous + poutcome, data=bank)
X <- X[,!(colnames(X) %in% "loanunknown")]

Y <- matrix(as.numeric(bank$y), nrow=nrow(bank))
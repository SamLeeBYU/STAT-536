credit <- read_csv("CCFraud.csv")[,-1]

#Out of sample data we need to predict
fraud <- read_csv("IsFraudulent.csv")

pc.covariates <- str_c("V", 1:28)

run_logit <- function(bootstrap=F){
  logit.model <<- glm(Class ~ ., family=binomial(link="logit"),
                     data=credit)
  
  if(bootstrap){
    n = 100
    data.control <- data.frame(
      Amount = seq(min(credit$Amount), max(credit$Amount), length.out = n)
    ) %>% cbind(matrix(colMeans(credit[,pc.covariates]), nrow=n, ncol=length(pc.covariates), byrow=T))
    colnames(data.control) <- c("Amount", pc.covariates)
    
    B = 100
    
    p.hat <- matrix(nrow=B, ncol=n)
    
    for(b in 1:B){
      print(b)
      data.b = credit[sample(nrow(credit), replace=T),]
      logit.model.b <- glm(Class ~ ., family=binomial(link="logit"),
                           data=data.b)
      p.hat.b <- predict(logit.model.b, newdata = data.control, type = "response")
      p.hat[b,] <- p.hat.b
    }
    
    p.estimates <- as.data.frame(t(apply(p.hat, 2, quantile, probs = c(0.025, 0.5, 0.975))))
    colnames(p.estimates) <- c("Lower", "Estimate", "Upper")
    
    write_csv(cbind(data.control, p.estimates), "p-estimates.csv")
  }
}
#run_logit()
library(tidyverse)
library(performance)

credit <- read_csv("CreditDebt.csv")
colnames(credit)
covariates <- c("Income", "Limit", "Cards", "Age", "Education", "Gender", "Student", "Married", "Ethnicity")
cor(matrix(c(credit$Rating, credit$Balance, credit$Limit), ncol=3))

credit$Ethnicity <- factor(credit$Ethnicity, levels = unique(credit$Ethnicity))

credit.lm <- lm(Balance ~ ., data = credit[, c("Balance", covariates)])
summary(credit.lm)


# Define the full model with all covariates
full_model <- lm(Balance ~ ., data = credit[, c("Balance", covariates)])

# Start with the null model (intercept only)
null_model <- lm(Balance ~ 1, data = credit)

# Perform forward stepwise selection
stepwise_model <- step(null_model, scope = list(lower = null_model, upper = full_model), 
                       direction = "forward")

n <- nrow(credit)

# Perform backward stepwise selection
stepwise_model <- step(full_model, direction = "backward",
                       k = log(n))

# View the selected model summary
summary(stepwise_model)

# Perform stepwise selection in both directions
stepwise_model <- step(full_model, direction = "both",
                       k = log(n))

plot(stepwise_model$residuals, stepwise_model$fitted.values)

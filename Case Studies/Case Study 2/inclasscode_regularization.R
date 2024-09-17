library(tidyverse)
library(graphics)

### Load data
# It's not a csv, so we use read.table and have to tell it that spaces seperate the entries and there is a header row of column names
gene = read.table('GeneExpression-reduced.txt',sep = ' ',header=T)

correlation_matrix <- cor(gene, use = "complete.obs")

# Get the row and column indices where the condition is met
high.cor.indices <- which(abs(correlation_matrix) >= 0.95 & abs(correlation_matrix) != 1, arr.ind = TRUE)

# Extract the corresponding column names (gene names) from the indices
high.cor.genes <- unique(c(rownames(correlation_matrix)[high.cor.indices[,1]],
                           colnames(correlation_matrix)[high.cor.indices[,2]]))

# Take random sample of highly correlated factors
n <- 10
subset <- high.cor.genes[sample(1:length(high.cor.genes), n)]
gene.subset <- gene[,subset]

heatmap(cor(gene.subset),
        col = colorRampPalette(c("#21AECF", "white", "#CC093E"))(256),
        scale = "none",
        main = "Correlation Heatmap")

### Run a full model regression
full.model = lm(Malignant ~ ., data=gene)
# summary(fullmodel) is full of NAs because p>n !

### Variable Selection
# Backward stepwise won't work as we can't fit the fullmodel
# So we could try forward stepwise selection, but the package we used last time won't work because full.model isn't actually fit... 
library(MASS)
forward.model = stepAIC(full.model,direction = "forward")
# note the errors. 
# There are other packages, but the sheer size of p>>n causes issues and warnings. 

### Regularization/penalized regression
library(glmnet)
X = gene[,101:110] #using only a subset so we can visualize
lasso.model = glmnet(x=X,y=gene$Malignant, alpha = 1)
ridge.model = glmnet(x=X,y=gene$Malignant, alpha = 0)
ridge.model = glmnet(x=scale(X),y=gene$Malignant, alpha = 0)

# We can plot to see how the coefficients are shrunk/selected, 
plot(ridge.model)
plot(lasso.model)
# but note that neither of the axes are lambda

# these fit a variety of lambda values, but we can only use one in the final model. Which one? 
# the model gives deviance, and we could take lambda with the best deviance: 
plot(lasso.model$lambda,lasso.model$dev.ratio)
plot(ridge.model$lambda,ridge.model$dev.ratio)
# looks like lambda=0 is the best??? but that's just regression! Deviance is kinda like R2, it's not going to get better by removing variables. 

### Cross-validation!
lasso.cv = cv.glmnet(x=gene[,-1],y=gene$Malignant, alpha = 1) #won't work as x must be a matrix
X = as.matrix(gene[,-1],nrow = nrow(gene))
lasso.cv = cv.glmnet(x=X,y=gene$Malignant, alpha = 1)
plot(lasso.cv)
# ?cv.glmnet suggests two possible lambdas
lasso.cv$lambda.min
lasso.cv$lambda.1se #note this is default for predict() and coef() functions. 

### get coefficients:
betahats = coef(lasso.cv)
sum(betahats!=0) #only 26 nonzero coefficients: much more manageable than 5000! 
# where are the p-values...?


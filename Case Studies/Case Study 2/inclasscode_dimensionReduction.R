### Load data
# It's not a csv, so we use read.table and have to tell it that spaces seperate the entries and there is a header row of column names
gene = read.table('GeneExpression-reduced.txt',sep = ' ',header=T)


### Dimension Reduction - there are several ways to do this in R. Here's one.
library(pls)
X = as.matrix(gene[,-1],nrow = nrow(gene)) # X is a matrix of all but the first column of gene

# `scale=TRUE` will make sure you scale the explanatory variables  
# The `validation` argument will run some cross-validation for you to determine the number of components to use. 
model.cv = pcr(Malignant ~ X, data=gene, validation="CV", scale=T)

# summary(pcr_obj) prints out some nice summary statistics to help you choose the number of components
summary(model.cv)

# validationplot(pcr_obj)is a visual way of determining the number of components.
validationplot(model.cv)
plot(model.cv, "validation", val.type = "R2", legendpos = "bottomright") # R >= 2.1.0

# fits a principal component regression model with at most `ncomp` components.  
model.2 = pcr(Malignant ~ X, data=gene, ncomp=2, scale=F)

model.2$loadings

predict(model.2)

# `plsr()` follows the same syntax as `pcr` but does partial least squares instead.

model.2$projection
crossprod(model.2$projection[,1], model.2$projection[,2])

 
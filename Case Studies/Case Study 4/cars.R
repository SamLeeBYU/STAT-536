library(tidyverse)
library(glmnet) 
library(pROC)
library(caret)

cars <- read_table("crash.txt")  

cat.var <- c("LGT_COND", "WEATHER", "ALCOHOL",
             "TYP_INT", "REST_USE", "VTRAFWAY",
             "VNUM_LAN", "VSPD_LIM", "VALIGN",
             "VSURCOND", "AIR_BAG")
cont.var <- c("HOUR")

for(k in 1:length(cat.var)){
  cars[[cat.var[k]]] <- factor(cars[[cat.var[k]]])
}

cars <- cars %>%
  mutate(
    HOUR = sin((HOUR - 12) * pi / 12)
  )

X <- model.matrix(as.formula(str_c(" ~ ", 
                                   paste(c(cont.var, cat.var), collapse = " + "))),
                  data=cars)[,-1]
Y <- as.numeric(cars$SEVERITY == 1)

model.lasso <- cv.glmnet(X, Y, family="binomial", alpha=1)
rownames(coef(model.lasso))[which(coef(model.lasso) > 0)]

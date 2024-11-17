library(ranger)

credit$Class <- factor(credit$Class, levels=0:1)

params <- expand.grid(
  mtry = sample(1:floor(sqrt(ncol(credit))), 5),
  min.node.size = c(1, 5, 10, 20),
  sample.fraction = seq(0.5, 1, by=0.1),
  num.trees = c(100, 200, 500),
  max.depth = c(10, 15, 20, 30)
)
N = 50 #Number of total RF models to select and fit
random.params <- params[sample(1:nrow(params), N),]

f.scores <- numeric(N)
for(i in 1:N){
  print(i)
  params.i <- random.params[i,]
  
  rf.model.i <- ranger(Class ~ ., data = credit, num.trees = params.i$num.trees, mtry = params.i$mtry, min.node.size = params.i$min.node.size,
                       sample.fraction = params.i, max.depth = params.i,
                       importance = "impurity")
  f.scores[i] <- MLmetrics::FBeta_Score(credit$Class, rf.model$predictions, beta=2)
}
best.params <- random.params[which(f.scores == max(f.scores)),]
write.csv(best.params, "rf-best_params.csv")

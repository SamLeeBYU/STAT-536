X.scaled <- cbind(1, scale(X))
colnames(X.scaled)[1] <- "Intercept"
svd.X <- svd(X.scaled)
U <- svd.X$u; D <- diag(svd.X$d); V <- svd.X$v

#Fit PCR using a choice of k
k <- 3
Z <- X.scaled%*%V[,1:k]
pcr.model <- glm(Y ~ Z - 1, family="binomial")
gamma.hat <- matrix(coef(pcr.model))
pcr.predicted <- invlogit(Z%*%gamma.hat)

youden.pcr <- sapply(cutoffs, get_youden, Y, pcr.predicted)
pcr.c.star <- cutoffs[which.max(youden.pcr)]
y.hat.pcr <- as.numeric(pcr.predicted > pcr.c.star)

pcr.insample <- c(
  "F1" = F1_Score(Y, y.hat.pcr),
  "AUC" = roc(as.numeric(Y), y.hat.pcr)$auc
)

loadings=V[,1:k]
colnames(loadings) <- paste0("PC", 1:3)
rownames(loadings) <- colnames(X.scaled)
abs.loadings <- abs(loadings)
top.factors <- apply(abs.loadings, 2, function(x) order(x, decreasing=TRUE)[1:5])
top.factors.names <- lapply(1:3, function(i) rownames(loadings)[top.factors[,i]])

run.bootstrap <- function(){
  pcr.bootstrap <- function(X=X.scaled, Y, B=1000, k=3){
    K = nrow(X)
    
    pcr.boot <- matrix(nrow=B, ncol=ncol(X))
    
    for(b in 1:B){
      print(b)
      observations.b <- sample(1:K, replace=T)
      X.b <- X[observations.b,]
      Y.b <- Y[observations.b]
      svd.X.b <- svd(X.b)
      U <- svd.X.b$u; D <- diag(svd.X.b$d); V <- svd.X.b$v
      Z.b <- X.b%*%V[,1:k]
      gamma.boot <- matrix(coef(glm(Y.b ~ Z.b - 1, family="binomial")))
      pcr.boot[b,] <- V[,1:k]%*%gamma.boot
    }
    
    return(pcr.boot %>% apply(2, function(x){
      x.cleaned <- na.omit(x)
      quantile(x.cleaned, c(0.025, 0.5, 0.975))
    }))
  }
  pcr.confint <- pcr.bootstrap(X.scaled, Y, B=100)
  colnames(pcr.confint) <- colnames(X.scaled)
  
  pcr.confint.important <- list()
  for(i in 1:(k+1)){
    if(i == k+1){
      pcr.confint.important[[i]] <- t(pcr.confint[,1])
    } else {
      factors <- top.factors.names[[i]]
      pcr.confint.important[[i]] <- t(pcr.confint[,factors])
    }
  }
}

#Use randomized grid search to find best parameters
pcr.randomized.cv <- function(){
  K <- 5
  folds <- createFolds(Y, k = K, list = TRUE, returnTrain = TRUE)
  n <- nrow(X.scaled)
  
  cutoff <- seq(0, 1, length.out=100)
  max.k <- min(dim(X.scaled))
  
  params <- expand.grid(
    cutoff = cutoff,
    k = 1:20
  )
  
  N = 40
  
  r.params <- params[sample(1:nrow(params), N),]
  
  f1.cv <- numeric(N)
  
  for(i in 1:N){
    print(i)
    f1.fold <- numeric(K)
    for(k in 1:K){
      train.index <- folds[[k]]
      test.index <- setdiff(1:nrow(X.scaled), train.index)
      
      X.train <- X.scaled[train.index,]
      Y.train <- Y[train.index]
      X.test <- X.scaled[test.index,]
      Y.test <- Y[test.index]
      
      Z.train <- X.train %*% V[,1:r.params[i,]$k]
      gamma.hat <- matrix(coef(glm(Y.train ~ Z.train - 1, family="binomial")))
      Z.test <- X.test%*%V[,1:r.params[i,]$k]
      Y.hat <- as.numeric(invlogit(Z.test%*%gamma.hat) > r.params[i,]$cutoff)
      
      f1.fold[k] = F1_Score(Y.test, Y.hat)
    }
    f1.cv[i] <- mean(f1.fold, na.rm=T)
  }
  
  
  best.params = r.params[which(f1.cv == max(f1.cv, na.rm = T)),]
  return(best.params)
}

visualize.loadings <- function(){
  
  custom_colors <- c("1" = "#4E79A7", "2" = "#F28E2B", "3" = "#E15759", "4" = "#76B7B2", "5" = "#59A14F")
  
  # Step 3: Filter loadings to only include top factors for each component
  loadings.data <- as.data.frame(loadings)
  loadings.data$variable <- rownames(loadings)
  loadings.data <- pivot_longer(loadings.data, cols = starts_with("PC"), names_to = "Component", values_to = "Loading")
  
  pc1 <- loadings.data %>%
    filter(Component == "PC1" & variable %in% rownames(loadings)[top.factors[,1]])
  pc1$index <- factor(rank(-pc1$Loading), levels = 1:5)
  
  pc2 <- loadings.data %>%
    filter(Component == "PC2" & variable %in% rownames(loadings)[top.factors[,2]]) 
  pc2$index <- factor(rank(-pc2$Loading), levels = 1:5)
  
  pc3 <- loadings.data %>%
    filter(Component == "PC3" & variable %in% rownames(loadings)[top.factors[,3]]) 
  pc3$index <- factor(rank(-pc3$Loading), levels = 1:5)
  
  
  # Step 4: Plotting each component separately
  p1 <- ggplot(pc1, aes(x = reorder(variable, Loading), y = Loading, fill = index)) +
    geom_bar(stat = "identity") +
    labs(title = "Most Influential Components in PC1", x="", y = "Loading") +
    theme_minimal() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    coord_flip()+
    scale_fill_manual(values = custom_colors)
  
  p2 <- ggplot(pc2, aes(x = reorder(variable, Loading), y = Loading, fill = index)) +
    geom_bar(stat = "identity") +
    labs(title = "Most Influential Components in PC2", x="", y = "Loading") +
    theme_minimal() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    coord_flip()+
    scale_fill_manual(values = custom_colors)

  p3 <- ggplot(pc3, aes(x = reorder(variable, Loading), y = Loading, fill = index)) +
    geom_bar(stat = "identity") +
    labs(title = "Most Influential Components in PC3", x="", y = "Loading") +
    theme_minimal() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    coord_flip()+
    scale_fill_manual(values = custom_colors)
  
  p1+p2+p3
}

visualize.social.media <- function(){
  contact.data <- data.frame(
    contact = X.scaled[,"contactsocialMedia"],
    y = pcr.predicted
  ) %>% mutate(
    contact = ifelse(contact < 0, "Direct Contact", "Social Media")
  )
  
  ggplot(contact.data, aes(x = contact, y = y, fill = contact)) +
    geom_boxplot() +
    labs(title = "Effectiveness of Contact Type on Account Opening", x = "Contact Type", y = "Predicted Probability") +
    theme_minimal()+
    theme(legend.position = "None",
          plot.title = element_text(hjust=0.5))
}

visualize.contacts <- function(){
  ggplot(X.scaled, aes(x = campaign, y = pcr.predicted)) +
    geom_point(stat = "summary", fun = mean) +
    geom_smooth(se=T, color="#4E79A7", method="gam")+
    labs(title = "Effect of Campaign Contacts on Predicted Probability", x = "Campaign Contacts", y = "(Average) Predicted Probability") +
    theme_minimal()+
    theme(
      plot.title = element_text(hjust=0.5)
    )
}

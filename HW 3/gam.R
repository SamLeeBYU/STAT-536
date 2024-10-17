library(mgcv)
library(caret)

covariates = get_covariates()
X <- covariates$X
X.GAM <- X[,-6]
Y <- covariates$Y

randomized_grid_search_gam <- function(X, Y, n_samples = 20, k_folds = 5) {

  param_grid <- expand.grid(
    k = seq(3, 20, by = 1),      
    basis = c("cr", "tp")
  ) 
  
  sampled_params <- list()
  for(j in 1:ncol(X)){
    sampled_params[[j]] = param_grid[sample(1:nrow(param_grid), n_samples),]
  }
  
  results <- list()
  folds <- createFolds(Y, k = k_folds, list = TRUE, returnTrain = TRUE)
  
  for (i in 1:n_samples) {
    
    fold_rmse <- numeric(k_folds)
    
    for (k in 1:k_folds) {

      train_index <- folds[[k]]
      test_index <- setdiff(1:nrow(X), train_index)
      
      X_train <- X[train_index, ]
      Y_train <- Y[train_index]
      X_test <- X[test_index, ]
      Y_test <- Y[test_index]
      
      # Build the formula dynamically
      build_gam_formula <- function(Y_train, X_train, sampled_params, i) {
        # Initialize the formula string with the response variable
        formula_str <- "Y_train ~ "
        
        # Loop through each column of X_train and add smooth terms to the formula string
        smooth_terms <- c()  # To collect smooth terms
        for (j in 1:ncol(X_train)) {
          # Get parameters for this predictor (for the ith sample from sampled_params)
          params <- sampled_params[[j]][i, ]
          
          # Add smooth term for this predictor, e.g., s(X_train[,1], k = 5, bs = "cr")
          smooth_term <- paste0("s(X_train[,", j, "], k = ", params$k, ", bs = '", params$basis, "')")
          
          # Append the smooth term to the list of smooth terms
          smooth_terms <- c(smooth_terms, smooth_term)
        }
        
        # Combine all smooth terms and add them to the formula string
        formula_str <- paste0(formula_str, paste(smooth_terms, collapse = " + "))
        
        # Convert the string to a formula object
        gam_formula <- as.formula(formula_str)
        
        return(gam_formula)
      }
      build_X_test_basis <- function(gam_model, X_test) {
        # Get the smooth objects from the model (this tells us the smooth terms structure)
        smooth_terms <- gam_model$smooth
        
        # Initialize an empty matrix to hold all the basis functions for X_test
        basis_list <- list()  # List to store each smooth basis for a term
        
        # Loop over each smooth term and compute the basis matrix for X_test
        for (i in 1:length(smooth_terms)) {
          smooth_term <- smooth_terms[[i]]  # Current smooth term structure
          
          # Get the index of the variable for this smooth term
          var_index <- smooth_term$term
          
          # Extract the correct column from X_test for this smooth term
          X_var <- as.matrix(X_test[, i])
          
          data_for_smooth <- data.frame(X_var)
          colnames(data_for_smooth) <- var_index  # Set the correct name
          
          # Create the basis functions for this smooth term using mgcv::smoothCon
          # 'tp' stands for thin plate spline, and 'cr' stands for cubic regression spline
          basis_matrix <- PredictMat(smooth_term, data_for_smooth)
          basis_list[[i]] <- basis_matrix
        }
        
        # Combine all the basis matrices column-wise (stack them horizontally)
        X_test_basis <- do.call(cbind, basis_list)
        
        return(X_test_basis)
      }
      
      gam_model <- gam(
        formula = build_gam_formula(Y_train, X_train, sampled_params, i),
        data = data.frame(X_train),
        family = gaussian(),   # Adjust this depending on the problem
        select = TRUE,         # Automatically selects the smoothness parameter
        method = "REML"       # Use REML for smoothness selection
      )
      
      X_test.basis = cbind(1, build_X_test_basis(gam_model, X_test))
      predictions = X_test.basis %*% coef(gam_model)
      
      fold_rmse[k] <- sqrt(mean((Y_test - predictions)^2))
    }
    
    avg_rmse <- mean(fold_rmse)
    
    results[[i]] <- list(
      param.index = data.frame(k = i, basis = i, RMSE = avg_rmse)
    )

    print(i)
  }
  
  rmse_values = sapply(results, function(res) res$param.index$RMSE)
  
  return(list(best_params = results[[which.min(rmse_values)]],
              sampled_params = sampled_params))
}

build_X_test_basis <- function(gam_model, X_test) {
  # Get the smooth objects from the model (this tells us the smooth terms structure)
  smooth_terms <- gam_model$smooth
  
  # Initialize an empty matrix to hold all the basis functions for X_test
  basis_list <- list()  # List to store each smooth basis for a term
  
  # Loop over each smooth term and compute the basis matrix for X_test
  for (i in 1:length(smooth_terms)) {
    smooth_term <- smooth_terms[[i]]  # Current smooth term structure
    
    # Get the index of the variable for this smooth term
    var_index <- smooth_term$term
    
    # Extract the correct column from X_test for this smooth term
    X_var <- as.matrix(X_test[, i])
    
    data_for_smooth <- data.frame(X_var)
    colnames(data_for_smooth) <- var_index  # Set the correct name
    
    # Create the basis functions for this smooth term using mgcv::smoothCon
    # 'tp' stands for thin plate spline, and 'cr' stands for cubic regression spline
    basis_matrix <- PredictMat(smooth_term, data_for_smooth)
    basis_list[[i]] <- basis_matrix
  }
  
  # Combine all the basis matrices column-wise (stack them horizontally)
  X_test_basis <- do.call(cbind, basis_list)
  
  return(X_test_basis)
}
build.gam.model <- function(best_params, attachment="_train"){
  k.star <- best_params$best_params$param.index$k
  
  formula_str <- str_c("Y", attachment, " ~ ")
  smooth_terms <- c()
  for(j in 1:ncol(X.GAM)){
    params = gam.params$sampled_params[[j]][k.star,]
    smooth_term <- paste0("s(X", attachment, "[,", j, "], k = ", params$k, ", bs = '", params$basis, "')")
    smooth_terms <- c(smooth_terms, smooth_term)
  }
  formula_str <- paste0(formula_str, paste(smooth_terms, collapse = " + "))
  gam_formula <- as.formula(formula_str)
  return(gam_formula)
}

gam.params <- randomized_grid_search_gam(X.GAM, Y)

gam.optimal <- build.gam.model(gam.params, attachment = ".GAM")
gam.optimal.train <- build.gam.model(gam.params, attachment = "_train.GAM")

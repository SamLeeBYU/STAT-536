library(kableExtra)

clean_covariate_names <- function(covariates) {
  # Replace "poly(variable, 2)1" with "variable"
  cleaned <- gsub("poly\\(([^,]+), 2\\)1", "\\1", covariates)
  
  # Replace "poly(variable, 2)2" with "variable (squared)"
  cleaned <- gsub("poly\\(([^,]+), 2\\)2", "\\1 (squared)", cleaned)
  
  # Replace interaction terms "poly(variable1, 2)x:poly(variable2, 2)y" 
  # with "variable1 * variable2 (squared)"
  cleaned <- gsub("poly\\(([^,]+), 2\\)1:poly\\(([^,]+), 2\\)1", "\\1 * \\2", cleaned)
  cleaned <- gsub("poly\\(([^,]+), 2\\)1:poly\\(([^,]+), 2\\)2", "\\1 * \\2 (squared)", cleaned)
  cleaned <- gsub("poly\\(([^,]+), 2\\)2:poly\\(([^,]+), 2\\)1", "\\1 (squared) * \\2", cleaned)
  cleaned <- gsub("poly\\(([^,]+), 2\\)2:poly\\(([^,]+), 2\\)2", "\\1 (squared) * \\2 (squared)", cleaned)
  
  # Handle (Intercept) separately if necessary
  cleaned <- gsub("\\(Intercept\\)", "Intercept", cleaned)
  
  return(cleaned)
}

metadata <- data.frame(
  Variable = c("LON", "LAT", "Slope", "Aspect", "ELEV"),
  Description = c(
    "Longitude coordinate of the plot",
    "Latitude coordinate of the plot",
    "Average slope of the plot in degrees (0=flat, 90=vertical)",
    "Counterclockwise degrees from north facing (90=west, 180=south, 270=east)",
    "Elevation of plot centroid in feet"
  ),
  stringsAsFactors = FALSE
)

describe_covariates <- function(clean_covariates, metadata=metadata) {
  # Initialize a vector to store the descriptions
  descriptions <- vector("character", length(clean_covariates))
  
  # Iterate over each covariate and generate description
  for (i in seq_along(clean_covariates)) {
    covariate <- clean_covariates[i]
    
    # Skip intercept
    if (covariate == "Intercept") {
      descriptions[i] <- ""
      next
    }
    
    # Check for interaction terms
    if (grepl("\\:", covariate)) {
      # Split the covariate names to get the individual variables
      interaction_vars <- strsplit(covariate, "\\:")[[1]]
      # Simply list the variables involved in the interaction
      descriptions[i] <- paste("Interaction effect between", paste(interaction_vars, collapse = " and "))
    } else {
      # Identify if it is a squared term using "(squared)" notation
      if (grepl("\\(squared\\)", covariate)) {
        base_var <- gsub(" \\(squared\\)", "", covariate)
        var_desc <- metadata$Description[metadata$Variable == base_var]
        descriptions[i] <- paste(var_desc, "(squared)")
      } else {
        # Regular single variable
        var_desc <- metadata$Description[metadata$Variable == covariate]
        descriptions[i] <- var_desc
      }
    }
  }
  
  # Remove any empty descriptions (from intercept)
  descriptions <- descriptions[descriptions != ""]
  
  # Return the descriptions
  return(descriptions)
}

to_latex <- function(beta_ci_centered, lasso_beta, beta_importance, covariate_names, get.significant=F) {
  covariate_names <- clean_covariate_names(covariate_names)
  
  # Combine the lower and upper CI into one column
  ci_combined <- paste0("(", round(beta_ci_centered[, 1], 3), ", ", round(beta_ci_centered[, 2], 3), ")")
  
  # Mark estimates with an asterisk if the confidence intervals do not include zero
  estimates <- sapply(1:length(lasso_beta), function(i) {
    lower_bound <- beta_ci_centered[i, 1]
    upper_bound <- beta_ci_centered[i, 2]
    est <- round(lasso_beta[i], 3)
    if (lower_bound > 0 | upper_bound < 0) {
      return(paste0(est, "*"))  # Add asterisk for significant estimates
    } else {
      return(as.character(est))  # No asterisk if intervals include zero
    }
  })
  
  # Match covariate names with metadata descriptions and units
  descriptions <- c("", describe_covariates(covariate_names, metadata))
  
  # Create a data frame with covariate names, descriptions, estimates, combined CI, and inclusion frequency
  results_df <- data.frame(
    "Covariate" = covariate_names,
    "Description" = descriptions,
    "Estimate" = estimates,  # Use the updated estimates with asterisks
    "95% CI" = ci_combined,
    "Inclusion Frequency" = round(beta_importance, 3)
  )
  
  # # Exclude intercept from sorting but keep it in the table
  # intercept_index <- which(results_df$Covariate == "(Intercept)")
  # non_intercept_df <- results_df[-intercept_index, ]
  # 
  # # Sort the non-intercept rows alphabetically by Covariate
  # sorted_df <- non_intercept_df[order(non_intercept_df$Covariate), ]
  # 
  # # Add the intercept back at the top
  # sorted_df <- rbind(results_df[intercept_index, ], sorted_df)
  
  lab <- "lasso-coefficients"
  if(get.significant){
    significant_rows <- grepl("\\*", results_df$Estimate)
    results_df <- results_df[significant_rows, ]
    lab <- str_c(lab, "-significant")
  }
  
  # Generate LaTeX table using knitr's kable with a fixed width for the description column and horizontal lines
  latex_table <- kable(results_df, format = "latex", booktabs = TRUE, 
                       col.names = c("Covariate", "Description", "Estimate", "95% CI", "Inclusion Frequency"),
                       row.names = FALSE,  # Removes row names
                       caption = str_c("\\label{tab:", lab, "}Lasso Coefficient Estimates")) %>%
    kable_styling(latex_options = "hold_position", font_size = 7.5) %>%  # Set smaller font size
    column_spec(2, width = "5cm", latex_valign = "m")  # Set the width of the 'Description' column
  
  return(latex_table)
}

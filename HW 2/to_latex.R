library(kableExtra)

to_latex <- function(beta_ci_centered, lasso_beta, beta_importance, covariate_names, metadata) {
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
  descriptions <- sapply(covariate_names, function(covariate) {
    if (covariate == "(Intercept)") {
      return("")  # Blank description for the intercept
    } else if (covariate == "Lon") {
      return("Longitude")  # Special case for Longitude
    } else {
      matched_row <- metadata[metadata$orig.vname == covariate, ]
      if (nrow(matched_row) > 0) {
        descr <- matched_row$descr
        unit <- matched_row$units
        if (!is.na(unit) && unit != "N/A") {
          return(paste0(descr, " (", unit, ")"))  # Include both description and units
        } else {
          return(descr)  # Include only the description
        }
      } else {
        return(NA)  # Handle cases where description is missing
      }
    }
  })
  
  # Create a data frame with covariate names, descriptions, estimates, combined CI, and inclusion frequency
  results_df <- data.frame(
    "Covariate" = covariate_names,
    "Description" = descriptions,
    "Estimate" = estimates,  # Use the updated estimates with asterisks
    "95% CI" = ci_combined,
    "Inclusion Frequency" = round(beta_importance, 3)
  )
  
  # Exclude intercept from sorting but keep it in the table
  intercept_index <- which(results_df$Covariate == "(Intercept)")
  non_intercept_df <- results_df[-intercept_index, ]
  
  # Sort the non-intercept rows alphabetically by Covariate
  sorted_df <- non_intercept_df[order(non_intercept_df$Covariate), ]
  
  # Add the intercept back at the top
  sorted_df <- rbind(results_df[intercept_index, ], sorted_df)
  
  # Generate LaTeX table using knitr's kable with a fixed width for the description column and horizontal lines
  latex_table <- kable(sorted_df, format = "latex", booktabs = TRUE, 
                       col.names = c("Covariate", "Description", "Estimate", "95% CI", "Inclusion Frequency"),
                       row.names = FALSE,  # Removes row names
                       caption = "Lasso Coefficient Estimates with Bootstrap Confidence Intervals, Descriptions, and Units") %>%
    kable_styling(latex_options = "hold_position", font_size = 7.5) %>%  # Set smaller font size
    column_spec(2, width = "5cm", latex_valign = "m")  # Set the width of the 'Description' column
  
  return(latex_table)
}
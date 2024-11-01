# Demo Script for Mack's Estimator Package
# Save this as inst/doc/demo-MackEstimatorCP.R

# Load required libraries
library(MackEstimatorCP) # Ensure this is the package name where your function resides

# Function definition for the demo
run_demo <- function() {
  # -----------------------
  # DEMO: Mack's Estimator
  # -----------------------
  
  # Step 1: Set up example cumulative claims data for 10 years
  claims_triangle <- matrix(c(
    1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500,  # Accident Year 1
    1200, 1800, 2400, 2900, 3500, 4100, 4700, 5300, 5900, NA,    # Accident Year 2
    1400, 2100, 2800, 3500, 4200, 4900, 5600, NA, NA, NA,       # Accident Year 3
    1600, 2400, 3200, 4000, 4800, NA, NA, NA, NA, NA,           # Accident Year 4
    1800, 2700, 3600, NA, NA, NA, NA, NA, NA, NA,               # Accident Year 5
    2000, 3000, NA, NA, NA, NA, NA, NA, NA, NA,                 # Accident Year 6
    2200, NA, NA, NA, NA, NA, NA, NA, NA, NA,                   # Accident Year 7
    2400, NA, NA, NA, NA, NA, NA, NA, NA, NA,                   # Accident Year 8
    2600, NA, NA, NA, NA, NA, NA, NA, NA, NA,                   # Accident Year 9
    2800, NA, NA, NA, NA, NA, NA, NA, NA, NA                    # Accident Year 10
  ), nrow = 10, byrow = TRUE)
  
  # Step 2: Set severity parameters (optional)
  severity_mean <- 2000  # Example mean severity of claims
  severity_sd <- 500     # Example standard deviation of claim severity
  
  # Step 3: Call the mack_estimator function from your package
  result <- mack_estimator(claims_triangle, severity_mean, severity_sd)
  
  # Step 4: Display the results
  cat("Development Factors:\n")
  print(result$development_factors)
  
  cat("\nVariance Estimates:\n")
  print(result$variance_estimates)
  
  cat("\nMSE Prediction:\n")
  print(result$mse_prediction)
  
  cat("\nStandardized Conditional MSE (L_alpha):\n")
  print(result$L_alpha)
  
  cat("\nAsymptotic Limits:\n")
  print(result$asymptotic_limits)
  
  cat("\nConditional MSE Estimate Result:\n")
  print(result$conditional_mse_estimate_result)
  
  # Step 5: Interpretation of results
  cat("\nInterpretation of Results:\n")
  cat("- Development Factors indicate the growth in claims over development years.\n")
  cat("- Variance Estimates provide insights into the uncertainty of the development factors.\n")
  cat("- MSE Predictions help assess the accuracy of the predictions.\n")
  cat("- Standardized Conditional MSE (L_alpha) gives a relative measure of MSE.\n")
  cat("- Asymptotic Limits indicate expected performance under certain conditions.\n")
  cat("- Conditional MSE Estimate Result provides a refined measure of prediction error.\n")
}

# Run the demo when the script is sourced
run_demo()

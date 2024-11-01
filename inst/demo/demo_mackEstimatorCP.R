# Load required libraries
library(stats)

# Define the run_demo function to execute all functions with sample data
run_demo <- function() {
  
  # 1. Generate a large cumulative claims vector (simulated)
  set.seed(1)
  cumulative_claims <- cumsum(runif(20, min = 50, max = 150))  # 20 periods of cumulative claims
  
  # 2. Generate larger claim severities and number of claims for the compound Poisson process
  claim_severities <- runif(100, min = 20, max = 80)  # 100 random claim severities
  num_claims <- 10  # Assume we have 10 claims
  
  # 3. Chain Ladder parameters
  f_t <- 1.3
  C_it <- 500
  sigma_t_squared <- 0.05
  
  # 4. Generate a larger claims triangle with NA values to mimic incomplete triangle data
  set.seed(2)
  claims_triangle <- matrix(NA, nrow = 10, ncol = 10)
  for (i in 1:10) {
    claims_triangle[i, 1:(11 - i)] <- cumsum(runif(11 - i, min = 100, max = 500))
  }
  
  # Test each function
  
  cat("Running demo for all actuarial functions...\n\n")
  
  # 1. Incremental Claims Representation
  cat("1. Incremental Claims Representation:\n")
  incremental_result <- incremental_claims(cumulative_claims)
  print(incremental_result)
  
  # 2. Compound Poisson Process Representation
  cat("\n2. Compound Poisson Process Representation:\n")
  compound_poisson_result <- compound_poisson_claims(num_claims, claim_severities)
  print(compound_poisson_result)
  
  # 3. Chain Ladder Assumptions
  cat("\n3. Chain Ladder Expectation and Variance:\n")
  chain_ladder_expectation_result <- chain_ladder_expectation(f_t, C_it)
  print(chain_ladder_expectation_result)
  
  chain_ladder_variance_result <- chain_ladder_variance(sigma_t_squared, C_it)
  print(chain_ladder_variance_result)
  
  # 4. Estimator of Development Factor
  cat("\n4. Development Factor Estimate (t=2):\n")
  development_factor_result <- development_factor_estimate(claims_triangle, 2)
  print(development_factor_result)
  
  # 5. Mack’s Conditional Mean Squared Error (MSE) of Prediction
  cat("\n5. Mack’s Conditional MSE of Prediction:\n")
  conditional_mse_result <- conditional_mse_prediction(sigma_t_squared, f_t, C_it, claims_triangle, 2)
  print(conditional_mse_result)
  
  # 6. Asymptotic Development Factor Limit
  cat("\n6. Asymptotic Development Factor Limit:\n")
  expected_Z_t_plus1 <- 120
  expected_Z_t <- 100
  asymptotic_dev_factor_result <- asymptotic_development_factor_limit(expected_Z_t_plus1, expected_Z_t)
  print(asymptotic_dev_factor_result)
  
  # 7. Variance Parameter Estimator
  cat("\n7. Variance Parameter Estimation:\n")
  expected_Z_squared_t_plus1 <- 14400
  expected_Z_t_plus1 <- 120
  expected_Z_squared_t <- 10000
  expected_Z_t <- 100
  variance_parameter_result <- variance_parameter_estimate(f_t, expected_Z_squared_t_plus1, expected_Z_t_plus1, expected_Z_squared_t, expected_Z_t)
  print(variance_parameter_result)
  
  # 8. Conditional MSE of Prediction in Asymptotic Case
  cat("\n8. Asymptotic Conditional MSE:\n")
  C_i_T <- 1000
  C_i_T_minus_i_plus1 <- 900
  f_s <- c(1.15, 1.2, 1.25)
  asymptotic_mse_result <- asymptotic_conditional_mse(C_i_T, C_i_T_minus_i_plus1, f_s)
  print(asymptotic_mse_result)
  
  # 9. Standardized Conditional MSE Prediction with Exposure Parameter (L_alpha)
  cat("\n9. Standardized Conditional MSE:\n")
  T <- 10
  i <- 3
  standardized_conditional_mse_result <- standardized_conditional_mse(C_i_T, C_i_T_minus_i_plus1, sigma_t_squared, f_t, claims_triangle, T, i)
  print(standardized_conditional_mse_result)
  
  # 10. Asymptotic Variance Estimation for Parameter
  cat("\n10. Asymptotic Variance Estimation:\n")
  asymptotic_variance_result <- asymptotic_variance_estimation(claims_triangle, f_t, 2)
  print(asymptotic_variance_result)
  
  # 11. Estimator for Conditional MSE (Equation 5.1)
  cat("\n11. Estimator for Conditional MSE (Equation 5.1):\n")
  conditional_mse_estimate_result <- conditional_mse_estimate(C_i_T, sigma_t_squared, f_t, claims_triangle, T, i)
  print(conditional_mse_estimate_result)
  
  cat("\nDemo completed.")
}

# Run the demo function
run_demo()

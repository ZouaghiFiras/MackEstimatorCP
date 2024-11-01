# Load required libraries
library(stats)

# 1. Incremental Claims Representation
incremental_claims <- function(cumulative_claims) {
  diff(cumulative_claims, lag = 1, differences = 1)
}

# 2. Compound Poisson Process Representation
compound_poisson_claims <- function(num_claims, claim_severities) {
  sum(claim_severities[1:num_claims])
}

# 3. Chain Ladder Assumptions
chain_ladder_expectation <- function(f_t, C_it) {
  f_t * C_it
}

chain_ladder_variance <- function(sigma_t_squared, C_it) {
  sigma_t_squared * C_it
}

# 4. Estimator of Development Factor
development_factor_estimate <- function(claims_triangle, t) {
  numerator <- sum(claims_triangle[1:(nrow(claims_triangle) - t), t + 1], na.rm = TRUE)
  denominator <- sum(claims_triangle[1:(nrow(claims_triangle) - t), t], na.rm = TRUE)
  if (denominator == 0) return(NA)
  numerator / denominator
}

# 5. Mack’s Conditional Mean Squared Error (MSE) of Prediction
conditional_mse_prediction <- function(sigma_t_squared, f_t, C_it, claims_triangle, t) {
  if (C_it == 0 || f_t == 0) return(NA)
  term1 <- sigma_t_squared / (f_t^2)
  term2 <- 1 / C_it
  term3 <- 1 / sum(claims_triangle[, t], na.rm = TRUE)
  term1 * (term2 + term3)
}

# 6. Asymptotic Development Factor Limit
asymptotic_development_factor_limit <- function(expected_Z_t_plus1, expected_Z_t) {
  if (expected_Z_t == 0) return(NA)
  expected_Z_t_plus1 / expected_Z_t
}

# 7. Variance Parameter Estimator
variance_parameter_estimate <- function(f_t, expected_Z_squared_t_plus1, expected_Z_t_plus1, expected_Z_squared_t, expected_Z_t) {
  if (expected_Z_t_plus1 == 0 || expected_Z_t == 0) return(NA)
  (f_t - 1) * (expected_Z_squared_t_plus1 / expected_Z_t_plus1 + (f_t - 1) * expected_Z_squared_t / expected_Z_t)
}

# 8. Extend Triangle to Full Rectangle (Ultimates Calculation)
complete_rectangle <- function(claims_triangle, development_factors) {
  n_accident_years <- nrow(claims_triangle)
  n_dev_years <- ncol(claims_triangle)
  
  for (i in 1:n_accident_years) {
    for (t in (n_dev_years - i + 1):n_dev_years) {
      claims_triangle[i, t] <- claims_triangle[i, t - 1] * development_factors[t - 1]
    }
  }
  claims_triangle
}

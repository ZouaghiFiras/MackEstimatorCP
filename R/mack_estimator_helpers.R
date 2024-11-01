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
  
  if (denominator == 0) {
    return(NA)
  }
  
  numerator / denominator
}

# 5. Mack’s Conditional Mean Squared Error (MSE) of Prediction
conditional_mse_prediction <- function(sigma_t_squared, f_t, C_it, claims_triangle, t) {
  if (C_it == 0 || f_t == 0) {
    return(NA) # Handle division by zero
  }
  
  term1 <- sigma_t_squared / (f_t^2)
  term2 <- 1 / C_it
  term3 <- 1 / sum(claims_triangle[, t], na.rm = TRUE)
  
  term1 * (term2 + term3)
}

# 6. Asymptotic Development Factor Limit
asymptotic_development_factor_limit <- function(expected_Z_t_plus1, expected_Z_t) {
  if (expected_Z_t == 0) {
    return(NA)
  }
  
  expected_Z_t_plus1 / expected_Z_t
}

# 7. Variance Parameter Estimator
variance_parameter_estimate <- function(f_t, expected_Z_squared_t_plus1, expected_Z_t_plus1, expected_Z_squared_t, expected_Z_t) {
  if (expected_Z_t_plus1 == 0 || expected_Z_t == 0) {
    return(NA)
  }
  
  (f_t - 1) * (expected_Z_squared_t_plus1 / expected_Z_t_plus1 +
                 (f_t - 1) * expected_Z_squared_t / expected_Z_t)
}

# 8. Conditional Mean Squared Error of Prediction in Asymptotic Case
asymptotic_conditional_mse <- function(C_i_T, C_i_T_minus_i_plus1, f_s) {
  if (C_i_T_minus_i_plus1 == 0) {
    return(NA)
  }
  
  cumulative_factor <- prod(f_s)
  (C_i_T - C_i_T_minus_i_plus1 * cumulative_factor)^2 / C_i_T_minus_i_plus1
}

# 9. Standardized Conditional MSE Prediction with Exposure Parameter (L_alpha)
standardized_conditional_mse <- function(C_i_T, C_i_T_minus_i_plus1, sigma_t_squared, f_t, claims_triangle, T, i) {
  if (C_i_T_minus_i_plus1 == 0) {
    return(NA) # Handle division by zero case
  }
  
  L_alpha <- (C_i_T^2 / C_i_T_minus_i_plus1) *
    sum(sapply((T - i + 1):(T - 1), function(t) {
      if (t < 1 || t > ncol(claims_triangle) || i > nrow(claims_triangle)) return(0)
      if (claims_triangle[i, t] == 0) return(0) # Avoid division by zero
      
      sigma_t_squared[t] / f_t[t]^2 * (1 / claims_triangle[i, t] + 1 / sum(claims_triangle[, t], na.rm = TRUE))
    }))
  
  L_alpha
}

# 10. Asymptotic Variance Estimation for Parameter
asymptotic_variance_estimation <- function(claims_triangle, f_t, t) {
  valid_rows <- 1:(nrow(claims_triangle) - t)
  
  residuals <- (claims_triangle[valid_rows, t + 1] / claims_triangle[valid_rows, t] - f_t)^2
  weighted_sum <- sum(claims_triangle[valid_rows, t] * residuals, na.rm = TRUE)
  
  if ((nrow(claims_triangle) - t - 1) == 0) {
    return(NA)
  }
  
  weighted_sum / (nrow(claims_triangle) - t - 1)
}

# 11. Estimator for Conditional Mean Squared Error (Equation 5.1)
conditional_mse_estimate <- function(C_i_T, sigma_t_squared, f_t, claims_triangle, T, i) {
  if (C_i_T == 0) {
    return(NA) # Avoid division by zero
  }
  
  L_alpha <- (C_i_T^2) *
    sum(sapply((T - i + 1):(T - 1), function(t) {
      if (t < 1 || t > ncol(claims_triangle) || i > nrow(claims_triangle)) return(0)
      if (claims_triangle[i, t] == 0) return(0) # Avoid division by zero
      
      sigma_t_squared[t] / f_t[t]^2 * (1 / claims_triangle[i, t] + 1 / sum(claims_triangle[, t], na.rm = TRUE))
    }))
  
  L_alpha
}

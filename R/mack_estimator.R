
#' Main function to compute Mack's estimator using helper functions
#'
#' @param claims_triangle A matrix representing cumulative claims data.
#' @param severity_mean Mean severity of claims, if available.
#' @param severity_sd Standard deviation of claim severity, if available.
#' @return A list containing development factors, variance estimates, and conditional MSE predictions.
#' @export
mack_estimator <- function(claims_triangle, severity_mean = NULL, severity_sd = NULL) {
  
  # Validate input
  if (!is.matrix(claims_triangle)) {
    stop("claims_triangle must be a matrix.")
  }
  if (nrow(claims_triangle) < 2 || ncol(claims_triangle) < 2) {
    stop("claims_triangle must have at least 2 rows and 2 columns.")
  }
  
  # Initialize parameters
  n_accident_years <- nrow(claims_triangle)
  n_dev_years <- ncol(claims_triangle)
  
  # Step 1: Calculate Development Factors (f_t) for each development year
  development_factors <- sapply(1:(n_dev_years - 1), function(t) {
    development_factor_estimate(claims_triangle, t)
  })
  
  # Step 2: Calculate Variance Estimates (sigma_t^2) for each development year
  variance_estimates <- sapply(1:(n_dev_years - 1), function(t) {
    asymptotic_variance_estimation(claims_triangle, development_factors[t], t)
  })
  
  # Step 3: Calculate Conditional MSE of Prediction for each cell in the claims triangle
  mse_prediction <- matrix(NA, nrow = n_accident_years, ncol = n_dev_years)
  for (i in 1:n_accident_years) {
    for (t in (i + 1):n_dev_years) {
      if (!is.na(claims_triangle[i, t - 1]) && 
          !is.na(variance_estimates[t - 1]) && 
          !is.na(development_factors[t - 1])) {
        mse_prediction[i, t] <- conditional_mse_prediction(
          sigma_t_squared = variance_estimates[t - 1],
          f_t = development_factors[t - 1],
          C_it = claims_triangle[i, t - 1],
          claims_triangle = claims_triangle,
          t = t - 1
        )
      }
    }
  }
  
  # Step 4: Calculate Standardized Conditional MSE with Exposure Parameter (L_alpha)
  L_alpha <- matrix(NA, nrow = n_accident_years, ncol = n_dev_years)
  for (i in 1:n_accident_years) {
    for (t in (i + 1):n_dev_years) {
      if (!is.na(claims_triangle[i, t - 1]) && !is.na(claims_triangle[i, i]) && 
          !is.na(variance_estimates[t - 1]) && !is.na(development_factors[t - 1])) {
        L_alpha[i, t] <- standardized_conditional_mse(
          C_i_T = claims_triangle[i, t - 1],
          C_i_T_minus_i_plus1 = claims_triangle[i, i],
          sigma_t_squared = variance_estimates[t - 1],
          f_t = development_factors[t - 1],
          claims_triangle = claims_triangle,
          T = n_dev_years,
          i = i
        )
      }
    }
  }
  
  # Step 5: Compute Asymptotic Development Factor Limits (if severity data is provided)
  asymptotic_limits <- NULL
  if (!is.null(severity_mean) && !is.null(severity_sd)) {
    asymptotic_limits <- sapply(1:(n_dev_years - 1), function(t) {
      asymptotic_development_factor_limit(severity_mean, severity_sd)
    })
  }
  
  # Step 6: Conditional MSE Estimator (Equation 5.1)
  conditional_mse_estimate_result <- matrix(NA, nrow = n_accident_years, ncol = n_dev_years)
  for (i in 1:n_accident_years) {
    for (t in (i + 1):n_dev_years) {
      if (!is.na(claims_triangle[i, t - 1])) {
        conditional_mse_estimate_result[i, t] <- conditional_mse_estimate(
          C_i_T = claims_triangle[i, t - 1],
          sigma_t_squared = variance_estimates[t - 1],
          f_t = development_factors[t - 1],
          claims_triangle = claims_triangle,
          T = n_dev_years,
          i = i
        )
      }
    }
  }
  
  # Return all results as a list
  return(list(
    development_factors = development_factors,
    variance_estimates = variance_estimates,
    mse_prediction = mse_prediction,
    L_alpha = L_alpha,
    asymptotic_limits = asymptotic_limits,
    conditional_mse_estimate_result = conditional_mse_estimate_result
  ))
}
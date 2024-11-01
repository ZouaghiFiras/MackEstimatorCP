# 9. Main function to compute Mack's estimator and return all results
mack_estimator <- function(claims_triangle, severity_mean = NULL, severity_sd = NULL) {
  
  if (!is.matrix(claims_triangle)) stop("claims_triangle must be a matrix.")
  if (nrow(claims_triangle) < 2 || ncol(claims_triangle) < 2) stop("claims_triangle must have at least 2 rows and 2 columns.")
  
  n_accident_years <- nrow(claims_triangle)
  n_dev_years <- ncol(claims_triangle)
  
  # Step 1: Calculate Development Factors (f_t) for each development year
  development_factors <- sapply(1:(n_dev_years - 1), function(t) {
    development_factor_estimate(claims_triangle, t)
  })
  
  # Step 2: Calculate Variance Estimates (sigma_t^2)
  variance_estimates <- sapply(1:(n_dev_years - 1), function(t) {
    asymptotic_variance_estimation(claims_triangle, development_factors[t], t)
  })
  
  # Step 3: Complete claims_triangle to full rectangle
  completed_rectangle <- complete_rectangle(claims_triangle, development_factors)
  
  # Step 4: Conditional MSE of Prediction
  mse_prediction <- matrix(NA, nrow = n_accident_years, ncol = n_dev_years)
  for (i in 1:n_accident_years) {
    for (t in (i + 1):n_dev_years) {
      if (!is.na(claims_triangle[i, t - 1]) && !is.na(variance_estimates[t - 1]) && !is.na(development_factors[t - 1])) {
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
  
  # Step 5: Asymptotic Development Factor Limits (if severity data is provided)
  asymptotic_limits <- NULL
  if (!is.null(severity_mean) && !is.null(severity_sd)) {
    asymptotic_limits <- sapply(1:(n_dev_years - 1), function(t) {
      asymptotic_development_factor_limit(severity_mean, severity_sd)
    })
  }
  
  # Return list of results
  return(list(
    development_factors = development_factors,
    variance_estimates = variance_estimates,
    mse_prediction = mse_prediction,
    completed_rectangle = completed_rectangle,
    asymptotic_limits = asymptotic_limits
  ))
}

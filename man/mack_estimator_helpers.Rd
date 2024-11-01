\name{mackEstimator}
\alias{mackEstimator}
\title{Mack Estimator Functions for Actuarial Claims Reserving}
\description{
A set of functions for implementing Mack's estimator in the chain ladder method for actuarial claims reserving, including incremental claims calculation, development factors, variance estimates, and the extension of the claims triangle to a full rectangle.
}
\usage{
incremental_claims(cumulative_claims)
compound_poisson_claims(num_claims, claim_severities)
chain_ladder_expectation(f_t, C_it)
chain_ladder_variance(sigma_t_squared, C_it)
development_factor_estimate(claims_triangle, t)
conditional_mse_prediction(sigma_t_squared, f_t, C_it, claims_triangle, t)
asymptotic_development_factor_limit(expected_Z_t_plus1, expected_Z_t)
variance_parameter_estimate(f_t, expected_Z_squared_t_plus1, expected_Z_t_plus1, expected_Z_squared_t, expected_Z_t)
complete_rectangle(claims_triangle, development_factors)
}
\arguments{
  \item{cumulative_claims}{A numeric vector of cumulative claims data for each period.}
  \item{num_claims}{Integer; number of claims for a given period.}
  \item{claim_severities}{A numeric vector of individual claim severities.}
  \item{f_t}{The development factor for a given development year.}
  \item{C_it}{The cumulative claims amount for an accident year and development year.}
  \item{sigma_t_squared}{The variance of the development factor for a given development year.}
  \item{claims_triangle}{A matrix of cumulative claims data organized as a run-off triangle.}
  \item{t}{An integer representing the development year index in the triangle.}
  \item{expected_Z_t_plus1}{The expected value of the claims amount for development year t+1.}
  \item{expected_Z_t}{The expected value of the claims amount for development year t.}
  \item{expected_Z_squared_t_plus1}{The expected value of the squared claims amount for development year t+1.}
  \item{expected_Z_squared_t}{The expected value of the squared claims amount for development year t.}
  \item{development_factors}{A numeric vector of calculated development factors for each development year.}
}
\details{
These functions provide an implementation of Mack's chain ladder model for claims reserving, allowing for the calculation of development factors, variance estimates, and extensions from a claims triangle to a full rectangle. The primary functions are:
\itemize{
  \item \code{incremental_claims}: Calculates incremental claims from cumulative claims data.
  \item \code{compound_poisson_claims}: Estimates total claims amount based on a compound Poisson process.
  \item \code{chain_ladder_expectation}: Calculates expected claims for a given development factor and cumulative claims.
  \item \code{chain_ladder_variance}: Calculates variance of claims using the chain ladder model.
  \item \code{development_factor_estimate}: Estimates the development factor for a specified development year.
  \item \code{conditional_mse_prediction}: Computes Mack's conditional mean squared error of prediction.
  \item \code{asymptotic_development_factor_limit}: Calculates the asymptotic development factor limit.
  \item \code{variance_parameter_estimate}: Estimates the variance parameter.
  \item \code{complete_rectangle}: Extends the claims triangle to a full rectangle by estimating ultimate claims.
}
}
\value{
  Each function returns results specific to the function:
  \itemize{
    \item \code{incremental_claims}: A numeric vector of incremental claims.
    \item \code{compound_poisson_claims}: A numeric value representing total claims amount.
    \item \code{chain_ladder_expectation}: Expected cumulative claims.
    \item \code{chain_ladder_variance}: Variance of cumulative claims.
    \item \code{development_factor_estimate}: The estimated development factor.
    \item \code{conditional_mse_prediction}: The conditional mean squared error of prediction.
    \item \code{asymptotic_development_factor_limit}: The asymptotic development factor limit.
    \item \code{variance_parameter_estimate}: The estimated variance parameter.
    \item \code{complete_rectangle}: A matrix representing the completed claims rectangle.
  }
}
\examples{
# Example data setup
cumulative_claims <- cumsum(runif(20, min = 50, max = 150))
claim_severities <- runif(100, min = 20, max = 80)
num_claims <- 10
claims_triangle <- matrix(NA, nrow = 10, ncol = 10)
for (i in 1:10) {
  claims_triangle[i, 1:(11 - i)] <- cumsum(runif(11 - i, min = 100, max = 500))
}

# Incremental Claims
incremental_claims(cumulative_claims)

# Compound Poisson Process Claims
compound_poisson_claims(num_claims, claim_severities)

# Chain Ladder Expectation and Variance
chain_ladder_expectation(f_t = 1.3, C_it = 500)
chain_ladder_variance(sigma_t_squared = 0.05, C_it = 500)

# Development Factor Estimate
development_factor_estimate(claims_triangle, t = 2)

# Mack's Conditional MSE of Prediction
conditional_mse_prediction(sigma_t_squared = 0.05, f_t = 1.3, C_it = 500, claims_triangle, t = 2)

# Asymptotic Development Factor Limit
asymptotic_development_factor_limit(expected_Z_t_plus1 = 120, expected_Z_t = 100)

# Variance Parameter Estimation
variance_parameter_estimate(f_t = 1.3, expected_Z_squared_t_plus1 = 14400, expected_Z_t_plus1 = 120,
                            expected_Z_squared_t = 10000, expected_Z_t = 100)

# Extend Triangle to Full Rectangle
development_factors <- sapply(1:9, function(t) development_factor_estimate(claims_triangle, t))
completed_rectangle <- complete_rectangle(claims_triangle, development_factors)
completed_rectangle
}
\seealso{
\code{\link[stats]{diff}}, \code{\link[stats]{sum}}
}
\keyword{actuarial}
\keyword{chain ladder}
\keyword{claims reserving}
\keyword{variance estimation}

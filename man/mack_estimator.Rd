\name{mack_estimator}
\alias{mack_estimator}
\title{Mack's Estimator for Claims Reserving with Development Factors and Variance Estimates}
\description{
Computes Mack's estimator for claims reserving in insurance by calculating development factors, variance estimates, and conditional MSE of prediction. This function also extends the claims triangle to a full rectangle (ultimate claims predictions) and can compute asymptotic development factor limits.
}
\usage{
mack_estimator(claims_triangle, severity_mean = NULL, severity_sd = NULL)
}
\arguments{
  \item{claims_triangle}{A numeric matrix representing cumulative claims data organized in a run-off triangle, with rows as accident years and columns as development years.}
  \item{severity_mean}{Optional numeric value; the mean severity of claims. Used for asymptotic limit calculation if provided.}
  \item{severity_sd}{Optional numeric value; the standard deviation of claim severity. Used for asymptotic limit calculation if provided.}
}
\details{
The \code{mack_estimator} function provides a comprehensive estimation for claims reserving using Mack's chain ladder approach. It calculates:
\itemize{
  \item Development factors for each development year based on historical claims data.
  \item Variance estimates for each development year.
  \item Conditional mean squared error (MSE) of prediction.
  \item Completed claims rectangle by extrapolating claims data beyond the observed triangle.
  \item Asymptotic development factor limits (if mean and standard deviation of severity are provided).
}
This function uses several helper functions: \code{development_factor_estimate}, \code{asymptotic_variance_estimation}, \code{complete_rectangle}, and \code{conditional_mse_prediction} to perform its calculations.
}
\value{
  A list containing the following components:
  \item{development_factors}{A numeric vector of development factors for each development year.}
  \item{variance_estimates}{A numeric vector of variance estimates for each development year.}
  \item{mse_prediction}{A matrix of conditional mean squared errors of prediction, with dimensions matching the input \code{claims_triangle}.}
  \item{completed_rectangle}{A numeric matrix representing the completed claims rectangle with ultimate predictions, extending the input \code{claims_triangle}.}
  \item{asymptotic_limits}{A numeric vector of asymptotic development factor limits if \code{severity_mean} and \code{severity_sd} are provided; otherwise \code{NULL}.}
}
\examples{
# Example setup of claims triangle data
set.seed(2)
claims_triangle <- matrix(NA, nrow = 10, ncol = 10)
for (i in 1:10) {
  claims_triangle[i, 1:(11 - i)] <- cumsum(runif(11 - i, min = 100, max = 500))
}

# Run Mack's estimator
result <- mack_estimator(claims_triangle)

# View results
print("Development Factors:")
print(result$development_factors)

print("Variance Estimates:")
print(result$variance_estimates)

print("MSE Prediction:")
print(result$mse_prediction)

print("Completed Rectangle (Ultimate Predictions):")
print(result$completed_rectangle)
}
\seealso{
\code{\link{development_factor_estimate}}, \code{\link{asymptotic_variance_estimation}}, \code{\link{complete_rectangle}}, \code{\link{conditional_mse_prediction}}
}
\keyword{actuarial}
\keyword{chain ladder}
\keyword{claims reserving}
\keyword{variance estimation}

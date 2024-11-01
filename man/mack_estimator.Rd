\name{mack_estimator}
\alias{mack_estimator}
\title{Compute Mack's Estimator for Claims Reserving}
\description{
  This function computes the development factors, variance estimates, and conditional MSE predictions
  for a claims reserving model in a compound Poisson setting using Mack's estimator.
}
\usage{
  mack_estimator(claims_triangle, severity_mean = NULL, severity_sd = NULL)
}
\arguments{
  \item{claims_triangle}{A matrix representing cumulative claims data, where rows are accident years and columns are development years.}
  \item{severity_mean}{Mean severity of claims, if available. Default is NULL.}
  \item{severity_sd}{Standard deviation of claim severity, if available. Default is NULL.}
}
\value{
  A list containing:
  \item{development_factors}{A vector of development factors computed for each development year.}
  \item{variance_estimates}{A vector of variance estimates for each development year.}
  \item{mse_prediction}{A matrix of conditional MSE predictions for the claims triangle.}
  \item{L_alpha}{A matrix of standardized conditional MSE with exposure parameter.}
  \item{asymptotic_limits}{A vector of asymptotic development factor limits, if severity data is provided.}
  \item{conditional_mse_estimate}{A matrix of conditional MSE estimates for each cell in the claims triangle.}
}
\export

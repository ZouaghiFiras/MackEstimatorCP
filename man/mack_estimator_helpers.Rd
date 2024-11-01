\name{mack_estimator_helpers}
\alias{incremental_claims}
\alias{compound_poisson_claims}
\alias{chain_ladder_expectation}
\alias{chain_ladder_variance}
\alias{development_factor_estimate}
\alias{conditional_mse_prediction}
\alias{asymptotic_development_factor_limit}
\alias{variance_parameter_estimate}
\alias{asymptotic_conditional_mse}
\alias{standardized_conditional_mse}
\alias{asymptotic_variance_estimation}
\alias{conditional_mse_estimate}

\title{Mack's Estimator Helper Functions}
\description{
  A collection of helper functions for implementing Mack's estimator 
  in claims reserving models. These functions facilitate the calculation 
  of incremental claims, development factors, conditional mean squared errors, 
  and other parameters relevant to actuarial reserving techniques.
}

\section{Functions}{
  \item{incremental_claims(cumulative_claims)}{
    Computes the incremental claims from cumulative claims data. This function calculates the difference between cumulative claims for successive periods, allowing actuaries to analyze claim development patterns.
  }
  \item{compound_poisson_claims(num_claims, claim_severities)}{
    Computes the total claims from a compound Poisson process representation. This function aggregates claims based on the number of claims and their severities, providing a stochastic model for total claims estimation.
  }
  \item{chain_ladder_expectation(f_t, C_it)}{
    Calculates the expected future claims using the development factor. This function applies the chain-ladder method to estimate future claims based on observed cumulative claims and the development factors.
  }
  \item{chain_ladder_variance(sigma_t_squared, C_it)}{
    Calculates the variance of future claims using the variance parameter. This function helps quantify uncertainty in future claims estimates by computing the variance based on cumulative claims and variance parameters.
  }
  \item{development_factor_estimate(claims_triangle, t)}{
    Estimates the development factor based on cumulative claims in a claims triangle. This function computes the development factor for a specified development period, which is crucial for forecasting future claims.
  }
  \item{conditional_mse_prediction(sigma_t_squared, f_t, C_it, claims_triangle, t)}{
    Computes the conditional mean squared error (MSE) of prediction for the estimated future claims. This function helps assess the accuracy of the predictions made using the chain-ladder method.
  }
  \item{asymptotic_development_factor_limit(expected_Z, expected_Z_t_plus1, expected_Z_t)}{
    Calculates the asymptotic development factor limit, which is important for evaluating the long-term behavior of development factors in claims reserving.
  }
  \item{variance_parameter_estimate(f_t, expected_Z_squared_t_plus1, expected_Z_t_plus1, expected_Z_squared_t, expected_Z_t)}{
    Estimates the variance parameter for future claims based on expected values. This function provides insights into the variability of claim developments over time.
  }
  \item{asymptotic_conditional_mse(C_i_T, C_i_T_minus_i_plus1, f_s)}{
    Calculates the conditional mean squared error in the asymptotic case, which is useful for understanding the long-term accuracy of claims estimates.
  }
  \item{standardized_conditional_mse(C_i_T, C_i_T_minus_i_plus1, sigma_t_squared, f_t, claims_triangle, T, i)}{
    Computes the standardized conditional MSE, incorporating exposure parameters. This function standardizes the conditional MSE, allowing for comparisons across different scenarios.
  }
  \item{asymptotic_variance_estimation(claims_triangle, f_t, t)}{
    Estimates the asymptotic variance for the parameter based on the claims triangle. This function provides a measure of uncertainty around the estimated claims reserves.
  }
  \item{conditional_mse_estimate(C_i_T, sigma_t_squared, f_t, claims_triangle, T, i)}{
    Estimates the conditional mean squared error using specific parameters. This function evaluates the MSE associated with the predicted claims at a certain development stage.
  }
}

\section{Parameters}{
  \describe{
    \item{cumulative_claims}{A numeric vector of cumulative claims data, where each element represents the total claims amount up to a specific development period.}
    \item{num_claims}{An integer representing the total number of claims observed in the dataset.}
    \item{claim_severities}{A numeric vector of claim severities corresponding to each individual claim. This vector indicates the financial impact of each claim.}
    \item{f_t}{The development factor for a specific development year \(t\). This factor is crucial for projecting future claims based on historical development trends.}
    \item{C_it}{The cumulative claims for the \(i\)-th claim at time \(t\), used in various calculations to determine future claims.}
    \item{claims_triangle}{A matrix representing the claims triangle, where rows correspond to accident years and columns correspond to development years. This triangle structure is essential for performing chain-ladder analyses.}
    \item{T}{The maximum development year in the analysis, indicating the final period for which claims are observed.}
    \item{i}{The index of the accident year in the claims triangle, allowing for specific targeting of data in analyses.}
    \item{sigma_t_squared}{A numeric vector of variance parameters for each development year, representing the uncertainty associated with claims development.}
    \item{expected_Z}{A numeric value representing the expected value of the claim severities for a given development period.}
    \item{expected_Z_t_plus1}{Expected values of claim severities at time \(t+1\), reflecting future expectations based on current trends.}
    \item{expected_Z_t}{Expected values of claim severities at time \(t\), which inform current development factors.}
    \item{C_i_T}{Cumulative claims for the \(i\)-th claim at the final time \(T\), essential for final reserve calculations.}
    \item{C_i_T_minus_i_plus1}{Cumulative claims for the \(i\)-th claim at time \(T-i+1\), used in conditional MSE calculations.}
    \item{f_s}{A numeric vector of development factors for all years from \(T-i+1\) to \(T-1\), used for various predictive and error estimations.}
  }
}

\section{Returns}{
  \describe{
    \item{incremental_claims}{A numeric vector of incremental claims, indicating the changes in cumulative claims between successive development periods.}
    \item{compound_poisson_claims}{A numeric value representing the total claims calculated from the compound Poisson process model.}
    \item{chain_ladder_expectation}{A numeric value of the expected future claims based on the cumulative claims and development factors.}
    \item{chain_ladder_variance}{A numeric value of the variance of future claims, reflecting the uncertainty in the estimate.}
    \item{development_factor_estimate}{A numeric value of the estimated development factor for a specified development period.}
    \item{conditional_mse_prediction}{A numeric value of the conditional mean squared error of prediction, quantifying the prediction accuracy.}
    \item{asymptotic_development_factor_limit}{A numeric value of the asymptotic development factor limit, indicating long-term trends in development factors.}
    \item{variance_parameter_estimate}{A numeric value of the estimated variance parameter, providing insights into the variability of claims.}
    \item{asymptotic_conditional_mse}{A numeric value of the asymptotic conditional MSE, useful for long-term estimates.}
    \item{standardized_conditional_mse}{A numeric value of the standardized conditional mean squared error, allowing for comparison across different estimates.}
    \item{asymptotic_variance_estimation}{A numeric value of the estimated asymptotic variance, measuring the uncertainty in reserve estimates.}
    \item{conditional_mse_estimate}{A numeric value of the estimated conditional mean squared error, reflecting prediction accuracy at a specified development stage.}
  }
}

\section{Examples}{
  \code{
    # Example usage of incremental_claims
    cumulative_claims <- c(100, 150, 190, 240)
    incremental_claims_result <- incremental_claims(cumulative_claims)
    print(incremental_claims_result)
    # Output: 50, 40, 50

    # Example usage of compound_poisson_claims
    num_claims <- 5
    claim_severities <- c(10, 20, 30, 40, 50)
    total_claims <- compound_poisson_claims(num_claims, claim_severities)
    print(total_claims)
    # Output: 150

    # Example usage of development_factor_estimate
    claims_triangle <- matrix(c(100, 150, 190, NA, 200, 250, NA, NA), nrow = 2, byrow = TRUE)
    dev_factor <- development_factor_estimate(claims_triangle, 1)
    print(dev_factor)
    # Output: estimated development factor value

    # Example usage of conditional_mse_estimate
    C_i_T <- 500
    sigma_t_squared <- c(10, 20)
    f_t <- c(1.2, 1.1)
    claims_triangle <- matrix(c(100, 150, 200, NA, 250, NA, NA), nrow = 2, byrow = TRUE)
    T <- 2
    i <- 1
    mse_estimate <- conditional_mse_estimate(C_i_T, sigma_t_squared, f_t, claims_triangle, T, i)
    print(mse_estimate)
    # Output: estimated conditional MSE value
  }
}

\references{
  Mack, T. (1993). Distribution-free calculation of the standard errors of reserves. *ASTIN Bulletin*, 23(2), 213-225.
  Mack, T. (1999). The standard error of chain ladder reserve estimates. *Insurance: Mathematics and Economics*, 25(3), 283-290.
}

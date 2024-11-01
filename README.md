# MackEstimatorCP

## Introduction

The `MackEstimatorCP` package implements Mack's estimator, motivated by large exposure asymptotics in a compound Poisson setting. This package is designed specifically for claims reserving in actuarial science, where it allows actuaries to model and forecast future liabilities with a focus on variance estimation and the behavior of large exposures over time.

This package is inspired by the research article titled:
**"Mack’s estimator motivated by large exposure asymptotics in a compound Poisson setting"**.

The paper explores the application of Mack's estimator under compound Poisson models, a method frequently applied in actuarial risk assessment and reserve estimation. It provides an in-depth analysis of the development factors, conditional mean squared error (MSE), and variance parameters required for robust claims prediction.

## Citation

If you use the `MackEstimatorCP` package, please cite:
**Zouaghi, F.** *Mack’s Estimator Motivated by Large Exposure Asymptotics in a Compound Poisson Setting*, [Link to Article or DOI].

---

## Purpose and Approach

This package is developed as part of a course project for **Introduction to Actuarial Methods and Techniques Using R**, instructed by **Najed Ksouri** at the **Institute of Financing and Development (I.FI.D) of the Arab Maghreb**. It aims to provide an accessible R-based solution for actuaries to apply Mack’s estimator in a compound Poisson setting, focusing on scenarios with large exposures.

The methods implemented here follow the theoretical and practical insights from Mack's model, which has been adapted to handle compound Poisson processes. This adaptation is particularly useful for insurers and actuaries who deal with large claims and require accurate prediction intervals and reserve estimates.

## Methods and Implementation

The `MackEstimatorCP` package includes several helper functions to compute core components of Mack’s estimator, such as:

1. **Incremental Claims Representation**: Calculates the incremental claims between development periods.
2. **Compound Poisson Process Representation**: Models claims as a sum of severities over events.
3. **Chain Ladder Assumptions**: Computes expected value and variance of claims under the chain ladder model.
4. **Estimator of Development Factor**: Calculates development factors for the claims triangle.
5. **Mack's Conditional MSE of Prediction**: Provides MSE for future claim estimates, incorporating variance.
6. **Asymptotic Development Factor Limit**: Calculates the limit of development factors as exposures become large.
7. **Variance Parameter Estimator**: Estimates variance parameters for each development period.
8. **Conditional MSE in Asymptotic Case**: Computes the MSE in cases with asymptotic assumptions.
9. **Standardized Conditional MSE with Exposure Parameter**: Standardizes MSE prediction based on exposure.
10. **Asymptotic Variance Estimation**: Estimates the asymptotic variance for each development period.
11. **Estimator for Conditional MSE (Equation 5.1)**: Implements Equation 5.1 for MSE under specific conditions.

These methods allow for a robust and flexible application of Mack’s estimator, especially suited for actuarial tasks involving large exposure reserves.

## Package Details

### Installation

To install the `MackEstimatorCP` package from source, you can use the following R commands:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install MackEstimatorCP package from local source
devtools::install("path/to/MackEstimatorCP")
```

### Example Usage

Load the package and use it to calculate the development factors, variance estimates, and MSE for a sample claims triangle.

```r
library(MackEstimatorCP)

# Example claims triangle
claims_triangle <- matrix(c(
  100, 150, 200, NA,
  120, 180, NA, NA,
  140, NA, NA, NA
), nrow = 3, byrow = TRUE)

# Compute Mack's estimator
result <- mack_estimator(claims_triangle)
print(result)
```

## Insights and Features

The `MackEstimatorCP` package provides insights into the behavior of large exposure risks, focusing on:
- Accurate development factor estimation to model growth in claims over time.
- Conditional MSE calculations that incorporate large exposure assumptions.
- Asymptotic variance estimation, crucial for long-term risk evaluation.

These features make it an essential tool for actuaries dealing with high-variance claims data.

## About the Author

This package was developed by **Zouaghi Firas** as part of a course project at the I.FI.D. Institute, under the supervision of **Najed Ksouri**.

### Contact
- **Email**: firas.zouaghi@enicar.ucar.tn
- **GitHub**: [https://github.com/ZouaghiFiras/MackEstimatorCP](https://github.com/ZouaghiFiras/MackEstimatorCP)
- **LinkedIn**: [https://tn.linkedin.com/in/firas-zouaghi](https://tn.linkedin.com/in/firas-zouaghi)

---

## Course Information

This project was completed as part of the course **Introduction to Actuarial Methods and Techniques Using R Language**, taught by **Instructor Najed Ksouri** at the **Institute of Financing and Development (I.FI.D) of the Arab Maghreb**. The course covers essential actuarial methodologies and aims to provide practical knowledge of R programming in actuarial applications.

## Acknowledgments

Special thanks to Najed Ksouri for guidance throughout the course and to the I.FI.D Institute for supporting actuarial education and research.

---

We hope this package helps actuaries and researchers apply Mack’s estimator in compound Poisson settings effectively, enabling better risk management and reserve estimation for high-exposure claims.



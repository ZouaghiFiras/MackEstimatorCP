# demo_mackEstimatorCP.R

# Load necessary libraries
library(MackEstimatorCP)
library(ggplot2)
library(tidyr)
library(chainladder)

# Function to run the demo
run_demo <- function() {
  
  # Print introductory message
  cat("Welcome to the Interactive Demo for MackEstimatorCP!\n")
  
  # Load RAA data from the chainladder package
  data("RAA", package = "chainladder")
  cat("Cumulative Claims Triangle (RAA Data):\n")
  print(RAA)
  
  # Apply Chain Ladder Method
  chainladder_fit <- Mack(Claims = RAA)
  cat("\nChain Ladder Estimates:\n")
  print(chainladder_fit$estimates)
  
  # Apply Custom Mack Estimator
  custom_fit <- MackEstimatorCP::MackEstimatorCP(Claims = RAA)
  cat("\nCustom Mack Estimates:\n")
  print(custom_fit)
  
  # Prepare data for comparative results
  comparison_df <- data.frame(
    Accident_Year = rownames(RAA),
    Chain_Ladder_Estimate = chainladder_fit$estimates,
    Custom_Mack_Estimate = custom_fit
  )
  
  # Visualization of Comparative Results
  comparison_long <- pivot_longer(comparison_df, 
                                  cols = c("Chain_Ladder_Estimate", "Custom_Mack_Estimate"),
                                  names_to = "Method",
                                  values_to = "Estimate")
  
  ggplot(comparison_long, aes(x = Accident_Year, y = Estimate, color = Method)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = "Comparative Estimates: Chain Ladder vs Custom Mack", 
         x = "Accident Year", 
         y = "Estimate",
         color = "Method")
  
  # Concluding message
  cat("\nDemo completed. Thank you for exploring the MackEstimatorCP package!\n")
}

# Run the demo
run_demo()

# Load required libraries
library(stats)
library(ggplot2)
library(reshape2)

# Function to display a claims triangle as a heatmap
display_triangle <- function(matrix_data, title) {
  melted_data <- melt(matrix_data, varnames = c("Accident Year", "Development Year"))
  ggplot(data = melted_data, aes(x = `Development Year`, y = `Accident Year`, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "blue", na.value = "grey90") +
    labs(title = title, fill = "Claims") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Run a demonstration
run_demo <- function() {
  # Step 1: Simulate claims triangle data
  set.seed(2)
  claims_triangle <- matrix(NA, nrow = 10, ncol = 10)
  for (i in 1:10) {
    claims_triangle[i, 1:(11 - i)] <- cumsum(runif(11 - i, min = 100, max = 500))
  }
  
  # Display the initial claims triangle
  print("Initial Claims Triangle:")
  display_triangle(claims_triangle, "Initial Claims Triangle")
  
  # Step 2: Run Mack's estimator to calculate results
  result <- mack_estimator(claims_triangle)
  
  # Step 3: Display development factors
  print("Development Factors:")
  print(result$development_factors)
  
  # Plot development factors as a bar chart
  bar_data <- data.frame(
    Development_Year = 1:(length(result$development_factors)),
    Development_Factor = result$development_factors
  )
  
  ggplot(bar_data, aes(x = Development_Year, y = Development_Factor)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Development Factors by Year", x = "Development Year", y = "Development Factor") +
    theme_minimal()
  
  # Step 4: Display variance estimates
  print("Variance Estimates:")
  print(result$variance_estimates)
  
  # Plot variance estimates as a line chart
  line_data <- data.frame(
    Development_Year = 1:(length(result$variance_estimates)),
    Variance_Estimate = result$variance_estimates
  )
  
  ggplot(line_data, aes(x = Development_Year, y = Variance_Estimate)) +
    geom_line(color = "darkred", size = 1.2) +
    labs(title = "Variance Estimates by Year", x = "Development Year", y = "Variance Estimate") +
    theme_minimal()
  
  # Step 5: Display MSE prediction
  print("MSE Prediction Matrix:")
  print(result$mse_prediction)
  
  # Display the MSE matrix as a heatmap
  display_triangle(result$mse_prediction, "MSE Prediction Matrix")
  
  # Step 6: Display completed rectangle
  print("Completed Claims Rectangle:")
  print(result$completed_rectangle)
  
  # Display completed rectangle as a heatmap
  display_triangle(result$completed_rectangle, "Completed Claims Rectangle (Ultimate Claims)")
}

# Run the demo
run_demo()

## MackChainLadder Demos
## Author: Markus Gesmann, March 2010, 2015

# Load necessary libraries
library(MackEstimatorCP)
library(ggplot2)
library(tidyr)
library(ChainLadder)
library(lattice)  # Added for plotting

# Load and plot General Insurance data
data("GenIns", package = "ChainLadder")
plot(GenIns)
plot(GenIns, lattice=TRUE)

# Apply Mack Chain Ladder to General Insurance data
GNI <- MackChainLadder(GenIns, est.sigma="Mack")
print(GNI$f)  # Display estimated factors

# Function to run the demo
run_demo <- function() {
  
  # Print introductory message
  cat("Welcome to the Interactive Demo for MackEstimatorCP!\n")
  
  # Load RAA data from the ChainLadder package
  data("RAA", package = "ChainLadder")
  cat("Cumulative Claims Triangle (RAA Data):\n")
  print(RAA)
  
  # Apply Chain Ladder Method
  ChainLadder_fit <- MackChainLadder(RAA)
  cat("\nChain Ladder Estimates:\n")
  print(ChainLadder_fit$estimates)
  
  # Apply Custom Mack Estimator
  custom_fit <- MackEstimatorCP::mack_estimator(RAA)
  cat("\nCustom Mack Estimates:\n")
  print(custom_fit)
  
  # Prepare data for comparative results
  comparison_df <- data.frame(
    Accident_Year = rownames(RAA),
    Chain_Ladder_Estimate = ChainLadder_fit$estimates,
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

# Display GNI results and plots
cat("Variance estimates:\n")
print(GNI$sigma^2)
print(GNI)  # Compare to tables in Mack's 1993 paper
plot(GNI)
plot(GNI, lattice=TRUE)

# Different weights example
# Using alpha=0 will use straight average age-to-age factors
straight_avg <- MackChainLadder(GenIns, alpha=0)$f
cat("\nStraight Average Age-to-Age Factors:\n")
print(straight_avg)

# Example from Mack's 1999 paper with Mortgage data
data("Mortgage", package = "ChainLadder")
plot(Mortgage)
MRT <- MackChainLadder(Mortgage, tail=1.05, tail.sigma=71, tail.se=0.02, est.sigma="Mack")
print(MRT)
plot(MRT, lattice=TRUE)

# Display specific values
cat("\nFactors and Standard Errors from MRT:\n")
print(MRT$f)
print(MRT$f.se)
print(MRT$F.se[3,])
print(MRT$sigma)

# Plot results
plot(MRT) # Observe trends along calendar years.

# Access various outputs
cat("\nCumulative claims:\n")
print(MRT$FullTriangle[,9]/1000)  # C_{i9}
print(MRT$FullTriangle[,10]/1000)  # C_{i,ult}
print(MRT$Mack.S.E[,9]/1000)  # s.e.(C_{i9})

# Access process risk error
cat("\nProcess risk error:\n")
print(MRT$Mack.ProcessRisk)

# Access parameter risk error
cat("\nParameter risk error:\n")
print(MRT$Mack.ParameterRisk)

# Total risk
cat("\nTotal risk standard errors:\n")
print(MRT$Mack.S.E)

# Plot CV by origin period
op <- par(mfrow=c(2,1))
plot(with(summary(MRT)$ByOrigin, Mack.S.E/Ultimate), t="l",
     ylab="CV(Ultimate)", xlab="origin period")
plot(summary(MRT)$ByOrigin[["CV(IBNR)"]], t="l", ylab="CV(IBNR)",
     xlab="origin period")
par(op)

# Display RAA data and apply Mack Chain Ladder
data("RAA", package = "ChainLadder")
plot(RAA)
R <- MackChainLadder(RAA)
print(R)
plot(R)
plot(R, lattice=TRUE)

# Show estimates and variances
cat("\nEstimates and variances from R:\n")
print(R$f)
print(R$sigma^2)

# Different sigma estimates
cat("\nUsing sigma estimates from R:\n")
print(MackChainLadder(RAA, est.sigma=R$sigma[7]))
print(MackChainLadder(RAA, est.sigma=R$sigma[8]))

# Investigate the Mack model in detail
cat("\nModel for first development period:\n")
print(R[["Models"]][[1]])   # Model for first development period
summary(R[["Models"]][[1]]) # Look at the model stats

# Plot residuals
op <- par(mfrow=c(2,2)) 
plot(R[["Models"]][[1]])
par(op)

# Update model to include intercept
newModel <- update(R[["Models"]][[1]], y ~ x + 1,
                   weights = 1/R[["Triangle"]][1:9, 1],
                   data = data.frame(x = R[["Triangle"]][1:9, 1],
                                     y = R[["Triangle"]][1:9, 2])
)

# View the new model
summary(newModel)
op <- par(mfrow=c(2,2))
plot(newModel)
par(op)

# Change the model for dev. period one to the newModel
R2 <- R
R2[["Models"]][[1]] <- newModel
predict(R2)  # Predict the full triangle with the new model

# Update the FullTriangle in R2
R2[["FullTriangle"]] <- predict(R2)$FullTriangle
cat("\nUpdated Full Triangle:\n")
print(R2[["FullTriangle"]])

# Display R2 results
R2  # Std. Errors have not been re-estimated!
# Plot the result
plot(R2, title="Changed R Model")

## Long table with claims development by line of business
# Create a table similar to what you would get from a 'real' database
myList <- list("General Liability" = RAA/1e3,
               "General Insurance" = GenIns/1e3,
               "Workers Comp" = ABC/1e3,
               "Mortgage Guarantee" = Mortgage/1e3)

myData <- do.call("rbind", lapply(names(myList),
                                  function(x) as.data.frame(myList[[x]], lob=x, na.rm=TRUE)))

# Normalize origin years
myData <- do.call("rbind",
                  by(myData, list(lob=myData$lob),
                     function(x) {
                       org = as.numeric(as.character(x$origin))
                       x$origin <- org - min(org) + 2000
                       x
                     }
                  ))
rownames(myData) <- NULL

# Plot claims development
head(myData)  # Check data structure
xyplot(value ~ dev | lob, groups=factor(origin), data=myData, t="l",
       scales="free", auto.key=list(space="right", points=FALSE, lines=TRUE))

# Create triangles again and apply MackChainLadder for each LOB
myResults <- by(myData, list(lob=myData$lob), function(x)
  MackChainLadder(as.triangle(x), est.sigma="Mack"))

# Display the output
myResults

# Summarize all results by origin period in one data frame
by.origin <- function(x) {
  data.frame(lob = x,
             origin = dimnames(myResults[[x]]$Triangle)$origin,
             summary(myResults[[x]])$ByOrigin)
}

ByOrigin <- do.call("rbind", lapply(names(myResults), by.origin))
print(ByOrigin)

# Similar for the totals
Totals <- do.call("rbind", lapply(names(myResults),
                                  function(x) data.frame(LOB = x, t(summary(myResults[[x]])$Totals))))
print(Totals)

# Bar chart for the latest and IBNR
barchart(Latest + IBNR ~ factor(origin) | lob, stack=TRUE, data=ByOrigin,
         scale="free", auto.key=TRUE, as.table=TRUE, xlab="origin")

## One year claims development result
# Example from the 2008 Merz & Wuthrich paper 
data("MW2008", package = "ChainLadder")
M <- MackChainLadder(MW2008, tail=0.5, est.sigma="Mack")
print(M)
plot(M)

# Provide clarity on different claim types
cat("\nClaim types included:\n")
print(dimnames(MW2008)[[1]])

# Concluding remarks
cat("\nDemo concluded! Thank you for participating!\n")


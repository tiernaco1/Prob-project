simulate_temperature <- function(dt) {
  Tmax_values <- c(100000) # creates a empty vector for results
  
  # Monte Carlo simulation
  for (i in 1:100000) {
    # stores the increments as a vector
    increments <- rnorm(1/dt, mean = 0, sd = sqrt(dt)) # variance = dt => sd = sqrt(dt)
    
    X <- c(0, cumsum(increments)) # makes a vector of the cumulative value
    
    #which.max() finds the point X is greatest
    Tmax_values[i] <- which.max(X) * dt # the multply bt dt to find the time its greatest
  }
  
  return(Tmax_values) # Return the Tmax values from all simulations
}

# Set simulation parameters
dt <- 0.01  # Time interval ∆t
# Run Simulation
Tmax_values <- simulate_temperature(dt)

# Plot histogram 1
hist(Tmax_values, breaks = 50, probability = TRUE, main = "dt = 0.01",
     xlab = "Time temperature is max", ylab = "Density", col = "skyblue", border = "white")

# Set simulation parameters
dt <- 0.001  # Time interval ∆t
# Run Simulation
Tmax_values <- simulate_temperature(dt)
# Plot histogram 2
hist(Tmax_values, breaks = 50, probability = TRUE, main = paste("dt = 0.001"),
     xlab = "Time temperature is max", ylab = "Density", col = "skyblue", border = "white")

# Set simulation parameters
dt <- 0.0001  # Time interval ∆t
# Run Simulation
Tmax_values <- simulate_temperature(dt)
# Plot histogram 3
hist(Tmax_values, breaks = 50, probability = TRUE, main = paste("dt = 0.0001"),
     xlab = "Time temperature is max", ylab = "Density", col = "skyblue", border = "white")

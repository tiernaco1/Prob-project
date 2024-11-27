
# Function to calculate the proportion of time the temperature is positive
simulate_temperature <- function(dt) {
  steps <- 1 / dt  # Number of steps for dt
  proportions <- c(100000)
  for (i in 1:100000) {
    # stores the increments as a vector
    increments <- rnorm(steps, mean = 0, sd = sqrt(dt)) # variance = dt => sd = sqrt(dt)
    
    process <- cumsum(increments) # makes a vector of the cumulative value
    
    # Calculate the proportion of time the temperature is positive
    positive <- 0
    for (x in process) {
      if (x > 0) {
        positive <- positive + 1
      }
    }
    proportions[i] <- positive / steps
  }
  return(proportions)
}

# Set simulation parameters
dt <- 0.01  # Time interval ∆t
# Run simulation
proportions <- simulate_temperature(dt)

# Plot histogram 1
hist(proportions, breaks = 50, probability = TRUE, main = "dt = 0.01",
     xlab = "Time proportion tempureatue is postive", ylab = "Density", col = "skyblue", border = "white")

# Set simulation parameters
dt <- 0.001  # Time interval ∆t
# Run simulation
proportions <- simulate_temperature(dt)

# Plot histogram 2
hist(proportions, breaks = 50, probability = TRUE, main = "dt = 0.001",
     xlab = "Time proportion tempureatue is postive", ylab = "Density", col = "skyblue", border = "white")

# Set simulation parameters
dt <- 0.0001  # Time interval ∆t
# Run simulation
proportions <- simulate_temperature(dt)

# Plot histogram 3
hist(proportions, breaks = 50, probability = TRUE, main = "dt = 0.0001",
     xlab = "Time proportion tempureatue is postive", ylab = "Density", col = "skyblue", border = "white")

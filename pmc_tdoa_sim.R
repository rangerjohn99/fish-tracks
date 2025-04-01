# Simulate acoustic data (replace with your actual data)
simulate_data <- function(true_location, receiver_locations, noise_sd = 1) {
  arrival_times <- sapply(receiver_locations, function(receiver) {
    dist <- sqrt(sum((true_location - receiver)^2))
    dist + rnorm(1, 0, noise_sd) # Add noise
  })
  return(arrival_times)
}

# Example receiver locations (replace with your actual locations)
receiver_locations <- list(c(1, 1), c(5, 1), c(3, 5))

# True location (for simulation purposes)
true_location <- c(3, 3)

# Simulated arrival times
observed_arrival_times <- simulate_data(true_location, receiver_locations)

# PMC Algorithm
pmc_localization <- function(observed_arrival_times, receiver_locations, num_particles = 1000, iterations = 100) {
  # Initialize particles (random locations)
  particles <- matrix(runif(num_particles * 2, 0, 10), ncol = 2) # Adjust the range (0, 10) according to your environment.
  weights <- rep(1 / num_particles, num_particles)
  
  for (i in 1:iterations) {
    # Calculate predicted arrival times for each particle
    predicted_arrival_times <- apply(particles, 1, function(particle) {
      sapply(receiver_locations, function(receiver) {
        sqrt(sum((particle - receiver)^2))
      })
    })
    
    # Calculate likelihood (assuming Gaussian noise)
    likelihoods <- apply(predicted_arrival_times, 2, function(predicted) {
      prod(dnorm(observed_arrival_times - predicted, 0, 0.2)) #Adjust standard deviation (0.2)
    })
    
    # Update weights
    weights <- weights * likelihoods
    weights <- weights / sum(weights)
    
    # Resampling (systematic resampling)
    cumulative_weights <- cumsum(weights)
    u <- (runif(num_particles) + (0:(num_particles - 1))) / num_particles
    resampled_indices <- sapply(u, function(ui) {
      min(which(cumulative_weights >= ui))
    })
    particles <- particles[resampled_indices, ]
    weights <- rep(1 / num_particles, num_particles)
  }
  
  # Estimate location (weighted mean)
  estimated_location <- colSums(particles * weights)
  return(list(estimated_location = estimated_location, particles = particles, weights = weights))
}

# Run PMC
result <- pmc_localization(observed_arrival_times, receiver_locations)

# Output
print(result$estimated_location)

#Visualize the particles
plot(result$particles[,1], result$particles[,2], main = "Particle Distribution")
points(true_location[1], true_location[2], col = "red", pch = 19)
points(result$estimated_location[1], result$estimated_location[2], col = "blue", pch = 19)
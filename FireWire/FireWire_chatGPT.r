# Generate random points
N <- 1000
X <- runif(N, -10, 10)
Y <- runif(N, -10, 10)

# Calculate distances between each pair of points
distances <- sqrt((outer(X, X, "-")^2) + (outer(Y, Y, "-")^2))

# Create a connectome based on the distances
connectome <- matrix(0, N, N)
for (i in 1:N) {
  accessible <- which(distances[i, ] < 1.3 & distances[i, ] > 0)
  if (length(accessible) > 2) {
    nbconnexi <- sample(2:length(accessible), 1)
    connexi <- sample(accessible, nbconnexi)
    connectome[i, connexi] <- 1
  }
}

# Add random weights to the connections
puissance <- connectome * matrix(rnorm(N^2, 0, 1), N, N)

# Visualize the network
plot(Y ~ X)
for (i in 1:N) {
  connectedto <- which(connectome[i, ] == 1)
  if (length(connectedto) > 0) {
    arrows(X[i], Y[i], X[connectedto], Y[connectedto], length = 0.05,
           lwd = abs(puissance[i, connectedto]), col = I(2 + sign(puissance[i, connectedto])))
  }
}

# Select input neurons and initialize charge
ninput <- 6
nbconfrom <- rowSums(connectome)
InputNeurons <- order(nbconfrom)[I(length(nbconfrom) - ninput + 1):length(nbconfrom)]
charge <- rep(0, N)

# Simulate the network dynamics
for (tps in 1:1000) {
  # Apply input to the network
  if (tps %% 10 == 1) charge[InputNeurons] <- 10
  
  # Identify firing neurons and update charge
  firing <- which(charge > 1)
  charge[firing] <- 0
  
  # Update the unavailable and charging neurons
  unavailable <- ifelse(firing > 0, 5, pmax(0, unavailable - 1))
  charging <- if (length(firing) > 1) colSums(puissance[firing, ]) else if (length(firing) == 1) puissance[firing, ] else charge * 0
  charging[unavailable > 0] <- 0
  
  # Update charge and visualize network activity
  charge <- charge + charging
  fire <- rep(1, N)
  fire[firing] <- 2
  plot(Y ~ X, col = fire, pch = I((fire - 1) * 18) + 1)
  
  # Update connection weights between firing neurons and their inputs
  for (i in 1:length(firing)) {
    reinforcelinkfrom <- which(connectome[, firing[i]] == 1 & lastfiring %in% InputNeurons)
    puissance[reinforcelinkfrom, firing[i]] <- puissance[reinforcelinkfrom, firing[i]] * 1.1
  }
  
  lastfiring <- firing
}

numNeurons <- 1000
maxAllowedCharge <- 1  # Set this to a suitable value


xCoordinates <- runif(numNeurons, -10, 10)
yCoordinates <- runif(numNeurons, -10, 10)

# Calculating distances between all pairs of neurons
pairwiseDistances <- matrix(0, numNeurons, numNeurons)
for (i in 1:numNeurons) {
  for (j in 1:numNeurons) {
    pairwiseDistances[i, j] <- sqrt((xCoordinates[j] - xCoordinates[i])^2 + (yCoordinates[j] - yCoordinates[i])^2)
  }
}

# Constructing the network of neurons
neuronNetwork <- matrix(0, numNeurons, numNeurons)

for (i in 1:numNeurons) {
  accessibleNeurons <- which(pairwiseDistances[i, ] < 1.3)
  if (length(accessibleNeurons) > 2) {
    accessibleNeurons <- accessibleNeurons[accessibleNeurons != i]
    numConnections <- sample(2:length(accessibleNeurons), 1)
    connections <- sample(accessibleNeurons, numConnections)
    neuronNetwork[i, connections] <- 1
  }
}

# Generating random strengths for each connection
connectionStrengths <- neuronNetwork * matrix(rnorm(numNeurons^2, 0, 1), numNeurons, numNeurons)

# Visualizing the network of neurons
plot(yCoordinates ~ xCoordinates)
for (i in 1:numNeurons) {
  connectedNeurons <- which(neuronNetwork[i, ] == 1)
  if (length(connectedNeurons) > 0) {
    arrows(x0 = rep(xCoordinates[i], length(connectedNeurons)), y0 = rep(yCoordinates[i], length(connectedNeurons)),
           x1 = xCoordinates[connectedNeurons], y1 = yCoordinates[connectedNeurons],
           length = 0.05, lwd = abs(connectionStrengths[i, connectedNeurons]), col = I(2 + sign(connectionStrengths[i, connectedNeurons])))
  }
}

# Finding the input and output neurons
numConnectionsFrom <- rowSums(neuronNetwork)
numInputNeurons <- 6
InputNeurons <- tail(order(numConnectionsFrom), numInputNeurons)

numConnectionsTo <- colSums(neuronNetwork)
numOutputNeurons <- 2
OutputNeurons <- tail(order(numConnectionsTo), numOutputNeurons)

# Copying the original connection strengths
originalStrengths <- connectionStrengths

# Initializing variables for the simulation
neuronCharges <- rep(0, numNeurons)
unavailableNeurons <- rep(0, numNeurons)

windows()

# Simulating the firing of neurons
neuronCharges[InputNeurons] <- 10
lastFiredNeurons <- c()
counter <- 0
for (time in 1:1000) {
  # Pause for 0.1 second (adjust as needed)
  Sys.sleep(0.1)
  
  counter <- counter + 1
  if (counter == 1) {
    neuronCharges[InputNeurons] <- 10
    counter <- 0
  }

  firingNeurons <- which(neuronCharges >= 1)
  neuronCharges[firingNeurons] <- 0
  unavailableNeurons[firingNeurons] <- 5
  unavailableNeurons <- unavailableNeurons - 1
  unavailableNeurons[unavailableNeurons < 0] <- 0
  neuronCharges <- 1 * neuronCharges

  if (length(firingNeurons) > 1) {
    chargingNeurons <- colSums(connectionStrengths[firingNeurons, ])
  } else if (length(firingNeurons) == 1) {
    chargingNeurons <- connectionStrengths[firingNeurons, ]
  } else {
    chargingNeurons <- neuronCharges * 0
  }
  
  chargingNeurons[unavailableNeurons > 0] <- 0
  neuronCharges <- neuronCharges + chargingNeurons
  
  # Limit the charge to the maximum allowed charge
  neuronCharges[neuronCharges > maxAllowedCharge] <- maxAllowedCharge

  for (i in 1:length(firingNeurons)) {
    reinforceLinksFrom <- lastFiredNeurons[neuronNetwork[lastFiredNeurons, firingNeurons[i]] == 1]
    connectionStrengths[reinforceLinksFrom, firingNeurons[i]] <- connectionStrengths[reinforceLinksFrom, firingNeurons[i]] * 1.1
  }

  # lastFiredNeurons <- firingNeurons
  # fire <- rep(1, numNeurons)
  # fire[firingNeurons] <- 2
  # plot(yCoordinates ~ xCoordinates, col = fire, pch = (fire - 1) * 18 + 1)
  
  # Normalize charge for plotting (adjust the scaling factor as needed)
  neuronSize <- neuronCharges / max(neuronCharges) * 2

  lastFiredNeurons <- firingNeurons
  fire <- rep(1, numNeurons)
  fire[firingNeurons] <- 2
  plot(yCoordinates ~ xCoordinates, col = fire, pch = (fire - 1) * 18 + 1, cex = neuronSize)
  
  # Add maximum charge as text to the plot
  # text(xCoordinates[1], yCoordinates[1], paste("Max charge:", round(max(neuronCharges), 2)), pos = 4, col = "red")
  title(main=paste("Max charge:", round(max(neuronCharges), 2)), col = "red")
}


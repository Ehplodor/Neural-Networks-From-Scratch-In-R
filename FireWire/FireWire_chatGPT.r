N <- 1000
X <- runif(N, -10, 10)
Y <- runif(N, -10, 10)

distances <- matrix(0, N, N)
for (i in 1:N) {
  for (j in 1:N) {
    distances[i, j] <- sqrt((X[j] - X[i])^2 + (Y[j] - Y[i])^2)
  }
}

connectome <- matrix(0, N, N)

for (i in 1:N) {
  accessible <- which(distances[i, ] < 1.3)
  if (length(accessible) > 2) {
    accessible <- accessible[accessible != i]
    nbconnexi <- sample(2:length(accessible), 1)
    connexi <- sample(accessible, nbconnexi)
    connectome[i, connexi] <- 1
  }
}

puissance <- connectome * matrix(rnorm(N^2, 0, 1), N, N)

plot(Y ~ X)
for (i in 1:N) {
  connectedto <- which(connectome[i, ] == 1)
  if (length(connectedto) > 0) {
    arrows(x0 = rep(X[i], length(connectedto)), y0 = rep(Y[i], length(connectedto)),
           x1 = X[connectedto], y1 = Y[connectedto],
           length = 0.05, lwd = abs(puissance[i, connectedto]), col = I(2 + sign(puissance[i, connectedto])))
  }
}

nbconfrom <- rowSums(connectome)
ninput <- 6
InputNeurons <- tail(order(nbconfrom), ninput)

nbconto <- colSums(connectome)
noutput <- 2
OutputNeurons <- tail(order(nbconto), noutput)

puissance2 <- puissance

puissance <- puissance2
charge <- rep(0, N)
unavailable <- rep(0, N)

windows()

charge[InputNeurons] <- 10
lastfiring <- c()
count <- 0
for (tps in 1:1000) {
  count <- count + 1
  if (count == 1) {
    charge[InputNeurons] <- 10
    count <- 0
  }

  firing <- which(charge > 1)
  charge[firing] <- 0
  unavailable[firing] <- 5
  unavailable <- unavailable - 1
  unavailable[unavailable < 0] <- 0
  charge <- 1 * charge

  if (length(firing) > 1) {
    charging <- colSums(puissance[firing, ])
  } else if (length(firing) == 1) {
    charging <- puissance[firing, ]
  } else {
    charging <- charge * 0
  }
  charging[unavailable > 0] <- 0
  charge <- charge + charging

  for (i in 1:length(firing)) {
    reinforcelinkfrom <- lastfiring[connectome[lastfiring, firing[i]] == 1]
    puissance[reinforcelinkfrom, firing[i]] <- puissance[reinforcelinkfrom, firing[i]] * 1.1
  }

  lastfiring <- firing
  fire <- rep(1, N)
  fire[firing] <- 2
  plot(Y ~ X, col = fire, pch = (fire - 1) * 18 + 1)
}


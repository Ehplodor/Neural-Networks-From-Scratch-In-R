

# test/verification

# Préparation du jeu d'entrainement XORx3
X1 <- sample(c(0,1),1000,replace=TRUE)
X2 <- sample(c(0,1),1000,replace=TRUE)
X3 <- sample(c(0,1),1000,replace=TRUE)

X <- rbind(X1,X2,X3)
Y1 <- as.numeric(xor(xor(X1,X2),X3))
# Y1 <- as.numeric((X1&X2))
Y <- rbind(Y1)


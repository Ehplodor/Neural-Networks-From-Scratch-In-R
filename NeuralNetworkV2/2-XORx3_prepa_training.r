
# Préparation du jeu d'entrainement XORx3
X1 <- sample(c(0,1),100,replace=TRUE)
X2 <- sample(c(0,1),100,replace=TRUE)
X3 <- sample(c(0,1),100,replace=TRUE)

X <- rbind(X1,X2,X3)
Y1 <- as.numeric(xor(xor(X1,X2),X3))
# Y1 <- as.numeric((X1&X2))
Y <- rbind(Y1)




H <- 0.7		# learning rate (entre 0 et 1)
rorre <- .01		# erreur seuil pour stopper l'apprentissage

# Astuce pour récupérer tous les éléments nommés pareil
# sapply(RB, with, c(id,type,connectedfrom,fire,weight))

Couches <- c(3,3,1)

RB <- brain(c(Couches))

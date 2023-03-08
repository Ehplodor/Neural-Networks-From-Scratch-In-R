HebbOptim <- function(NN, X, Y){

	N0N2W <- NN[1]
	N0N3W <- NN[2]
	
	N1N2W <- NN[1]
	N1N3W <- NN[2]
	
	N2N4W <- NN[3]
	N3N4W <- NN[4]
	
	N0 <- 1 # ordonnée à l'origine
	
	N1 <- 0 # entrée (X)
	
	N2 <- 0
	N3 <- 0
	
	N4 <- 0 # sortie (Yobservé)
	
	N1 <- X
	
	N2 <- N1N2W*N1+N0N2W*N0
	N3 <- N1N3W*N1+N0N3W*N0
	
	N4 <- N2N4W*N2+N3N4W*N3
	
	# recompense <- 1/(1+abs(Y-N4))
	recompense <- 1/(abs(Y-N4))
	
	# le renforcement des connections doit être proportionnel à "recompense" ET au degré d'activation effectif de chaque neurone... Plus 2 neurones sont activés ensemble, plus leur lien se renforce
	# N0N2W <- N0N2W*recompense*abs(N0*N2)
	# N0N3W <- N0N3W*recompense*abs(N0*N3)
	
	# N1N2W <- N1N2W*recompense*abs(N1*N2)
	# N1N3W <- N1N3W*recompense*abs(N1*N3)
	
	# N2N4W <- N2N4W*recompense*abs(N2*N4)
	# N3N4W <- N3N4W*recompense*abs(N3*N4)

	N0N2W <- N0N2W+recompense*abs(N0*N2)*0.01
	N0N3W <- N0N3W+recompense*abs(N0*N3)*0.01
	
	N1N2W <- N1N2W+recompense*abs(N1*N2)*0.01
	N1N3W <- N1N3W+recompense*abs(N1*N3)*0.01
	
	N2N4W <- N2N4W+recompense*abs(N2*N4)*0.01
	N3N4W <- N3N4W+recompense*abs(N3*N4)*0.01
	
	
	# NNupd <- c(N0N2W,N0N3W,N1N2W,N1N3W,N2N4W,N3N4W)/max(abs(c(N0N2W,N0N3W,N1N2W,N1N3W,N2N4W,N3N4W)))
	# NNupd <- c(N0N2W,N0N3W,N1N2W,N1N3W,N2N4W,N3N4W)*(1+exp(-(abs(c(N0N2W,N0N3W,N1N2W,N1N3W,N2N4W,N3N4W)))))
	NNupd <- c(N0N2W,N0N3W,N1N2W,N1N3W,N2N4W,N3N4W)/sd((c(N0N2W,N0N3W,N1N2W,N1N3W,N2N4W,N3N4W)))

	print(c(Y, N4, recompense))
	return(NNupd)
	

}

NNexec <- function(NN, X){

	N0N2W <- NN[1]
	N0N3W <- NN[2]
	
	N1N2W <- NN[3]
	N1N3W <- NN[4]
	
	N2N4W <- NN[5]
	N3N4W <- NN[6]
	
	N0 <- 1
	
	N1 <- 0 # entrée (X)
	
	N2 <- 0
	N3 <- 0
	
	N4 <- 0 # sortie (Yobservé)
	
	N1 <- X
	
	N2 <- N1N2W*N1+N0N2W*N0
	N3 <- N1N3W*N1+N0N3W*N0
	
	N4 <- N2N4W*N2+N3N4W*N3
	

	return(N4)
	

}


mafonctionsimple <- function(x){
	y <- 1
	if(x>0.5){y <- 2}
	
	return(y)
}

monNN <- rnorm(4)
print(monNN)

for(i in 1:10000){
	mX <- runif(1)
	mY <- mafonctionsimple(mX)
	
	monNN <- HebbOptim(NN=monNN, X=mX, Y=mY)
	
}

print(monNN)


result <- c()
mXsave <- c()
for(i in 1:1000){
	mX <- runif(1)
	mY <- mafonctionsimple(mX)
	
	mXsave <- c(mXsave, mX)
	result <- c(result,NNexec(NN=monNN, X=mX))
	
}


plot(result ~ mXsave)

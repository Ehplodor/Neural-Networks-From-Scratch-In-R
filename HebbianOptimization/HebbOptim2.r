# HebbOptim2

# Sigmmoid
act <- function(a){return(1/(1+exp(-a)))}
act2 <- function(a){return(-1+2/(1+exp(-a)))}

HebbOptim2 <- function(W, X, Y){
	
	YnotNA <- !is.na(Y)
	
	S <- X
	Wold <- W
	alpha <- 0.1
	
	nbNeurons <- nrow(W)
	
	# W is "carré" weight matrix looking like
	#	from 	N1	N2	N3
	# 	to
	# 	N1		w11	w12	w13
	# 	N2		w21	w22	w23
	# 	N3		w31	w32	w33
	
	# X is Input signal to every neurons of the network
	
	# Sacc = accumulator of signals
	# ==> to estimate link force between neurons
	# the more, the better
	# Sacc <- matrix(0, nbNeurons, nbNeurons)
	
	# update rule : Wnew = Wold + learningRate[~0.01] * recompense[+1 OR -1] * act2(Sacc)
	
	# Sacc is squashed to between -1 and +1 interval to be useful
	
	# recompense depends on wether if updated weights with +1 factor recompense makes better or worse output than non updated weights.
	# if updated weights perform worse, re-update from non updated Weights using -1 recompense factor (punition) in hope to go in right direction. Because that would mean the network has "learned" a worse weights matrix
	
	# and learning rate is a constant
	
	Sacc1 <- matrix(0, nbNeurons, nbNeurons)
	
	
	S0 <- S
	# Sacc1 <- Sacc1+S0 # pour l'instant, je ne sais pas si c'est utile
	
	# step 1
	S1 <- S0*Wold
	Sacc1 <- Sacc1+S1
	S1 <- act(colSums(S1))
	
	
	# step 2
	S2 <- S1*Wold
	Sacc1 <- Sacc1+S2
	S2 <- colSums(S2)
	
	print(round(S2,3))	
	
	# on arrête à 2 steps
	
	# évaluer la qualité des sorties
	# SS1 <- sum(abs(S2-Y))
	SS2 <- sum((S2[YnotNA]-Y[YnotNA])^2)
	
	
	
	# update Weights first time (recompense = +1)
	Wnewplus = Wold + alpha * act2(Sacc1)
	
	# redo everything with new weights
	Sacc <- matrix(0, nbNeurons, nbNeurons)

	S0 <- S
	# Sacc <- Sacc+S0 # pour l'instant, je ne sais pas si c'est utile
	
	# step 1
	S1 <- S0*Wnewplus
	Sacc <- Sacc+S1
	S1 <- act(colSums(S1))
	
	# step 2
	S2 <- S1*Wnewplus
	Sacc <- Sacc+S2
	S2 <- colSums(S2)
		
	print(round(S2,3))	

	# on arrête à 2 steps
	
	# évaluer la qualité des sorties
	# SS3 <- sum(abs(S2-Y))
	SS4 <- sum((S2[YnotNA]-Y[YnotNA])^2)
	
	
	
	# update Weights second time (recompense = -1)
	Wnewmin = Wold - alpha * act2(Sacc1)
	
	# redo everything with new weights
	Sacc <- matrix(0, nbNeurons, nbNeurons)
	
	S0 <- S
	# Sacc <- Sacc+S0 # pour l'instant, je ne sais pas si c'est utile
	
	# step 1
	S1 <- S0*Wnewmin
	Sacc <- Sacc+S1
	S1 <- act(colSums(S1))
	
	# step 2
	S2 <- S1*Wnewmin
	Sacc <- Sacc+S2
	S2 <- colSums(S2)
	
	print(round(S2,3))	
	# on arrête à 2 steps
	
	# évaluer la qualité des sorties
	# SS5 <- sum(abs(S2-Y))
	SS6 <- sum((S2[YnotNA]-Y[YnotNA])^2)
	
	print(paste("G2 = ", SS2))
	print(paste("G4 = ", SS4))
	print(paste("G6 = ", SS6))
	
	if(SS4 < SS6){	# lower is better
	
		return(Wnewplus)
		
	}else{
	
		return(Wnewmin)
	
	}
	
	
	
}


Nneu <- 100
m1 <- matrix(rnorm(Nneu*Nneu),Nneu,Nneu)
X <- c(1,rep(0, (Nneu-1)))
Y <- c(rep(NA, (Nneu-1)),1)
mold <- m1
for(i in 1:2000){
	m1 <- HebbOptim2(m1, X, Y)
}
round(m1,2)



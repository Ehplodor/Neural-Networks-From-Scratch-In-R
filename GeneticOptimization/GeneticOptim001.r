# algorithme d'optimisation "génétique"

# Objectif : approximer la fonction ET avec un réseau de neurones simples

source("C:/Users/hades/Documents/NeuralNetworks/BoucleNN/fonctions communes.r")


# Thermodynamic simulated annealing algorithm
# Algorithm TSA (Solution: configuration; T0, Tend: temperature;
# kA: run-time parameter) return Solution
# Begin
# k = 1;
# T = T0;
# Loop
	# NewSolution = Neighbor(Solution);
	# DCk = cost(NewSolution) − cost(Solution);
	# Pk = exp(−DCk/Tk);
	# If (RANDOM (0, 1)<Pk) then
		# Solution = NewSolution;
		# DCT = DCT + DCk ; --Update total cost variation
	# End if;
	# If (DCk > 0)
		# DST = DST − DCk/Tk; --Update total entropy variation
	# If (DCT >= 0 or DST = 0) --Update temperature
		# T = T0;
	# Else
		# T = kA(DCT /DST );
	# End if;
	# k = k + 1;
	# While (T >Tend or T is not stabilized);
		# --Initially, T may suffer large fluctuations beyond Tend.
		# --These fluctuations must be neglected to avoid the premature
		# --convergence of the algorithm.
		# Return Solution;
# End TSA;


# optimiser les coefficients a et b pour refléter la fonction y = 3 * x + 2

nbI <- 100
T0 <- 0.01	# température initiale

maxk <- 1000

# init coefs aa, bb et cc

aabest <- 1
bbbest <- 1
ccbest <- 1


xx <- c(-10:10)
yy <- 3*xx+2

Tk <- T0	# température


aarec <- c()
bbrec <- c()
ccrec <- c()
abmprec <- c()

# total cost variation
DCT <- 0
DST <- 0
k <- 1

abmaxperf <- 10000

# while(T > 0.001 | k < maxk){	# k = n° de génération
while(abmaxperf > 0.001 & k < maxk){	# k = n° de génération
	print(paste("############# k=",k))

	aa <- c(rnorm(nbI,aabest,Tk))
	bb <- c(rnorm(nbI,bbbest,Tk))
	cc <- c(rnorm(nbI,ccbest,Tk))
	
	
	# évaluer les performances de chaque couple a,b (= chaque individu)
	abperf <- c()
	for(i in 1:nbI){
		abperf[i] <- sum(abs(cc[i]*xx^2+aa[i]*xx+bb[i]-yy))
	}
	
	# choix du meilleur ensemble de coefficients

	abmaxperf <- min(abperf)
	
	print(paste("# abmaxperf=",abmaxperf))
	
	abmprec[k] <- abmaxperf

	if(k==1){
		
		plot.new()
		plot.window(xlim=c(0,maxk), ylim=c(0,I(abmaxperf*2)))

	}
	
	
	# ajustement de la température

	if(k > 1){
		

			abbest <- which(abperf==abmaxperf)
			aabest <- aa[abbest]
			bbbest <- bb[abbest]
			ccbest <- cc[abbest]

			aarec[k] <- aabest
			bbrec[k] <- bbbest
			ccrec[k] <- ccbest


			print(paste("generation ", k, " : a=",aabest," ; b=",bbbest," ; c=",ccbest))
			
	}


	k <- k + 1	

}

# évolution de l'erreur du meilleur ensemble de poids
plot(abmprec)







# signal depuis i vers j


N <- 1000
X <- runif(N,-10,10)# rnorm(N,0,1)
Y <- runif(N,-10,10)# rnorm(N,0,1)
# Z <- rnorm(N,0,1)



distances <- array(rep(0, N^2),c(N,N))
for(i in 1:N){
	for(j in 1:N){
		distances[i,j] <- sqrt((X[j]-X[i])^2+(Y[j]-Y[i])^2)
	}
}

connectome <- array(rep(0, N^2),c(N,N)) 		#####

for(i in 1:N){
	accessible <- which(distances[i,] < 1.3)
	if(length(accessible)>2){
		accessible <- accessible[-which(accessible == i)] #  pas d'auto-connexion
		nbconnexi <- sample(c(2:length(accessible)),1)
		connexi <- sample(accessible,nbconnexi)
		connectome[i,connexi] <- 1
	}
}

puissance <- connectome*array(rnorm(N^2,0,1),c(N,N)) 		#####


# visualisation du réseau
plot(Y~X)
for(i in 1:N){
	connectedto <- which(connectome[i,]==1)
	if(length(connectedto)>0){
		arrows(x0=rep(X[i],length(connectedto)), y0=rep(Y[i],length(connectedto)), x1 <- X[connectedto], y1 <- Y[connectedto],
		length=0.05, lwd=abs(puissance[i,][connectedto]), col=I(2+sign(puissance[i,][connectedto])))
	}
}

nbconfrom <- rowSums(connectome)

ninput <- 6

InputNeurons <- order(nbconfrom)[I(length(nbconfrom)-ninput+1):length(nbconfrom)]

nbconto <- colSums(connectome)

# noutput <- 2

# OutputNeurons <- order(nbconto)[I(length(nbconto)-noutput+1):length(nbconto)]

puissance2 <- puissance


##### neurons that fire together, wire together !

puissance <- puissance2
charge <- rep(0,N)
unavailable <- rep(0,N)

windows()
charge[InputNeurons] <- 10
lastfiring <- c()
count <- 0
for(tps in 1:1000){
	count <- count+1
	if(count==1){
		charge[InputNeurons] <- 10
		count <- 0
	}
	
	firing <- which(charge>1)	# id des neurones sufisamment chargés pour "fire"	
	
	charge[firing] <- 0		# décharge complète des neurones "firing"
	
	unavailable[firing] <- 5		# début de phase insensible
	unavailable <- unavailable-1
	unavailable[which(unavailable<0)] <- 0
	
	charge <- 1*charge	# décharge
	
	if(length(firing)>1){
		charging <- colSums(puissance[firing,])		# plusieurs neurones "fire"
	}else{
		if(length(firing)==1){
			charging <- puissance[firing,]		# un seul neurone "fire"
		}else{
			charging <- charge*0	# pas de "fire"
		}
	}
	charging[which(unavailable>0)] <- 0
	
	charge <- charge+charging		# chargement des neurones
	print(firing)
	
	# le lien entre un neurone qui "fire" et le ou les neurone(s) ayant contribué à le charger, doit être renforcé
	
	for(i in 1:length(firing)){
		
		reinforcelinkfrom <- lastfiring[which(connectome[lastfiring,firing[i]]==1)]
		puissance[reinforcelinkfrom,firing[i]] <- puissance[reinforcelinkfrom,firing[i]]*1.1
		
		
		
	}
	
	
	lastfiring <- firing
	
	
	fire <- rep(1,N)
	fire[firing] <- 2
		
	
	plot(Y~X, col=fire, pch=I((fire-1)*18)+1)
	
	
	
}

	
	








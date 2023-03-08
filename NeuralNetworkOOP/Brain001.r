




nbNeurone_init <- 5

addNeurone <- function(brain){
	NEWnbNeurones <- brain$nbNeurones + 1
	
	brain$nbNeurones <- NEWnbNeurones
	
	NEWneurone <- list(	  id=NEWnbNeurones
							, charge=0 # accumulation de charge. tends vers zéro selon la fonction charge[t+1] = charge[t]+0.1. si > charge max, déchargement jusqu'en négatif (charge = -0.5) (réfractaire à toute nouvelle charge extérieure tant que négatif
							, connections=list()
							, nbConnections=0

						 )
	brain$neurones[[NEWnbNeurones]] <- NEWneurone
	
	return(brain)
}

addConnection <- function(neurone, to){
	NEWnbConnect <- neurone$nbConnections + 1
	neurone$nbConnections <- NEWnbConnect
	
	connection <- list(  to=to
					   , nStep=sample(10,1)
					   , curStep=-1
					  )
		neurone$connections[[NEWnbConnect]] <- connection
	return(neurone)
}

oneStepConnection <- function(connection){
	newStep <- connection$curStep + 1
	if(newStep = connection$nStep){
		connection$curStep <- -1
	}else{
		connection$curStep <- newStep
	}
	
	return(connection)
}




brain <- list(  nbNeurones = 0
				 , neurones=list()
				)

for(i in 1:nbNeurone_init){
	

	
	brain <- addNeurone(brain)
	neurone_i <- brain$neurones[[i]]
	cible <- sample(nbNeurone_init,1)
	neurone_i <- addConnection(neurone_i, cible)
	cible <- sample(nbNeurone_init,1)
	neurone_i <- addConnection(neurone_i, cible)
	cible <- sample(nbNeurone_init,1)
	neurone_i <- addConnection(neurone_i, cible)
	brain$neurones[[i]] <- neurone_i
	
}


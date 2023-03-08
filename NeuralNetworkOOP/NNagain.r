if (!require('dfoptim')) install.packages('dfoptim'); require('dfoptim')

# réseau de neurone nouvelle version

NN.new <- function(couches=c(2,3,1)){
	
	NN <- list()
	nbneurones <- sum(couches)
	neurones <- 1:nbneurones
	
	Ninput <- 1:couches[1]
	nbcouches <- length(couches)
	nbOut <- couches[nbcouches]
	Noutput <- I(nbneurones-nbOut):nbOut
	
	coefs <- rnorm(nbneurones*nbneurones)
	intercepts <- rnorm(nbneurones)
	values <- rep(0,nbneurones)
	NN$couches = couches
	NN$nbcouches = nbcouches
	NN$nbneurones = nbneurones
	NN$neurones = neurones
	NN$Ninput = Ninput
	NN$Noutput = Noutput
	NN$coefs = coefs
	NN$intercepts = intercepts
	NN$values = values
	
	return(NN)
}
NN.new()

NN.updValues <- function(NN, values){
	
	NN$values <- values
	
	return(NN)
	
}
NN.updValues(NN.new(), c(1,2,3,4,5,6))

NN.updIntercepts <- function(NN, intercepts){
	
	NN$intercepts <- intercepts
	
	return(NN)
	
}
NN.updIntercepts(NN.new(couches=c(2,3,1)), c(1,2,3,4,5,6))


NN.updCoefs <- function(NN, coefs){
	
	NN$coefs <- coefs
	
	return(NN)
	
}
NN.updCoefs(NN.new(couches=c(2,3,1))
	 , c(  0,0,1,1,1,0
		 , 0,0,1,1,1,0
		 , 0,0,0,0,0,1
		 , 0,0,0,0,0,1
		 , 0,0,0,0,0,1
		 , 0,0,0,0,0,0))


NN.oneStep <- function(NN){
	
	coefsM <- matrix(NN$coefs, NN$nbneurones, NN$nbneurones, byrow=TRUE)
	MSum <- NN$values%*%coefsM
	actIn <- MSum+NN$intercepts
	# activation = logistique (ou marche douce, ou sigmoide)
	NN$values <- 1/(1+exp(-actIn))
	
	return(NN)
	
}
NN.updValues(NN.new(), c(1,2,3,4,5,6))$values
NN.oneStep(NN.updValues(NN.new(couches=c(2,3,1)), c(1,2,3,4,5,6)))$values


NN.nStep <- function(NN, nbstep=1){
	
	coefsM <- matrix(NN$coefs, NN$nbneurones, NN$nbneurones, byrow=TRUE)
	# print(coefsM)
	
	for(i in 1:nbstep){
		MSum <- NN$values%*%coefsM
		# print(MSum)
		actIn <- MSum+NN$intercepts
		# print(actIn)
		# activation = logistique (ou marche douce, ou sigmoide)
		NN$values <- 1/(1+exp(-actIn))
		# print(NN$values)
		plot(y=NN$values, x=I(1:NN$nbneurones))
	}
	return(NN)
	
}

NN1 <- NN.updCoefs(NN.new(couches=c(2,3,1))
	 , coefs=c(  0,0,1,1,1,0
               , 0,0,1,1,1,0
			   , 0,0,0,0,0,1
			   , 0,0,0,0,0,1
			   , 0,0,0,0,0,1
			   , 0,0,0,0,0,0))
			   
NN1 <- NN.new(couches=c(10,10))
NN2 <- NN.updValues(NN1, c(1:NN1$nbneurones))
NN3 <- NN.nStep(NN2, 1)

NN3$coefs <- rnorm(NN3$nbneurones*NN3$nbneurones)
# print(NN1$values)
# NN3 <- NN.updValues(NN1, c(1,1,0,0,0,0))
print(NN3$values)
for(i in 1:1000){
	NN3 <- NN.nStep(NN3, 1)
	print(NN3$values)
}










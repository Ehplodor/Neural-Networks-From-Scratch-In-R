


### Trainer = algorithme d'entrainement génétique pour un réseau de neurone
# avec en entrée les poids d'un autre réseau quelconque, ainsi que les entrées et les sorties observées et attendues de ce réseau,
# qui produit en sortie les nouveaux poids de ce réseau à entrainer.
# on évalue le réseau entraineur sur sa capacité à produire des poids meilleurs que les précédents,
# c'est à dire des poids nouveaux qui permettent d'obtenir une sortie du réseau entrainé plus proche de l'attendu que la sortie observée précédente...


# Astuce pour récupérer tous les éléments nommés pareil
# sapply(RB, with, c(id,type,connectedfrom,fire,weight))
NbTraineeInput <- 2
NbTraineeOutput <- 1
TraineeCouches <- c(  NbTraineeInput
					, 3
					, NbTraineeOutput)


#TraineeCouches <- c(2,2,1)
NbTraineeCouches <- length(TraineeCouches)



NbTraineeInput <- TraineeCouches[1]
NbTraineeOutput <- TraineeCouches[NbTraineeCouches]

TraineeRB <- InitNN(CreateNN(couches=TraineeCouches))


# Pour ne garder que les poids d'un réseau RB :

TraineeRBPoids <- TraineeRB$Weights[which((TraineeRB$Weights!=0))]

NbTraineeRBPoids <- sum((TraineeRB$Weights!=0))


# 
NbTrainerInput <- NbTraineeRBPoids+NbTraineeInput+NbTraineeOutput*2

TrainerCouches <- c(  NbTrainerInput
					, NbTrainerInput*2
					, NbTrainerInput
					, NbTraineeRBPoids)

# 

set.seed(1)

# Créer la population de départ
PopInit <- function(Couches,N){
	I001 <- InitNN(CreateNN(Couches))
	myPop <- list()
	for(i in 1:N){
		myPop[[i]] <- InitNN2(I001)
	}
	return(myPop)
}


# calculer le segment représentatif des probabilités de sélection de chaque individu
SelectionProba <- function(popfit){
	return(stable_softmax(popfit))
}

stable_softmax <- function(X){
	Xm <- max(X)
	exps <- exp(X-Xm)
	return(exps/sum(exps))
}


# Créer la nouvelle génération par hybridation / "accouplement"
# # sélection de deux individus selon leur probabilité de sélection (calculée à partir de leur fitness)
# # hybrider les individus pour créer un nouvel indiv. et le faire muter (25% de probabilité de mutation pour chaque paramètre de l'individu)
# # recommencer jusqu'à avoir créé une nouvelle population complète

NewGen2 <- function(population,mutProb=0.25,INPUT,ATTENDU,batchSize=1){
	newPop <- list()


	N <- length(population)
	
	# évaluation de la fitness de la population par rapport à l'attendu de l'échantillon batch
	# plus la fitness augmente, plus l'individu est performant sur le batch
	popfit <- rep(0,N)
	for(j in 1:batchSize){
		popfit <- popfit + PopFitness(population,INPUT=INPUT[[j]],ATTENDU=ATTENDU[[j]])
	}
	popfit <- popfit/batchSize
	# hist(popfit)
	print(median(popfit))
	
	
	
	# classer les individus par fitness décroissante
	opfD <- order(popfit, decreasing = TRUE)
	popfitOD <- popfit[opfD]
	population <- population[opfD]
	# print(GlobalPopFit(popfit=popfit))
	
	# transformer la fitness en probabilité de sélection
	selproba <- stable_softmax(popfitOD)
	
	# génération de la population suivante
	for(i in 1:N){
		if( (i/N) <= 0.2 ){ # garder par défaut les 20% les meilleurs 
			newPop[[i]] <- population[[i]]
		}else{	# pour le reste, accoupler et muter jusqu'à avoir le nombre souhaité d'individus dans la population
			r1 <- runif(1,0,1)
			r2 <- runif(1,0,1)
			Indiv1 <- seqR(selproba,r1)
			Indiv2 <- seqR(selproba,r2)
			newPop[[i]] <- Muter(Accoupler(population[[Indiv1]], population[[Indiv2]]),mutProb)
		}
	}
	return(newPop)
}


# recherche séquentielle du ième "panier" dans lequel est "tombé" la valeur a
seqR <- function(vec,a){
	i_sum <- 0
	for(i in 1:length(vec)){
	
		i_sum <- i_sum + vec[i]
		
		if(a <= i_sum){
			break
		}
	}
	
	return(i)
}


### fonction d'accouplement
## à partir de 2 RB de même structure, en créer un 3ème de structure identique dont les poids sont un mix des poids des 2 premiers, avec quelques mutations aléatoires

# test1 <- InitNN(CreateNN(couches=c(2,1,1)),mseed=1)
# test2 <- InitNN(CreateNN(couches=c(2,1,1)),mseed=2)

Accoupler <- function(RB1, RB2){
	
	RBlen <- RB1$NbNeurons
	RB1Mlen <- RBlen**2
		
	# Pour ne garder que les poids d'un réseau RB1 :
	# posWei <- which((RB1$Weights!=0))

	# position des poids dans le réseau
	
	
	
	# Initialisation de RB3
	RB3 <- RB1
	
	# Pour chaque poids de RB3, lui affecter au hasard (pile ou face)soit le poids de RB1 soit celui de RB2.
	
	RB1SelWei <- RB1$Weights
	
	# sélection des poids de RB1 qui vont être reportés dans la descendance
	RB1SelWei[1:RB1Mlen] <- sample(c(0,1),RB1Mlen, replace=TRUE)
	
	# sélection des poids de RB2 qui vont être reportés dans la descendance (= ceux non sélectionnés de RB1)
	RB2SelWei <- abs(RB1SelWei-1)
	
	# mélange des poids
	RB3$Weights <- RB1$Weights*RB1SelWei + RB2$Weights*RB2SelWei
	
	return(RB3)
	
	
}

# Accoupler(test1,test2)$Weights


Muter <- function(RB1,mutationProba){	# mutationProba compris entre 0 et 1
	posWei <- which((RB1$Weights!=0))
	lenposwei <- length(posWei)
	RB1$Weights[posWei] <- RB1$Weights[posWei] + rnorm(lenposwei,0,0.1) * sample(c(0,1), lenposwei, replace=TRUE, prob=c(1-mutationProba, mutationProba))
	return(RB1)
}
# Muter(test1)





# Calculer la fitness de tous les individus d'une population
PopFitness <- function(Population,INPUT,ATTENDU){
	popfit <- c()
	input <- INPUT
	attendu <- ATTENDU
	for(i in 1:length(Population)){
		# print(paste(c("###",i),collapse=" "))
		# calcul de la fitness de l'individu i par rapport à l'exemple en entrée
		popfit[i] <- Perf(Population[[i]],INPUT=input,ATTENDU=attendu)
		# plus le résultat est proche de zéro, mieux c'est (c'est la somme des carrés des écarts à l'attendu)
	}
	popfit2 <- (1/popfit)
	# fitness d'un individu = zéro => somme des carrés des écarts très élevée => performance vraiment nulle
	# =1 => somme des carrés des écarts à l'attendu = 1 => c'est bien
	# > 1 => c'est très bien

	return(popfit2)
}

# Calculer la fitness d'un individu
# Fitness1 <- function(trainer,INPUT,ATTENDU){
	# input <- INPUT
	# attendu <- ATTENDU
	# ifit <- Perf(TRAINER=trainer,INPUT=input,ATTENDU=attendu)  # Retourne la somme des carrés des écarts. + petit = mieux
	# return(ifit)
# }


Perf <- function(TRAINER,INPUT,ATTENDU){
# (La méthodologie d'évaluation des performances d'un réseau entraineur pour un réseau entrainé est la suivante :)	
	
	# il faut effectuer une passe en avant sur le réseau avec les input et comparer les output à l'attendu
	
	OBSERVE <- EnAvant(TRAINER,INPUT)#[,1]
	
	d2 <- EvalResults(ATTENDU,OBSERVE)
	
	# retourner la somme des carrés des écarts. plus c'est petit, mieux c'est
	return(d2)
	
	
}



EvalResults <- function(ATTENDU,OBSERVE){
	
	# ATTENDU et OBSERVE doivent être des vecteurs de même longueur.
	# print(paste(c("attendu = ",ATTENDU, " ; observé = ", OBSERVE),collapse=""))
	# return(sum(abs(ATTENDU-OBSERVE)))
	return(sum((ATTENDU-OBSERVE)**2))
	
	
}


# calculer la fitness globale d'une population
GlobalPopFit <- function(popfit){
	return(median(popfit))
}


# calculer la fitness globale d'une population
GlobalPopFit2 <- function(popfit){
	return(max(popfit))
}



# calculer la fitness globale d'une population
GlobalPopFit3 <- function(popfit){
	return(mean(popfit))
}



### MNIST Evolution de population
# source("C:/Users/lauverjonr/Documents/Divers/Réseaux de neurones/load-mnist.r")


set.seed(1)
# FirstInd <- CreateNN(c(784,32,10))



# recommencer au début avec la nouvelle population
myPop <- PopInit(Couches=c(784,100,nblvl),N=20)

# trainedNN <- myPop[[1]]

# EnAvant(trainedNN,as.numeric(test[1,-785]))

# EnAvant(myPop[[1]],c(0,0))
# print(round(EnAvant(myPop[[1]],as.numeric(train[1,-785])),5))

nblvl <- 26

gpfA1 <- 0
gpfA2 <- 0

batchSize <- 5

NbGEN <- length(train[,1])-batchSize
# NbEx <- 5

k <- 1

#plot(x=0,y=0,xlim=c(0,NbGEN),ylim=c(0,1))

for(i in k:NbGEN){
	print(i)
	
	# attendu i : 
	Yatti <- as.numeric(as.character(train[i,785]))
	Yatt <- rep(0,nblvl)
	Yatt[as.numeric(as.character(train[i,785]))] <- 1
	
	batchInput <- list(as.numeric((train[i,-785])))
	batchYatt <- list(Yatt)
	if(batchSize>1){
		for(j in 2:batchSize){
			batchInput[[j]] <- as.numeric((train[i+j-1,-785]))
			
			Yatti <- as.numeric(as.character(train[i+j-1,785]))
			Yatt <- rep(0,nblvl)
			Yatt[as.numeric(as.character(train[i+j-1,785]))] <- 1
			batchYatt[[j]] <- Yatt
		}
	}
	
	myPop <- NewGen2(	 myPop
						,mutProb=0.1
						,INPUT=batchInput
						,ATTENDU=batchYatt
						, batchSize=batchSize
						)
	#
	# gpfA2 <- 1/GlobalPopFit3(popfit=(PopFitness( myPop
												# ,INPUT=as.numeric((train[i,-785]))
												# ,ATTENDU=Yatt
												# )))

	#segments(x0=i-1, y0=gpfA1, x1 = i, y1 = gpfA2,col=1)
	# gpfA1 <- gpfA2	
	
	# print(EnAvant(myPop[[1]],c(0,0)))
	
	
	#print(paste("attendu = ", I(0.05+as.numeric(as.character(train[i,785]))/10)," ; observé = ",round(EnAvant(myPop[[1]],as.numeric(as.character(train[i,-785]))),5)))


	
}
k <- i



batchSize <- 100
m <- 1
batchInput <- list(as.numeric((train[m,-785])))
batchYatt <- list(Yatt)
if(batchSize>1){
	for(j in 2:batchSize){
		batchInput[[j]] <- as.numeric((train[m+j-1,-785]))
		
		Yatti <- as.numeric(as.character(train[m+j-1,785]))
		Yatt <- rep(0,nblvl)
		Yatt[as.numeric(as.character(train[m+j-1,785]))] <- 1
		batchYatt[[j]] <- Yatt
	}
}
	

pp <- rep(0,length(myPop))
for(m in 1:batchSize){
ppm <- PopFitness(myPop,INPUT=batchInput[[m]]
						,ATTENDU=batchYatt[[m]])
print(round(1/ppm,2))
plot(1/ppm, main=m)
pp <- pp+ppm

}

plot(1/pp)

trainedNN <- myPop[[which(I(1/pp)==max(1/pp))]]




nbtest <- length(test[,1])
windows()
XtestT <- c()
# YtestT <- c()
finalrep <- c()
jmax <- 1000
for(k in 1:jmax){
	j <- ceiling(runif(1,0,1)*nbtest)
	# XtestT[j] <- I(0.05+as.numeric(as.character(test[j,785]))/10)
	XtestT[k] <- as.numeric(as.character(test[j,785]))
	# YtestT[j] <- EnAvant(trainedNN,as.numeric((test[j,-785])))
	# quelle est la précision de ce modèle ?
	reponse <- EnAvant(trainedNN,as.numeric(test[j,-785]))
	maxrep <- max(reponse)
	# finalrep[j] <- c(0:nblvl)[which(reponse==maxrep)]
	finalrep[k] <- c(1:nblvl)[which(reponse==maxrep)]
	
	print(k)
}

plot(I(finalrep++runif(jmax,-0.3,0.3))~I(XtestT+runif(jmax,-0.3,0.3)))
sum(finalrep == XtestT)





# un graphe de plus
# plot(stable_softmax(EnAvant(trainedNN,as.numeric(as.character(test[j,-785]))))~c(0:9))
# segments(x0=as.numeric(as.character(test[j,785])),x1=as.numeric(as.character(test[j,785])),y0=0,y1=1,col=2)




# newNN = réseau de neurone sous forme matricielle

DevelopperIds <- function(vec=NULL){
	if(is.null(vec)){
		print("vec is NULL")
		return()
	}
	if(!(typeof(vec)=="integer") & !(typeof(vec)=="double")){
		print("type of vec is not integer, and it is not double neither")
		return()
	}
	UIds <- c()
	for(i in vec){
		
		UIds <- c(UIds,c(1:i))
		
	}
	return(UIds)
	
}

CreateNN <- function(couches=c(2,3,2)){
	NbCouches <- length(couches)
	NbBias <- NbCouches-1
	NbNeurons <- sum(couches)+NbBias
	NbInput <- couches[1]
	NbOutput <- couches[NbCouches]
	NbHidden <- NbNeurons-NbInput-NbOutput-NbBias
	
	WhichCouche <- c(rep(c(1:NbCouches),couches),c(1:I(NbCouches-1)))
	TypeOfNeurons <- WhichCouche
	TypeOfNeurons[I(NbNeurons-NbBias+1):NbNeurons] <- "B"
	
	TypeOfNeurons[which(TypeOfNeurons==1)] <- "I"
	TypeOfNeurons[which(TypeOfNeurons==NbCouches)] <- "O"
	TypeOfNeurons[which(!((TypeOfNeurons == "I") | (TypeOfNeurons=="O") | (TypeOfNeurons=="B")))] <- "H"
	
	myNNDimnames <- paste(TypeOfNeurons,WhichCouche, c(DevelopperIds(couches),rep(1,NbBias)), sep="")
	myNNIds <- c(1:NbNeurons)
	
	# Weights / Poids de chaque neurone "ligne" vers chaque neurone "colonne"
	myNNWeights <- matrix(data = 0, nrow = NbNeurons, ncol = NbNeurons, byrow = FALSE)
	
	# Values / Valeur en sortie de chaque neurone. Chaque neurone Bias vaut toujours 1 (avant multiplication par le poids vers le neurone suivant)
	myNNOutValues <- c(rep(1,NbInput),rep(0,NbHidden),rep(0,NbOutput),rep(1,NbBias))
	
	myNNError <- rep(0,NbNeurons)
	
	# Valeur en entrée de chaque neurone (après multiplication mais avant somme et activation)
	myNNWeightedOutValues <- matrix(data = 0, nrow = NbNeurons, ncol = NbNeurons, byrow = FALSE)
	
	myNNSummedWeightedOutValues <- c(rep(0,NbInput),rep(0,NbHidden),rep(0,NbOutput),rep(0,NbBias))
	
	# fonction d'activation : 0 = pas de fonction d'activation ; 1 = fonction act() ; -1 = fonction qui retourne toujours 1 (pour les neurones de bias) ; 2 = softmax
	myNNSquashingFunc <- c(rep(0,NbInput),rep(1,NbHidden),rep(2,NbOutput),rep(-1,NbBias))
	
	return(list(Weights=myNNWeights, OutValues=myNNOutValues, Error=myNNError, Weighted=myNNWeightedOutValues, Summed=myNNSummedWeightedOutValues,
				SquashingFunc=myNNSquashingFunc, couches=couches, NbCouches=NbCouches, NbNeurons=NbNeurons, NbInput=NbInput, NbHidden=NbHidden, NbOutput=NbOutput, NbBias=NbBias,
				WhichCouche=WhichCouche, TypeOfNeurons=TypeOfNeurons, NeuronIds=myNNIds, gradient=myNNWeights))
}


softmax <- function(X){
	exps <- exp(X)
	return(exps/sum(exps))
}

stable_softmax <- function(X){
	Xm <- max(X)
	exps <- exp(X-Xm)
	return(exps/sum(exps))
}

deriv_softmax <- function(P){
	Pl <- length(P)
	for(i in 1:Pl){
		for(j in 1:Pl){
		
		}
		
	}
}


# initialiser les poids d'un réseau de neurone à partir des informations de connection du réseau
InitNN <- function(myNN){
	
	for(i in 1:I(myNN$NbCouches-1)){
		rows <- which(myNN$WhichCouche==i)
		cols <- which(myNN$WhichCouche==I(i+1) & myNN$TypeOfNeurons!="B")
		
		myNN$Weights[rows,cols] <- rnorm(I(length(rows)*length(cols)),0,1)
	}
	
	
	
	return(myNN)
	
	
}



# à partir d'un réseau dors et déjà initialisé avec InitNN
InitNN2 <- function(myNN){
	
	# Weights <- myNN$Weights
	
	posWei <- which(!myNN$Weights==0)
	
	myNN$Weights[posWei] <- rnorm(length(posWei),0,1)
	
	# for(rowi in myNN$NeuronIds){
		
		# for(coli in myNN$NeuronIds){
		
			# if((myNN$WhichCouche[coli] == I(myNN$WhichCouche[rowi]+1)) & !(myNN$TypeOfNeurons[coli]=="B")){
				##### (re-)init poids !!! feed-forward uniquement !!!
				# Weights[rowi,coli] <- rnorm(1,0,1)
			# }
		
		# }
		
	# }
	# myNN$Weights <- Weights
	return(myNN)
	
	
}


act <- function(a){return(1/(1+exp(-a)))}
dact <- function(a){return(act(a)*(1-act(a)))}

invact <- function(a){return(log(-a/(a-1)))}


OneStep <- function(NN,m){
	# NN = réseau de neurones modélisé plus haut
	# m = numéro de la couche actuellement en cours de traitement (commence à 1 jusqu'à NbCouches-1)
	
	Nidm <- which(NN$WhichCouche==m)
	
		
	# appliquer les poids 'colonne' aux sorties des neurones 'ligne' de la couche m
	NN$Weighted[Nidm,] <- NN$OutValues[Nidm] * NN$Weights[Nidm,]
	
	# sommer les valeurs pondérées, sur chaque colonne. ce qui donne la valeurs en entrée de chaque neurone de la couche m+1
	NN$Summed <- colSums(NN$Weighted)
	
	# les neuronnes de la couche N suivante recevant le signal depuis la couche m sont ceux dont la valeur NN$Summed est différente de zéro
	#NidN <- which(!(NN$Summed==0))
	NidN <- which(NN$WhichCouche == I(m+1) & !NN$TypeOfNeurons == "B")
		
	# activation : on applique la fonction d'activation au signal en entrée des neurones de la couche suivante
	if((m <= I(NN$NbCouches-1))){
		NN$OutValues[NidN] <- act(NN$Summed[NidN])
	}else{
		# NN$OutValues[NidN] <- stable_softmax(NN$Summed[NidN])
		NN$OutValues[NidN] <- act(NN$Summed[NidN])
		print(NN$OutValues[NidN])
	}	
	return(NN)
	
}




# Passe complète :
EnAvant <- function(NN,JDD_INPUT){

	NN$OutValues[which(NN$TypeOfNeurons=="I")] <- JDD_INPUT
	NN$OutValues[which(NN$TypeOfNeurons=="H")] <- 0
	NN$OutValues[which(NN$TypeOfNeurons=="O")] <- 0
	NN$OutValues[which(NN$TypeOfNeurons=="B")] <- 1
	
	for(i in 1:I(NN$NbCouches-1)){
		NN <- OneStep(NN,i)
	}
	
	#return(NN)
	return(I(NN$OutValues[which(NN$TypeOfNeurons=="O")]))
}

# test1 <- EnAvant(test,c(1,2))

# calcul des erreurs :

Erreur <- function(OBSERVE,ATTENDU){
	
	return(I(OBSERVE-ATTENDU))
	# erreur négative signifie que l'observé était < à l'attendu
	# erreur positive signifie que l'observé était > à l'attendu
	
}

# test_res <- EnAvant(test,c(1,2))
# test_err <- Erreur(test_res,c(0.5,0.5))
# InitNN(CreateNN(c(5,3,2)))

CompleteRG <- function(NN,JDD_INPUT,ATTENDU,H=0.1, batchSize=1){
	
	# Out <- rep(0,length(NN$OutValues))
	# Att <- rep(0,length(ATTENDU))
	
	for(m in 1:batchSize){
		
		Att <- ATTENDU[[m]]
		
		NN$OutValues[which(NN$TypeOfNeurons=="I")] <- JDD_INPUT[[m]]
		NN$OutValues[which(NN$TypeOfNeurons=="H")] <- 0
		NN$OutValues[which(NN$TypeOfNeurons=="O")] <- 0
		NN$OutValues[which(NN$TypeOfNeurons=="B")] <- 1
		
		# print("#01")
		### En AVANT
		for(i in 1:I(NN$NbCouches-1)){
			NN <- OneStep(NN,i)
		}
		
	
		Out <- NN$OutValues
	}
	
	# print("#02")
	
	# Calcul Erreur sur la sortie
	NidO <- which(NN$TypeOfNeurons=="O")
	# print("#03")

	for(j in 1:length(NidO)){
		NidOj <- NidO[j]
		NNOj <- Out[NidOj]
		NN$Error[NidOj] <- I(Att[j]-NNOj) *  dact(NN$Summed[NidOj]) # NNOj*(1-NNOj)
	}
	# print("#04")

	
	### CALCUL DU GRADIENT (BACKPROP) en partant de l'avant dernière couche en en remontant jusqu'à la seconde couche
	# for (i in I(NN$NbCouches-1):2){
	for (i in I(NN$NbCouches-1):1){
		# Pour chaque couche cachée en partant de celle juste avant la sortie et en remontant vers la 1ère couche cachée
		
		# déterminer quels neurones "H" appartiennent à cette couche
		# NidH <- which(NN$TypeOfNeurons=="H" & NN$WhichCouche==i)
		NidH <- which(NN$WhichCouche==i)
	# print("#05")
		# pour chaque neurone caché de la couche en cours de traitement, déterminer l'erreur sur ce neurone
		for(j in 1:length(NidH)){	
			
			NidHj <- NidH[j]
			
	# print("#06")
			# rechercher les neurones en aval du neurone "NidH[j]"
			#NidHAval <- which(!(NN$Weights[,NidHj] == 0))	############################# OK ?
			 NidHAval <- which((NN$Weights[NidHj,  ] != 0))	############################# KO ?
			
			# déterminer l'erreur sur le neurone j =
			# somme des erreurs des neurones k en aval de j, multipliées chacune par le poids de j vers k.
			# Puis multiplier cette somme des erreurs par la dérivée de act de la sortie du neurone j.
			NN$Error[NidHj] <- sum(NN$Error[NidHAval] * NN$Weights[NidHj,NidHAval]) * dact(Out[NidHj])  
			
		}
		
	# print("#07")
		NidnotI <- which((NN$TypeOfNeurons!="I"))
	# print("#08")
		for(j in 1:length(NidnotI)){
			NidnotIj <- NidnotI[j]
			
			NidAff <- which((NN$Weights[,NidnotIj]!=0)) # quels sont les neurones en amont de j (= afférents de j)?
			
			NN$Weights[NidAff,NidnotIj] <- NN$Weights[NidAff,NidnotIj] + H * NN$Error[NidnotIj] * Out[NidAff]
			
		}
	# print("#09")
	}
	
	OBSERVE <- Out[which(NN$TypeOfNeurons=="O")]
	print("#10")
	for(i in 1:NN$NbOutput){
		print(paste(c("OBSERVE = ",round(OBSERVE[i],5)," VS ATTENDU = ",round(Att[i],5)), collapse=";"))
	}
	# print("#11")
	# print(NN)
	return(list(NN=NN, OBS=OBSERVE, ATT=Att))
	
}




########## test 2
test <- CreateNN(couches=c(2,2,1))
test <- InitNN(test)

# saveW1 <- test$Weights
# CompleteRG(NN=test,JDD_INPUT=c(0,0),ATTENDU=c(0.1),H=0.5)$NN$Weights

EnAvant(test,c(0,0))
EnAvant(test,c(0,1))
EnAvant(test,c(1,0))
EnAvant(test,c(1,1))

for(i in 1:1000){
	test <- CompleteRG(NN=test,JDD_INPUT=c(0,0),ATTENDU=c(0.1),H=0.1)$NN
	test <- CompleteRG(NN=test,JDD_INPUT=c(0,1),ATTENDU=c(0.3),H=0.1)$NN
	test <- CompleteRG(NN=test,JDD_INPUT=c(1,0),ATTENDU=c(0.5),H=0.1)$NN
	test <- CompleteRG(NN=test,JDD_INPUT=c(1,1),ATTENDU=c(0.7),H=0.1)$NN
	# print(paste("############", round(c(EnAvant(test,c(0,0)),EnAvant(test,c(0,1)),EnAvant(test,c(1,0)),EnAvant(test,c(1,1))),5)))

}

EnAvant(test,c(0,0))
EnAvant(test,c(0,1))
EnAvant(test,c(1,0))
EnAvant(test,c(1,1))

# saveW2 <- test$Weights


test




#### MNIST avec back-prop

set.seed(1)
mnistBP <- CreateNN(c(784,32,1))

mnistBP <- InitNN(mnistBP)

NbGEN <- 60000

mlrngrt <- 0.1
for(m in 2:2){

# mlrngrt <- mlrngrt/m

windows()

plot(x=0, y=0, xlim=c(0,NbGEN), ylim=c(-1,1), main=m)

k <- 1

	for(i in k:NbGEN){
		
		resultat <- CompleteRG(NN=mnistBP,JDD_INPUT=as.numeric((train[i,-785])),ATTENDU=I(0.05+as.numeric(as.character(train[i,785]))/10),H=mlrngrt)
		mnistBP <- resultat$NN
		points(x=i, y=I(resultat$OBS-resultat$ATT), col=I(10*(resultat$ATT-0.05)))
		print(i)
	}

k <- i
}

# MNIST_001 <- mnistBP


XtestT <- c()
YtestT <- c()
jmax <- 100
for(j in 1:jmax){
	XtestT[j] <- I(0.05+as.numeric(as.character(test[j,785]))/10)
	YtestT[j] <- EnAvant(mnistBP,as.numeric((test[j,-785])))
	print(j)

	
}

	Xtest <- 10*(XtestT-0.05)
	Ytest <- 10*(YtestT-0.05)

plot(Ytest~Xtest,xlim=c(0,9), ylim=c(0,9))
plot(Ytest~as.factor(as.character(Xtest)), ylim=c(0,9))

summary(lm(Ytest~Xtest))

# quelle est la précision de ce modèle ?




# plot(Xtest~Ytest,xlim=c(0,1), ylim=c(0,1))

# summary(lm(Xtest~Ytest))

# tableau des performances de classification

# classperf <- matrix(0,10,10)
# for(j in 1:jmax){
	
	
# }

# Précision = nombre correctement classifié / nombre total



dYX <- Ytest-Xtest








#### MNIST avec back-prop : approche multi-classe en sortie (10 classes) (sans softmax)
nblvl <- 26

trainedNNsave <- trainedNN

set.seed(1)
mnistBP <- CreateNN(c(784,100,nblvl))

mnistBP <- InitNN(mnistBP)

# > str(mnistBP)
# List of 16
 # $ Weights      : num [1:819, 1:819] 0 0 0 0 0 0 0 0 0 0 ...
 # $ OutValues    : num [1:819] 1 1 1 1 1 1 1 1 1 1 ...
 # $ Error        : num [1:819] 0 0 0 0 0 0 0 0 0 0 ...
 # $ Weighted     : num [1:819, 1:819] 0 0 0 0 0 0 0 0 0 0 ...
 # $ Summed       : num [1:819] 0 0 0 0 0 0 0 0 0 0 ...
 # $ SquashingFunc: num [1:819] 0 0 0 0 0 0 0 0 0 0 ...
 # $ couches      : num [1:3] 784 32 1
 # $ NbCouches    : int 3
 # $ NbNeurons    : num 819
 # $ NbInput      : num 784
 # $ NbHidden     : num 32
 # $ NbOutput     : num 1
 # $ NbBias       : num 2
 # $ WhichCouche  : int [1:819] 1 1 1 1 1 1 1 1 1 1 ...
 # $ TypeOfNeurons: chr [1:819] "I" "I" "I" "I" ...
 # $ NeuronIds    : int [1:819] 1 2 3 4 5 6 7 8 9 10 ...


NbGEN <- 60000
batchSize <- 1


windows()

plot(x=0, y=0, xlim=c(0,NbGEN), ylim=c(0,2), main="train01")

legend(x=1, y=0.4, legend=c(0:nblvl), fill=rainbow(nblvl))

for(m in 1:1){
	
	k <- 1
	
	for(i in seq(k,NbGEN,batchSize)){
		
		Yatti <- as.numeric(as.character(train[i,785]))
		Yatt <- rep(0,nblvl)
		# Yatt[as.numeric(as.character(train[i,785]))+1] <- 1		# numéros de 0 à 9
		Yatt[as.numeric(as.character(train[i,785]))] <- 1			# lettres de 1 à 26


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
	

		resultat <- CompleteRG(
						  NN=mnistBP#,JDD_INPUT=as.numeric((train[i,-785])),ATTENDU=Yatt,H=0.1
						, JDD_INPUT=batchInput
						, ATTENDU=batchYatt
						, H=0.1
						, batchSize=batchSize)
		mnistBP <- resultat[[1]]
		# points(x=i, y=I(sum(((stable_softmax(resultat$OBS)-resultat$ATT))**2)), col=I(as.numeric(as.character(train[i,785]))), cex=0.5)
		points(x=i, y=I(sum(((resultat$OBS-resultat$ATT))**2)), col=I(as.numeric(as.character(train[i,785]))), cex=0.5)
		# print(i)
		
		
		print(paste("############",m,":",i))
	}

	k <- i

}



trainedNN <- mnistBP
# plot(trainedNN$Error~I(trainedNN$WhichCouche+runif(length(trainedNN$WhichCouche),-0.1,0.1)))

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




	Xtest <- 10*(XtestT-0.05)
	Ytest <- 10*(YtestT-0.05)

plot(Ytest~Xtest,xlim=c(0,9), ylim=c(0,9))
plot(Ytest~as.factor(as.character(Xtest)), ylim=c(0,9))

summary(lm(Ytest~Xtest))





# un graphe de plus
plot(stable_softmax(EnAvant(trainedNN,as.numeric(as.character(test[j,-785]))))~c(0:9))
segments(x0=as.numeric(as.character(test[j,785])),x1=as.numeric(as.character(test[j,785])),y0=0,y1=1,col=2)




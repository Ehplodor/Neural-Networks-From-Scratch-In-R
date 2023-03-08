# Fonctions communes




softmax <- function(X){
	exps <- exp(X)
	return(exps/sum(exps))
}

stable_softmax <- function(X){
	Xm <- max(X)
	exps <- exp(X-Xm)
	return(exps/sum(exps))
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
	}else{	# si couche de sortie, quelle activation ?
		NN$OutValues[NidN] <- stable_softmax(NN$Summed)[NidN]
		# NN$OutValues[NidN] <- act(NN$Summed[NidN])
		
		# print(NN$OutValues[NidN])
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

EnAvantListe <- function(NN, JDD_INPUT_LIST){
	
	#JDD_INPUT_LIST doit comporter pour chaque occurrence un vecteur c(x,y)
	
	Ylist <- list()
	
	imax <- length(JDD_INPUT_LIST)
	for(i in 1:imax){
		X <- JDD_INPUT_LIST[[i]]
		Ylist[[i]] <- EnAvant(NN=NN, JDD_INPUT=X)
		
	}
	return(Ylist)

}





EvalResults <- function(ATTENDU,OBSERVE){
	
	# ATTENDU et OBSERVE doivent être des vecteurs de même longueur.
	# print(paste(c("attendu = ",ATTENDU, " ; observé = ", OBSERVE),collapse=""))
	# return(sum(abs(ATTENDU-OBSERVE)))
	return(sum((ATTENDU-OBSERVE)**2))
	
	
}



# calcul des erreurs :

Erreur1 <- function(OBSERVE,ATTENDU){
	
	return(I(OBSERVE-ATTENDU))
	# erreur négative signifie que l'observé était < à l'attendu
	# erreur positive signifie que l'observé était > à l'attendu
	
}

Erreur2 <- function(OBSERVE,ATTENDU){
	
	return(I((OBSERVE-ATTENDU)**2))
	# erreur négative signifie que l'observé était < à l'attendu
	# erreur positive signifie que l'observé était > à l'attendu
	
}

Erreur3 <- function(OBSERVE,ATTENDU){
	
	return(I((OBSERVE-ATTENDU)**3))
	# erreur négative signifie que l'observé était < à l'attendu
	# erreur positive signifie que l'observé était > à l'attendu
	
}

Erreur2s <- function(OBSERVE,ATTENDU){
	delta <- OBSERVE-ATTENDU
	return(I(sign(delta)*(delta)**2))
	# erreur négative signifie que l'observé était < à l'attendu
	# erreur positive signifie que l'observé était > à l'attendu
	
}



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
	myNNGrad <- myNNWeights

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
				WhichCouche=WhichCouche, TypeOfNeurons=TypeOfNeurons, NeuronIds=myNNIds, Grad=myNNGrad))
}


deriv_softmax <- function(P){
	Pl <- length(P)
	for(i in 1:Pl){
		for(j in 1:Pl){
		
		}
		
	}
}

# test_res <- EnAvant(test,c(1,2))
# test_err <- Erreur(test_res,c(0.5,0.5))
# InitNN(CreateNN(c(5,3,2)))




CompleteRG <- function(NN,JDD_INPUT,ATTENDU,H=0.1, batchSize=1){
	
	NeurIdList <- c(1:NN$NbNeurons)
	
	NidnotI <- NeurIdList[which((NN$TypeOfNeurons!="I" & NN$TypeOfNeurons!="B"))]
	
	NidnotI_reverse <- NidnotI[length(NidnotI):1]

	# NidnotI <- which((NN$TypeOfNeurons!="I"))
	NidO <- NeurIdList[which(NN$TypeOfNeurons=="O")]
	
	# liste des neurones afférents
	NidAffList <- list()
	for(j in 1:NN$NbNeurons){
		# NidnotIj <- NidnotI[j]
		# print(NidnotIj)
		# print(which((NN$Weights[,NidnotIj]!=0)))
		NeuronesEnAmont <- NeurIdList[which((NN$Weights[,j]!=0))]
		if(length(NeuronesEnAmont)>0){
			NidAffList[[j]] <- NeuronesEnAmont # quels sont les neurones en amont de j (= afférents de j)?
		}
	}
	# print("NidAffList =")
	# print(NidAffList)
	
	sumOut <- rep(0,length(NN$OutValues))
	sumAtt <- rep(0,length(ATTENDU))
	
	for(m in 1:batchSize){
		
		# sumAtt <- sumAtt + ATTENDU[[m]]
		sumAtt <- ATTENDU[[m]]
		
		NN$OutValues[which(NN$TypeOfNeurons=="I")] <- JDD_INPUT[[m]]
		NN$OutValues[which(NN$TypeOfNeurons=="H")] <- 0
		NN$OutValues[which(NN$TypeOfNeurons=="O")] <- 0
		NN$OutValues[which(NN$TypeOfNeurons=="B")] <- 1
		
		# print("#01")
		### En AVANT
		for(i in 1:I(NN$NbCouches-1)){
			NN <- OneStep(NN,i)
		}
		
		
		# sumOut <- sumOut+NN$OutValues
		sumOut <- NN$OutValues
	
		# }
	
		# sumAtt <- sumAtt/batchSize
		# sumOut <- sumOut/batchSize
		
		
		# print("#02")
		
		# Calcul Erreur sur la sortie
		# print("#03")

		for(j in 1:length(NidO)){
			NidOj <- NidO[j]
			NNOj <- sumOut[NidOj]

			
			# NN$Error[NidOj] <- Erreur1(sumAtt[j],NNOj) *  dact(NNOj)
			NN$Error[NidOj] <- Erreur1(sumAtt[j],NNOj) *  NNOj*(1-NNOj)
		}
		# print("#04")
		
		# sumOut <- NN$OutValues
		
		### à ce stade, on a exécuté une passe en avant du réseau de neurones
		# puis on a calculé l'erreur en sortie.
		
		#### il reste à rebrousser chemin pour calculer l'erreur, puis le gradient, et mettre à jour les poids
		
		# # NN$Weights[NidAff,NidnotIj] <- NN$Weights[NidAff,NidnotIj] + H * NN$Error[NidnotIj] * sumOut[NidAff]
		
		# déterminer l'erreur sur le neurone j de la couche i = delta entre la sortie observée du neurone et la sortie attendue
		# la sortie attendue étant elle-même calculée à partir 
		# somme des erreurs des neurones k en aval de j, multipliées chacune par le poids de j vers k.
		# Puis multiplier cette somme des erreurs par la dérivée de act de la sortie du neurone j.
		
		
		
		# puis à mettre à jour toutes les valeurs des paramètres (des poids)
		
		# dans quel ordre doit-on traiter la liste des neurones ?
		
		ordre_de_traitement <- c()
		for (i in NN$NbCouches:1){
			NidHi <- NeurIdList[which(NN$WhichCouche==i)]
			
			# pour chaque neurone  de la couche en cours de traitement, déterminer l'erreur sur ce neurone
			for(j in 1:length(NidHi)){
				
				ordre_de_traitement <- NidHi[j]
				
			}
		}
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		### CALCUL DU GRADIENT (BACKPROP) en partant de l'avant dernière couche en en remontant jusqu'à la première couche
		for (i in I(NN$NbCouches-1):2){
		# for (i in I(NN$NbCouches-1):1){
			# Pour chaque couche cachée en partant de celle juste avant la sortie et en remontant vers la 1ère couche cachée
			
			# déterminer quels neurones "H" appartiennent à cette couche
			# NidH <- which(NN$TypeOfNeurons=="H" & NN$WhichCouche==i)
			NidHi <- NeurIdList[which(NN$WhichCouche==i)]
			
			# pour chaque neurone  de la couche en cours de traitement, déterminer l'erreur sur ce neurone
			for(j in 1:length(NidHi)){	
				
				NidHij <- NidHi[j]
				
				
				# rechercher les neurones en aval du neurone "NidHi[j]"
				NidHAval <- which((NN$Weights[NidHij,  ] != 0))	
				
				# déterminer l'erreur sur le neurone j de la couche i =
				# somme des erreurs des neurones k en aval de j, multipliées chacune par le poids de j vers k.
				# Puis multiplier cette somme des erreurs par la dérivée de act de la sortie du neurone j.
				
				# si avant-dernière couche :
				# if (i == NN$NbCouches-1){
				
				# si fonction d'activation softmax :
				# NN$Error[NidHij] <- sum(NN$Error[NidHAval] * NN$Weights[NidHij,NidHAval]) *  sumOut
				
				
				# si fonction d'activation sigmoide :
				# NN$Error[NidHij] <- sum(NN$Error[NidHAval] * NN$Weights[NidHij,NidHAval]) * sumOut[NidHij]*(1-sumOut[NidHij]) 
				
				# }else{
					# fonction d'activation sigmoide :
				NN$Error[NidHij] <- sum(NN$Error[NidHAval] * NN$Weights[NidHij,NidHAval]) * sumOut[NidHij]*(1-sumOut[NidHij]) 
				
				# }
				
				# NN$Error[NidHij] <- sum(NN$Error[NidHAval] * NN$Weights[NidHij,NidHAval]) * dact(sumOut[NidHij])
				
				
			}

			
		}
		# if(type=="H"){
			
			# for(j in 1:length(RB)){
			
				# tmp <- sapply(RB, with, c(connectedfrom))[[j]]
				
				# if(i %in% tmp){	# si i est afférent à j <=> si i est en amont de j et connecté à j
					
					# ii = emplacement de i dans la liste des neurones connectés en amont de j
					# ii <- which(tmp==i)
					
					# somme des erreurs des neurones j en aval de i, multipliées chacune par le poids de i vers j
					# Error[i] <- Error[i] + Error[j]*sapply(RB, with, c(weight))[[j]][ii]
					
				# }
				
			# }
			# Error[i] <- RB[[i]][["fire"]]*(1-RB[[i]][["fire"]])*Error[i]
		# }
		
		# print(paste(c("### NidnotI_reverse = ", NidnotI_reverse), sep=", ", collapse=TRUE))
		# calcul du gradient pour chaque paramètre (poids)
		# print(NidAffList)
		for(j in 1:length(NidnotI_reverse)){
			NidnotIj <- NidnotI_reverse[j]
			# print("#1")
			# print(NidnotIj)
			NidAff <- NidAffList[[NidnotIj]]
			# MaJ du gradient des poids entre chaque neurone afférent au Jème neurone et le Jème neurone (ni bias ni input)
			# NN$Grad[NidAffList[[j]],NidnotIj] <- NN$Grad[NidAffList[[j]],NidnotIj] + NN$Error[NidnotIj] * sumOut[NidAffList[[j]]]
			# NN$Grad[NidAff,NidnotIj] <- NN$Grad[NidAff,NidnotIj] + NN$Error[NidnotIj] * sumOut[NidAff]
			# NN$Grad[NidAff,NidnotIj] <- NN$Grad[NidAff,NidnotIj] + NN$Error[NidnotIj] * sumOut[NidAff]
			# print(NN$Grad)
			# print(NN$Grad[NidAff,NidnotIj])
			# print(NN$Error[NidnotIj])
			# print(sumOut[NidAff])
			NN$Grad[NidAff,NidnotIj] <- NN$Error[NidnotIj] * sumOut[NidAff]
			# NN$Weights[NidAff,NidnotIj] <- NN$Weights[NidAff,NidnotIj] + H * NN$Error[NidnotIj] * sumOut[NidAff]
			
		}
	
	}
	
	# print(NidnotI)

	for(j in 1:length(NidnotI)){
		NidnotIj <- NidnotI[j]
			# print("#2")
			# print(NidnotIj)
		NidAff <- NidAffList[[NidnotIj]]		#!!!
		# NidAff <- which((NN$Weights[,NidnotIj]!=0)) # quels sont les neurones en amont de j (= afférents de j)?
		
		# NN$Grad[NidAff,NidnotIj] <- NN$Grad[NidAff,NidnotIj] + NN$Error[NidnotIj] * sumOut[NidAff]
		# print("####")
		
		# print(NN$Weights)
		# print(NN$Grad)
		# print(NidnotIj)
		# print(NidAff)
		# print(NidnotIj)
		# print(NN$Weights[NidAff,NidnotIj])
		# print(H)
		# print(NN$Weights[NidAff,NidnotIj])
		# print(NN$Grad[NidAff,NidnotIj])
		# NN$Weights[NidAffList[[j]],NidnotIj] <- NN$Weights[NidAffList[[j]],NidnotIj] + H * NN$Grad[NidAffList[[j]],NidnotIj]
		NN$Weights[NidAff,NidnotIj] <- NN$Weights[NidAff,NidnotIj] + H * NN$Grad[NidAff,NidnotIj]	#!!!
		
	}
	
	OBSERVE <- sumOut[NidO]
	# print("#10")
	# for(i in 1:NN$NbOutput){
		# print(paste(c("OBSERVE = ",round(OBSERVE[i],5)," VS ATTENDU = ",round(sumAtt[i],5)), collapse=";"))
	# }
	# print("#11")
	# print(NN)
		# print(NidAffList)
	return(list(NN=NN, OBS=OBSERVE, ATT=sumAtt))
	
}


############### SUPERNN

## Objectif : coder un assemblage de réseaux de neurones capables d'être appelés en boucle et d'être entrainés ensemble, ou unitairement.

# l'objectif ultime est d'obtenir une représentation interne "centrale" d'un "objet" et de pouvoir décoder cette représentation interne en plusieurs représentations externes, et de pouvoir encoder une représentation externe en sa représentation interne.

# Dans ma tête, un tel "réseau de réseaux" devrait pouvoir apprendre à encoder / décoder à partir d'exemples indépendants les ins des autres, sans aucun label.
# De plus, on peut imaginer que ce réseau "imaginerait" les informations manquantes d'une représentation E1 pour construire la représentation interne I1, et générer la représentation E2 malgré les informations partielles de E1.

## exemple 1 : apprendre à traduire une langue en une autre, sans aucun parallèle, à partir de deux livres indépendants, l'un en la langue E1, l'autre en E2.

## exemple 2 :
### à partir d'un plan E1, générer un nuage de points en 3D E2.
### à partir de la représentation 3D, générer le plan correspondant.
### à partir de la représentation 3D et de la position d'une caméra dans l'espace, générer une image d'un autre angle de vue.
### etc...



# l'idée globale est qu'à une représentation E1 correspond une représentation E2 et une seule, et vice-versa.


# Chaque neurone i (Ni) est une liste constituée des paramètres suivants :
# - vecteur Vi de n valeurs d'entrées Ej (j = 1 à n, j représentant le neurone Nj en amont de Ni) réels
# - une fonction d'aggrégation Fagg(Vi) -> S1i (S1 réel)
# - une fonction d'activation Fact(S1) -> S2 (S2 réel)



# COnstruire les diverses représentations :

## représentation interne I1 : 
### nombre de propriétés de I1
# I1.nbProp <- 2
# I1 <- rep(0,I1.nbProp)

# représentation externe E1 : 
## nombre de propriétés de E1
# E1.nbProp <- 10
# E1 <- rep(0,E1.nbProp)

# représentation externe E2 : 
## nombre de propriétés de E2
# E2.nbProp <- 10
# E2 <- rep(0,E2.nbProp)


# Construire et initialiser les réseaux A, B, C, D tels qu
# A(E1) = I1
# B(I1) = E2
# C(E2) = I1
# D(I1) = E1

# A(E1) = I1

# B(I1) = E2

# C(E2) = I1

# D(I1) = E1



# Construire ABCD et CDAB tels que :
# ABCD(E1) = E1
# CDAB(E2) = E2

# avec les couches des réseaux correspondants qui s'enchainent
# initialiser ces super-réseaux avec les poids des sous-réseaux


# Entrainer les super-réseaux à reproduire leur propre entrée en sortie, l'un après l'autre.
# à chaque passage et update d'un super-réseau, mettre à jour les poids des sous-réseaux et de l'autre super-réseau



A <- InitNN(CreateNN(c(2,3,4)))
B <- InitNN(CreateNN(c(4,5,6)))
C <- InitNN(CreateNN(c(6,5,4)))
D <- InitNN(CreateNN(c(4,3,2)))

ABCD <- InitNN(CreateNN(c(2,3,4,5,6,5,4,3,2)))
CDAB <- InitNN(CreateNN(c(6,5,4,3,2,3,4,5,6)))



CreateSuperNN <- function(SubNNList=list(A=I(InitNN(CreateNN(c(1,2,1)))),B=I(InitNN(CreateNN(c(1,2,1)))))){
	
	## ATTENTION les sous-réseaux doivent bien s'enchainer les uns les autres !!!
	
	SubNNListLen <- length(SubNNList)
	SubNames <- names(SubNNList)
	
	couches <- c()
	for(i in 1:SubNNListLen){
	
		SubNNi <- SubNNList[[i]]
		
		if(i != SubNNListLen){
			couches <- c(couches,SubNNi$couches[-length(SubNNi$couches)])
		}else{
			couches <- c(couches,SubNNi$couches)
		}
		
	}
	
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
	myNNGrad <- myNNWeights

	# Values / Valeur en sortie de chaque neurone. Chaque neurone Bias vaut toujours 1 (avant multiplication par le poids vers le neurone suivant)
	myNNOutValues <- c(rep(1,NbInput),rep(0,NbHidden),rep(0,NbOutput),rep(1,NbBias))
	
	myNNError <- rep(0,NbNeurons)
	
	# Valeur en entrée de chaque neurone (après multiplication mais avant somme et activation)
	myNNWeightedOutValues <- matrix(data = 0, nrow = NbNeurons, ncol = NbNeurons, byrow = FALSE)
	
	myNNSummedWeightedOutValues <- c(rep(0,NbInput),rep(0,NbHidden),rep(0,NbOutput),rep(0,NbBias))
	
	# fonction d'activation : 0 = pas de fonction d'activation ; 1 = fonction act() ; -1 = fonction qui retourne toujours 1 (pour les neurones de bias) ; 2 = softmax
	myNNSquashingFunc <- c(rep(0,NbInput),rep(1,NbHidden),rep(2,NbOutput),rep(-1,NbBias))
	

	myNeuronsBySubNN <- list()
	# print(names(myNeuronsBySubNN))
	
	aa <- 0	# clé de repositionnement dans les neurones non biais
	bb <- NbNeurons-NbBias	# clé de repositionnement dans les neurones de biais
	
	for(i in 1:SubNNListLen){
		SubNNi <- SubNNList[[i]]
		
		myNeuronsBySubNNA <- c(1:I(SubNNi$NbInput+SubNNi$NbHidden+SubNNi$NbOutput)) + aa
		
		aa <- aa + SubNNi$NbInput + SubNNi$NbHidden
		
		myNeuronsBySubNNB <- c(1:SubNNi$NbBias) + bb
		
		bb <- bb + SubNNi$NbBias
		
		myNeuronsBySubNN[[i]] <- c(myNeuronsBySubNNA, myNeuronsBySubNNB)
	
		names(myNeuronsBySubNN)[i] <- SubNames[i]

	}
	
	# print(SubNames)

	return(list(
			  Weights=myNNWeights
			, OutValues=myNNOutValues
			, Error=myNNError
			, Weighted=myNNWeightedOutValues
			, Summed=myNNSummedWeightedOutValues
			, SquashingFunc=myNNSquashingFunc
			, couches=couches
			, NbCouches=NbCouches
			, NbNeurons=NbNeurons
			, NbInput=NbInput
			, NbHidden=NbHidden
			, NbOutput=NbOutput
			, NbBias=NbBias
			, WhichCouche=WhichCouche
			, TypeOfNeurons=TypeOfNeurons
			, NeuronIds=myNNIds
			, Grad=myNNGrad
			, NeuronsBySubNN=myNeuronsBySubNN
			))
}

CreateSuperNN()


superInit <- function(COUCHES=c(2,2,1), NOM="A"){
	A <- CreateNN(COUCHES)
	A <- CreateSuperNN(list(A=A))
	names(A$NeuronsBySubNN) <- NOM
	A <- InitNN(A)
	return(A)
}
superInit()




A <- CreateSuperNN(list(A=A))
B <- CreateSuperNN(list(B=B))
C <- CreateSuperNN(list(C=C))
D <- CreateSuperNN(list(D=D))

ABCD <- CreateSuperNN(list(A=A,B=B,C=C,D=D))

CDBA <- CreateSuperNN(list(C=C,D=D,A=A,B=B))


# comment transférer A de ABCD vers A ou de ABCD vers CDAB et vice-versa ?

ABCD$Weights[ABCD$NeuronsBySubNN$A,ABCD$NeuronsBySubNN$A] <- A$Weights
ABCD$Weights[ABCD$NeuronsBySubNN$B,ABCD$NeuronsBySubNN$B] <- B$Weights
ABCD$Weights[ABCD$NeuronsBySubNN$C,ABCD$NeuronsBySubNN$C] <- C$Weights
ABCD$Weights[ABCD$NeuronsBySubNN$D,ABCD$NeuronsBySubNN$D] <- D$Weights

CDAB$Weights[CDAB$NeuronsBySubNN$A,CDAB$NeuronsBySubNN$A] <- ABCD$Weights[ABCD$NeuronsBySubNN$A,ABCD$NeuronsBySubNN$A]

# le transfert des poinds d'un superNN à un autre est facile :

CDAB$Weights[CDAB$NeuronsBySubNN$A,CDAB$NeuronsBySubNN$A] <- ABCD$Weights[ABCD$NeuronsBySubNN$A,ABCD$NeuronsBySubNN$A]

# On peut le rendre encore plus simple avec une fonction
SimpleTransfert <- function(FROM=SuperNN1, TO=SuperNN2, SUBNN=list("A")){
	for(i in 1:length(SUBNN)){
		subnni <- SUBNN[[i]]
		Nid1 <- FROM$NeuronsBySubNN[[subnni]]
		Nid2 <- TO$NeuronsBySubNN[[subnni]]
		TO$Weights[Nid2,Nid2] <- FROM$Weights[Nid1,Nid1]
	}
	return(TO)
	
}

A1 <- InitNN(A)
A1$Weights
A2 <- InitNN(A)
A2$Weights
A3 <- A1
A3$Weights
A3 <- SimpleTransfert(FROM=A2,TO=A3,SUBNN="A")
A3$Weights



# On peut le rendre encore plus simple avec une fonction
ComplexTransfert <- function(FROM=list(SuperNN1), TO=list(SuperNN2), SUBNN=list("A")){
	for(i in 1:length(SUBNN)){
		subnni <- SUBNN[[i]]
		# print(subnni)
		for(j in 1:length(FROM)){
			# print(j)
			# print(names(FROM[[j]]$NeuronsBySubNN))
			if(subnni %in% names(FROM[[j]]$NeuronsBySubNN)){
			# print("break")
				break
			}
			
		}

		Nid1 <- FROM[[j]]$NeuronsBySubNN[[subnni]]
		# print("#1")
		fromjW <- FROM[[j]]$Weights[Nid1,Nid1]
		# print("#2")
		for(k in 1:length(TO)){
			
			if(subnni %in% names(TO[[k]]$NeuronsBySubNN)){
				Nid2 <- TO[[k]]$NeuronsBySubNN[[subnni]]
				TO[[k]]$Weights[Nid2,Nid2] <- fromjW
			}
			
		}
		
	}
	return(TO)
	
}

# A1 <- InitNN(A)
# A1$Weights
# A2 <- InitNN(A)
# A2$Weights
# A3 <- A1
# A3$Weights
A3 <- ComplexTransfert(FROM=list(A2),TO=list(A3),SUBNN=list("A"))[[1]]
A3$Weights







# transfert d'une partie deulement des poids d'1 réseau "A" vers un second réseau de même forme matricielle "B"

PartialTransfert <- function(
	  FromWeightMatrix=matrix(1,4,4)
	, ToWeightMatrix=matrix(1,4,4)
	, FromNeurons=c(1,2,3)
	, ToNeurons=c(2,3,4)
	){
	
	dimWM1 <- dim(FromWeightMatrix)
	dimWM2 <- dim(ToWeightMatrix2)
	if((dimWLM1 == 2) & (dimWM1[1] == dimWM1[2]) & all(dimWM1==dimWM2)){
	
		NbNeurons <- dimWM1[1]
		
		
		if(FromNeurons[1]==0){
			# sélectionner tous les neurones
			FromNeurons <- c(1:NbNeurons)
		}else{
		if(FromNeurons[1]<1){
			prob <- FromNeurons[1]
			FromNeurons <- c()
			# sélectionner la fraction indiquée de neurones
			for(i in 1:NbNeurons){
				if(sample(c(0,1),1,c(prob,1-prob))==0){
					FromNeurons[i] <- 1
				}
			}
			FromNeurons <- which(!is.na(FromNeurons))
		}
		}
		
		if(ToNeurons[1]==0){
			# sélectionner tous les neurones
			ToNeurons <- c(1:NbNeurons)
		}else{
		if(ToNeurons[1]<1){
			prob <- ToNeurons[1]
			ToNeurons <- c()
			# sélectionner la fraction indiquée de neurones
			for(i in 1:NbNeurons){
				if(sample(c(0,1),1,c(prob,1-prob))==0){
					ToNeurons[i] <- 1
				}
			}
			ToNeurons <- which(!is.na(ToNeurons))
		}
		}
		
		ToWeightMatrix[FromNeurons,ToNeurons] <- FromWeightMatrix[FromNeurons,ToNeurons]
		
		return(ToWeightMatrix)
		
	
	}else{
		print("matrices de poids de dimensions incorrectes")
	}
	
	
	
}



A3$Weights <- PartialTransfert(
	  FromWeightMatrix=A2$Weights
	, ToWeightMatrix=A3$Weights
	, FromNeurons=c(1,2,3)
	, ToNeurons=c(2,3,4))
A3$Weights


















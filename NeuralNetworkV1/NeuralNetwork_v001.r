


H <- .1		# learning rate (entre 0 et 1)
rorre <- .1		# erreur seuil pour stopper l'apprentissage
act <- function(a){return(1/(1+exp(-a)))}
dact <- function(a){return(act(a)*(1-act(a)))}

# Astuce pour récupérer tous les éléments nommés pareil
# sapply(RB, with, c(id,type,connectedfrom,fire,weight))

# RB <- list()
# Couches <- c(2,4,1)
# for(i in 1:length(Couches)){
	
	# nci <- Couches[i]
	# if(i==1){
		# for(j in 1:nci){
			
			# RB <- c(RB,list(
				# id=j,
				# type="I",
				# connectedfrom=c(),
				# weight=c()
				# )
			# )
			
		# }
	# }else{
		# if(i==length(Couches)){
			
			# for(j in 1:nci){
				# RB <- c(RB,
					# list(
						# id=Couches[1]+sum(Couches[2:I(i-1)])*2+j*2-1,
						# type="C",
						# connectedfrom=c(),
						# weight=c()
					# )
				# RB <- c(RB,
					# list(
						# id=Couches[1]+sum(Couches[2:I(i-1)])*2+j*2,
						# type="O",
						# connectedfrom=c(I(Couches[1]+sum(Couches[2:I(i-1)])*2+j*2-1),I(Couches[1]+sum(Couches[2:I(i-1)])*2-c(c(1:Couches[i-1])*2-1))),
						# weight=c(rnorm(I(Couches[i-1]+1)))
					# )
				# )
				
			# }
			
		# }else{
		# if(i==2){
			
			# for(j in 1:nci){
				# RB <- c(RB,
					# list(
						# id=Couches[1]+sum(Couches[2:I(i-1)])*2+j*2-1,
						# type="C",
						# connectedfrom=c(),
						# weight=c()
					# )
				# RB <- c(RB,
					# list(
						# id=Couches[1]+sum(Couches[2:I(i-1)])*2+j*2,
						# type="O",
						# connectedfrom=c(I(Couches[1]+sum(Couches[2:I(i-1)])*2+j*2-1),I(Couches[1]+sum(Couches[2:I(i-1)])*2-c(c(1:Couches[i-1])*2-1))),
						# weight=c(rnorm(I(Couches[i-1]+1)))
					# )
				# )
				
			# }
			
		# }else{
			
			# for(j in 1:nci){
				# RB <- c(RB,
					# list(
						# id=Couches[1]+sum(Couches[2:i])*2-c(c(nci:1)[j]*2-1),
						# type="C",
						# connectedfrom=c(),
						# weight=c()
					# )
				# RB <- c(RB,
					# list(
						# id=Couches[1]+sum(Couches[2:i])*2-c(c(nci:1)[j]*2-2),
						# type="H",
						# connectedfrom=c(I(Couches[1]+sum(Couches[2:i])*2-c(c(nci:1)[j]*2-1)),I(Couches[1]+sum(Couches[2:I(i-1)])*2-c(c(1:Couches[i-1])*2-1))),
						# weight=c(rnorm(I(Couches[i-1]+1)))
					# )
				# )
				
			# }
			
		# }
	# }
	
# }



RB <- list(
	
	list(
		id=1,
		type="I",	# "I" input ; "O" output ; "C" constant ; "H" hidden
		connectedfrom=c(),	# identifiants des neurones afférants
		fire=0,	# 0 ou 1 ; vers les neurones dont l'attribut "connectedfrom" contient l'"id" de celui-ci
		weight=c()	# coefficient multiplicateur du "fire" reçu des neurones dont l'"id" est contenu dans l'attribut "connectedfrom" de celui-ci		
	),
	
	list(
		id=2,
		type="I",	# "I" input ; "O" output ; "C" constant ; "H" hidden
		connectedfrom=c(),	# identifiants des neurones afférants
		fire=0,	# 0 ou 1
		weight=c()	# coefficient multiplicateur du "fire" reçu des neurones dont l'"id" est contenu dans l'attribut "connectedfrom" de celui-ci		
	),
	
	list(
		id=3,
		type="C",	# "I" input ; "O" output ; "C" constant ; "H" hidden
		connectedfrom=c(),	# identifiants des neurones afférants
		fire=1,	# 0 ou 1 ; vers les neurones dont l'attribut "connectedfrom" contient l'"id" de celui-ci
		weight=c()	# coefficient multiplicateur du "fire" reçu des neurones dont l'"id" est contenu dans l'attribut "connectedfrom" de celui-ci		
	),
	
	list(
		id=4,
		type="H",	# "I" input ; "O" output ; "C" constant ; "H" hidden
		connectedfrom=c(3,1,2),	# identifiants des neurones afférants
		fire=0,	# 0 ou 1 ; vers les neurones dont l'attribut "connectedfrom" contient l'"id" de celui-ci
		weight=rnorm(3)#c(0,0,0)	# coefficient multiplicateur du "fire" reçu des neurones dont l'"id" est contenu dans l'attribut "connectedfrom" de celui-ci		
	),
	
	list(
		id=5,
		type="C",	# "I" input ; "O" output ; "C" constant ; "H" hidden
		connectedfrom=c(),	# identifiants des neurones afférants
		fire=1,	# 0 ou 1 ; vers les neurones dont l'attribut "connectedfrom" contient l'"id" de celui-ci
		weight=c()	# coefficient multiplicateur du "fire" reçu des neurones dont l'"id" est contenu dans l'attribut "connectedfrom" de celui-ci		
	),
	
	list(
		id=6,
		type="H",	# "I" input ; "O" output ; "C" constant ; "H" hidden
		connectedfrom=c(5,1,2),	# identifiants des neurones afférants
		fire=0,	# 0 ou 1 ; vers les neurones dont l'attribut "connectedfrom" contient l'"id" de celui-ci
		weight=rnorm(3)#c(0,0,0)	# coefficient multiplicateur du "fire" reçu des neurones dont l'"id" est contenu dans l'attribut "connectedfrom" de celui-ci		
	),
	
	list(
		id=7,
		type="C",	# "I" input ; "O" output ; "C" constant ; "H" hidden
		connectedfrom=c(),	# identifiants des neurones afférants
		fire=1,	# 0 ou 1 ; vers les neurones dont l'attribut "connectedfrom" contient l'"id" de celui-ci
		weight=c()	# coefficient multiplicateur du "fire" reçu des neurones dont l'"id" est contenu dans l'attribut "connectedfrom" de celui-ci
	),
	
	list(
		id=8,
		type="H",	# "I" input ; "O" output ; "C" constant ; "H" hidden
		connectedfrom=c(7,1,2),	# identifiants des neurones afférants
		fire=0,	# 0 ou 1 ; vers les neurones dont l'attribut "connectedfrom" contient l'"id" de celui-ci
		weight=rnorm(3)#c(0,0,0,0)	# coefficient multiplicateur du "fire" reçu des neurones dont l'"id" est contenu dans l'attribut "connectedfrom" de celui-ci	
	),
		list(
		id=9,
		type="C",	# "I" input ; "O" output ; "C" constant ; "H" hidden
		connectedfrom=c(),	# identifiants des neurones afférants
		fire=1,	# 0 ou 1 ; vers les neurones dont l'attribut "connectedfrom" contient l'"id" de celui-ci
		weight=c()	# coefficient multiplicateur du "fire" reçu des neurones dont l'"id" est contenu dans l'attribut "connectedfrom" de celui-ci
	),
	
	list(
		id=10,
		type="O",	# "I" input ; "O" output ; "C" constant ; "H" hidden
		connectedfrom=c(9,4,6,8),	# identifiants des neurones afférants
		fire=0,	# 0 ou 1 ; vers les neurones dont l'attribut "connectedfrom" contient l'"id" de celui-ci
		weight=rnorm(4)#c(0,0,0,0)	# coefficient multiplicateur du "fire" reçu des neurones dont l'"id" est contenu dans l'attribut "connectedfrom" de celui-ci	
	)
	
)



### Entrainement (ajustement des "weight")


X1 <- sample(c(0,1),100,replace=TRUE)
X2 <- sample(c(0,1),100,replace=TRUE)
X <- rbind(X1,X2)

# Y1 <- as.numeric(xor(X1,X2))
Y1 <- as.numeric((X1&X2))

Y <- rbind(Y1)


windows()
plot.new()
par(usr=c(x0=0,x1=10000, y0=-1,y1=1))
axis(1)
axis(2)
grid()
mtext("ntry",side=1,line=2)
mtext("Erreur",side=2,line=2)


regRes <- list()	# Sorties / Outputs à chaque essai
regErr <- list()	# Erreur Attendu - Output à chaque essai

npassmax <- 100	# nombre de passage maximum sur le jeu de données d'entrainement

ntry <- 0
for(pass in 1:npassmax){	
npass <- pass

for(n in 1:length(Y[1,])){
	ntry <- ntry+1
	print(ntry)
	
	Input <- which(sapply(RB, with, c(type))=="I")
	for(i in 1:length(Input)){
		
		RB[[Input[i]]]["fire"] <- X[i,n]
		
	}
	
	NotInput <- which(sapply(RB, with, c(type))!="I")
	for(i in 1:length(NotInput)){
		
		from <- RB[[NotInput[i]]][["connectedfrom"]]
		if(!is.null(from)){
			input <- c()
			for(k in 1:length(from)){
				input <- c(input,RB[[from[k]]][["fire"]])
			}
			weight <- RB[[NotInput[i]]][["weight"]]
			RB[[NotInput[i]]][["fire"]] <- act(sum(input*weight))#as.numeric(sum(input*weight)>=1)
			# print(RB[[NotInput[i]]][["fire"]])
		}
	}
	
	# calcul des erreurs
	Output <- which(sapply(RB, with, c(type))=="O")
	
	Error <- rep(0,length(RB))
	
	for(i in length(RB):1){
		
		type <- RB[[i]][["type"]]
		
		if(type=="O"){
			
			Error[i] <- RB[[i]][["fire"]]*(1-RB[[i]][["fire"]])*I(Y[which(Output==i),n]-RB[[i]][["fire"]])
			
		}
		if(type=="H"){
			
			for(j in 1:length(RB)){
			
				tmp <- sapply(RB, with, c(connectedfrom))[[j]]
				
				if(i %in% tmp){	# si i est afférent à j
				
					ii <- which(tmp==i)
					
					Error[i] <- Error[i] + Error[j]*sapply(RB, with, c(weight))[[j]][ii]
					
				}
				
			}
			Error[i] <- RB[[i]][["fire"]]*(1-RB[[i]][["fire"]])*Error[i]
		}
	}

	# mise à jour des poids (weightnew = weight*error
	for(i in 1:length(NotInput)){
		
		from <- RB[[NotInput[i]]][["connectedfrom"]]
		if(!is.null(from)){
			
			weight <- RB[[NotInput[i]]][["weight"]]
			
			input <- c()
			for(k in 1:length(from)){
				input <- c(input,RB[[from[k]]][["fire"]])
			}
			
			RB[[NotInput[i]]][["weight"]] <- weight+H*Error[NotInput[i]]*input		# mise à jour des poids
			
		}
	}
	
	Result <- c()
	for(i in 1:length(Output)){
		Result <- c(Result,RB[[Output[i]]][["fire"]])
	}
	Err <- c()
	for(i in 1:length(Output)){
		Err <- c(Err, I(Y[i,n]-RB[[Output[i]]][["fire"]]))
	}
	
	points(Err~rep(ntry,length(Err)),col=1:length(Err))
	# points(Result~rep(ntry,length(Err)),col=1:length(Err))
	
	# print(X[,n])
	# print(cbind(Y[,n],Result))
	regRes <- c(regRes,Result)
	regErr <- c(regErr,Err)
	
	# END <- FALSE			# problème, parfois, avec une seule sortie , l'erreur peut être infime par hasard... dès le début....
	# if(all(abs(Err)<rorre)){
		# END <- TRUE
		# break()
	# }
	
	
}
# if(END){
	# print(paste("Erreur <",rorre,"apres",npass,"passages et",ntry,"essais"))
	# break()
# }
}

windows()
for(i in 1:length(regRes[[1]])){
	# plot(regRes[i,]~Y1[i,])
	test <- is.array(simplify2array(regRes))
	if(test){
		plot(simplify2array(regRes)[i,])
	}else{
		plot(simplify2array(regRes))
	}
	grid()

}

windows()
for(i in 1:length(regErr[[1]])){
	# plot(regErr[i,]~Y1[i,])
	test <- is.array(simplify2array(regErr))
	if(test){
		plot(simplify2array(regErr)[i,])
	}else{
		plot(simplify2array(regErr))
	}
	grid()

}
# rbind(X,Y,regErr)





# test/verification


X1 <- sample(c(0,1),1000,replace=TRUE)
X2 <- sample(c(0,1),1000,replace=TRUE)
X <- rbind(X1,X2)
# Y1 <- as.numeric(xor(X1,X2))
Y1 <- as.numeric((X1&X2))

Y <- rbind(Y1)


regRes <- list()
regErr <- list()

for(n in 1:length(Y[1,])){
	
	print(n)
	
	Input <- which(sapply(RB, with, c(type))=="I")
	for(i in 1:length(Input)){
		
		RB[[Input[i]]]["fire"] <- X[i,n]
		
	}
	
	NotInput <- which(sapply(RB, with, c(type))!="I")
	for(i in 1:length(NotInput)){
		
		from <- RB[[NotInput[i]]][["connectedfrom"]]
		if(!is.null(from)){
			input <- c()
			for(k in 1:length(from)){
				input <- c(input,RB[[from[k]]][["fire"]])
			}
			weight <- RB[[NotInput[i]]][["weight"]]
			RB[[NotInput[i]]][["fire"]] <- act(sum(input*weight))#as.numeric(act(sum(input*weight))>=H)##as.numeric(sum(input*weight)>=1)
			# print(RB[[NotInput[i]]][["fire"]])
		}
	}
	Result <- c()
	for(i in 1:length(Output)){
		Result <- c(Result,RB[[Output[i]]][["fire"]])
	}
	Err <- c()
	for(i in 1:length(Output)){
		Err <- c(Err, I(Y[i,n]-RB[[Output[i]]][["fire"]]))
	}
	
	# print(X[,n])
	# print(cbind(Y[,n],Result))
	regRes <- c(regRes,Result)
	regErr <- c(regErr,Err)
}


# Résultats du modèle
windows()
for(i in 1:length(regRes[[1]])){
	# plot(regRes[i,]~Y1[i,])
	test <- is.array(simplify2array(regRes))
	if(test){
		plot(simplify2array(regRes)[i,])
	}else{
		plot(simplify2array(regRes))
	}
	grid()

}

# erreur résiduelle du modèle
windows()
for(i in 1:length(regErr[[1]])){
	# plot(regErr[i,]~Y1[i,])
	test <- is.array(simplify2array(regErr))
	if(test){
		plot(simplify2array(regErr)[i,])
	}else{
		plot(simplify2array(regErr))
	}
	grid()

}

# Résultats vs attendus
plot(as.vector(simplify2array(regRes))~as.vector(simplify2array(Y)))

# rbind(simplify2array(X),simplify2array(Y),simplify2array(regRes),simplify2array(regErr))


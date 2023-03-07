# >FAIT : structuration automatique du réseau à
 # partir de l'info Couches <- c(2,4,5,1) par exemple (pour un modèle multicouches "simple")
# >FAIT : 	représentation graphique du réseau avec neurones, 
# liens, force des liens (poids) et sens des liens


brain <- function(Couches=c(2,3,1)){
	RB <- list()
	for(i in 1:length(Couches)){
		
		nci <- Couches[i]
		if(i==1){
			for(j in 1:nci){
				
				RB <- c(RB,list(list(
					couche=i,
					id=j,
					type="I",
					connectedfrom=c(),
					fire=0,
					weight=c()
					))
				)
				
			}
		}else{
			if(i==2){
				if(i!=length(Couches)){
					for(j in 1:nci){
						RB <- c(RB,
							list(list(
								couche=i,
								id=Couches[1]+j*2-1,
								type="C",
								connectedfrom=c(),
								fire=1,
								weight=c()
							)),
							list(list(
								couche=i,
								id=Couches[1]+j*2,
								type="H",
								connectedfrom=c(c(1:Couches[i-1]),Couches[1]+j*2-1),
								fire=0,
								weight=c(rnorm(I(Couches[i-1]+1)))
							))
						)
						
					}
				}else{
					for(j in 1:nci){
						RB <- c(RB,
							list(list(
								couche=i,
								id=Couches[1]+j*2-1,
								type="C",
								connectedfrom=c(),
								fire=1,
								weight=c()
							)),
							list(list(
								couche=i,
								id=Couches[1]+j*2,
								type="O",
								connectedfrom=c(c(1:Couches[i-1]),Couches[1]+j*2-1),
								fire=0,
								weight=c(rnorm(I(Couches[i-1]+1)))
							))
						)
						
					}
				}
			}else{
				if(i<1){print("Erreur ! couche à moins de 1 neurone impossible !")}
				if(i>2){			
					if(i!=length(Couches)){
						for(j in 1:nci){
							RB <- c(RB,
								list(list(
									couche=i,
									id=Couches[1]+sum(Couches[2:I(i-1)])*2+j*2-1,
									type="C",
									connectedfrom=c(),
									fire=1,
									weight=c()
								)),
							
								list(list(
									couche=i,
									id=Couches[1]+sum(Couches[2:I(i-1)])*2+j*2,
									type="H",
									connectedfrom=c(sum(Couches[1:I(i-2)])*2-Couches[1]+seq(2,Couches[i-1]*2+1,2),Couches[1]+sum(Couches[2:I(i-1)])*2+j*2-1),					#c(I(Couches[1]+sum(Couches[2:I(i-1)])*2+j*2-1),I(Couches[1]+sum(Couches[2:I(i-1)])*2-c(c(1:Couches[i-1])*2-1))),
									fire=0,
									weight=rnorm(I(Couches[i-1]+1))
								))
							)
							
						}
					}else{
						for(j in 1:nci){
							RB <- c(RB,
								list(list(
									couche=i,
									id=Couches[1]+sum(Couches[2:I(i-1)])*2+j*2-1,
									type="C",
									connectedfrom=c(),
									fire=1,
									weight=c()
								)),
								list(list(
									couche=i,
									id=Couches[1]+sum(Couches[2:I(i-1)])*2+j*2,
									type="O",
									connectedfrom=c(sum(Couches[1:I(i-2)])*2-Couches[1]+seq(2,Couches[I(i-1)]*2+1,2),Couches[1]+sum(Couches[2:I(i-1)])*2+j*2-1),					#c(I(Couches[1]+sum(Couches[2:I(i-1)])*2+j*2-1),I(Couches[1]+sum(Couches[2:I(i-1)])*2-c(c(1:Couches[i-1])*2-1))),
									fire=0,
									weight=rnorm(I(Couches[I(i-1)]+1))
								))
							)
							
						}
					}
				}
			}
		}
	}
	return(RB)
}


act <- function(a){return(1/(1+exp(-a)))}
# dact <- function(a){return(act(a)*(1-act(a)))}

invact <- function(a){return(log(-a/(a-1)))}

# fonction d'activation des neurones de la couche de sortie
# act.out <- act	# classique
# ou lineaire
# act.out <- function(a){return(a)}


### Entrainement (ajustement des "weight")

X1 <- sample(c(0,1),100,replace=TRUE)
X2 <- sample(c(0,1),100,replace=TRUE)
X <- rbind(X1,X2)
# Y1 <- as.numeric(xor(X1,X2))
Y1 <- as.numeric((X1&X2))
Y <- rbind(Y1)



# test estimation pente d'une droite définie par 2 points d'abcisse 1 et 2, et d'ordonnée X1 et X2
Nx <- 10000
X1 <- runif(Nx)
X2 <- runif(Nx)
X3 <- runif(Nx)
X4 <- runif(Nx)
X <- rbind(X1,X2,X3,X4)

Y1 <- c()
for(i in 1:Nx){
	Y1 <- c(Y1,coef(lm(c(X1[i],X2[i])~c(X3[i],X4[i])))[[2]])
}
Y <- rbind(Y1)



# test regression lineaire simple
Nx <- 10000
X1 <- 100*runif(Nx)

X <- rbind(X1)

Y1 <- 3*X1+2+rnorm(Nx)

maxY1 <- max(abs(Y1))
Y1 <- 0.25+Y1/(2*maxY1)
Y <- rbind(Y1)




H <- .5		# learning rate (entre 0 et 1)
rorre <- .01		# erreur seuil pour stopper l'apprentissage

# Astuce pour récupérer tous les éléments nommés pareil
# sapply(RB, with, c(id,type,connectedfrom,fire,weight))

Couches <- c(2,4,1)

RB <- brain(c(Couches))

# sapply(RB, with, c(couche,id,type,connectedfrom,fire,weight))





windows()
plot.new()
par(usr=c(x0=0,x1=10000, y0=-1,y1=1))	# si Y entre 0 et 1 ou comparaison des valeurs dans l'ensemble transforme entre 0 et 1
# par(usr=c(x0=0,x1=10000, y0=-0.1,y1=0.1))	# ???
# par(usr=c(x0=0,x1=10000, y0=-100,y1=100))	# compare les valeurs de Y brutes avec les sorties retransformees du modele

polygon(x=c(par("usr")[1],par("usr")[1],par("usr")[2],par("usr")[2]), y=c(-rorre,rorre,rorre,-rorre), density=NA, col=rgb(.9, .9, .9), border=0)

axis(1)
axis(2)
grid()
mtext("ntry",side=1,line=2)
mtext("Erreur",side=2,line=2)
box()
# abline(h=c(-rorre,rorre),col="grey",lwd=2)


regRes <- list()	# Sorties / Outputs à chaque essai
regErr <- list()	# Erreur Attendu - Output à chaque essai

notsobad <- c()

npassmax <- 100	# nombre de passage maximum sur le jeu de données d'entrainement

ntry <- 0
for(pass in 1:npassmax){	
npass <- pass
END <- FALSE
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
			
			# CALCUL DES SORTIES
			if(RB[[NotInput[i]]][["type"]]=="O"){
				RB[[NotInput[i]]][["fire"]] <- act(sum(input*weight))
				# RB[[NotInput[i]]][["fire"]] <- sum(input*weight)
			}else{
				RB[[NotInput[i]]][["fire"]] <- act(sum(input*weight))#as.numeric(sum(input*weight)>=1)
			}
			
			# print(RB[[NotInput[i]]][["fire"]])
		}
	}
	
	# calcul des erreurs
	Output <- which(sapply(RB, with, c(type))=="O")
	
	Error <- rep(0,length(RB))
	
	for(i in length(RB):1){
		
		type <- RB[[i]][["type"]]
		
		if(type=="O"){
			
			Error[i] <- RB[[i]][["fire"]]*(1-RB[[i]][["fire"]]) * I(Y[which(Output==i),n]-RB[[i]][["fire"]])	# possible uniquement si Y compris entre 0 et 1
			# Error[i] <- RB[[i]][["fire"]]*(1-RB[[i]][["fire"]])*I(Y[which(Output==i),n]/1000-RB[[i]][["fire"]])	# A oublier
			# Error[i] <- RB[[i]][["fire"]]*(1-RB[[i]][["fire"]])*I(act(Y[which(Output==i),n])-RB[[i]][["fire"]])	# compare la transformee de Y avec la sortie brute du modele
			# Error[i] <- RB[[i]][["fire"]]*(1-RB[[i]][["fire"]])*I(Y[which(Output==i),n]-invact(RB[[i]][["fire"]]))	# compare Y avec la sortie retransformee du modele
			
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

	# MISE A JOUR DES POIDS (weightnew = weight*error
	for(i in 1:length(NotInput)){
		
		from <- RB[[NotInput[i]]][["connectedfrom"]]
		if(!is.null(from)){
			
			weight <- RB[[NotInput[i]]][["weight"]]
			
			input <- c()
			for(k in 1:length(from)){
				input <- c(input,RB[[from[k]]][["fire"]])
			}
			
			RB[[NotInput[i]]][["weight"]] <- weight+H*Error[NotInput[i]]*input		# mise à jour des poids
			# RB[[i]][["fire"]]
			
			# RB[[NotInput[i]]][["weight"]] <- weight+H*RB[[i]][["fire"]]*(1-RB[[i]][["fire"]])*
			
			# RB[[NotInput[i]]][["weight"]] <- weight+H*Error[NotInput[i]]*weight		# mise à jour des poids

		}
	}
	
	Result <- c()
	for(i in 1:length(Output)){
		Result <- c(Result,RB[[Output[i]]][["fire"]])
	}
	Err <- c()
	for(i in 1:length(Output)){
		Err <- c(Err, I(Y[i,n]-RB[[Output[i]]][["fire"]]))	# possible uniquement si Y compris entre 0 et 1
		# Err <- c(Err, I(Y[i,n]/1000-RB[[Output[i]]][["fire"]]))	# A oublier
		# Err <- c(Err, I(act(Y[i,n])-RB[[Output[i]]][["fire"]]))	# compare la transformee de Y avec la sortie brute du modele
		# Err <- c(Err, I(Y[i,n]-invact(RB[[Output[i]]][["fire"]])))	# compare Y avec la sortie retransformee du modele
	}
	
	points(Err~rep(ntry,length(Err)),col=1:length(Err))
	# points(Result~rep(ntry,length(Err)),col=1:length(Err))
	
	# print(X[,n])
	# print(cbind(Y[,n],Result))
	regRes <- c(regRes,Result)
	regErr <- c(regErr,Err)
	
	if(all(abs(Err)<rorre)){
		notsobad <- c(notsobad,TRUE)
	}else{
		notsobad <- c(notsobad,FALSE)
	}
	
				# problème, parfois, avec une seule sortie , l'erreur peut être infime par hasard... dès le début....
	if(length(notsobad)>100){
		if(all(notsobad[I(length(notsobad)-100):length(notsobad)])){
			END <- TRUE
			break()
		}
	}
	
}
if(END){
	print(paste("Erreur <",rorre,"apres",npass,"passages et",ntry,"essais"))
	break()
}
}


# Résultats de l'entrainement
	# regRes = sorties brutes
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

	# regErr = erreur par rapport à l'attendu
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
# X3 <- sample(c(0,1),1000,replace=TRUE)

X <- rbind(X1,X2)
# X <- rbind(X1,X2,X3)

# Y1 <- as.numeric(xor(X1,X2))
Y1 <- as.numeric((X1&X2))
# Y1 <- as.numeric((X1&X2&X3))

Y <- rbind(Y1)




X1 <- runif(1000)
X2 <- runif(1000)
X3 <- runif(1000)
X4 <- runif(1000)
X <- rbind(X1,X2,X3,X4)

Y1 <- c()
for(i in 1:length(X1)){
	Y1 <- c(Y1,coef(lm(c(X1[i],X2[i])~c(X3[i],X4[i])))[[2]])
}
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
# plot(invact(as.vector(simplify2array(regRes)))~as.vector(simplify2array(Y)), xlim=c(0,10), y=c(0,10))
# plot((as.vector(simplify2array(regRes)))~act(as.vector(simplify2array(Y))), xlim=quantile(act(as.vector(simplify2array(Y))),c(.1,.9)))

# rbind(simplify2array(X),simplify2array(Y),simplify2array(regRes),simplify2array(regErr))




# représentation graphique du réseau avec neurones, 
# liens, force des liens et sens des liens
# Couches <- c(3,4,4,3,2,5,1)
# RB <- brain(Couches)

# sapply(RB, with, c(couche,id,type,connectedfrom,fire,weight))
neurocouch <- as.vector(sapply(RB, with, c(couche)))
couches <- sort(unique(neurocouch))

windows()
plot.new()
par(usr=c(x0=0, x1=max(neurocouch)+1, y0=-max(Couches)-2, y1=max(Couches)+2))
# par(usr=c(x0=, x1=, y0=, y1=))

for(i in 1:length(neurocouch)){
	couchid <- neurocouch[i]
	couchallneur_beforei <- which(neurocouch<couchid)
	if(length(couchallneur_beforei>0)){
		couchallneurid <- which(neurocouch<=couchid)[- which(neurocouch<couchid)]
	}else{
		couchallneurid <- which(neurocouch<=couchid)
	}
	yy <- I(-(which(couchallneurid==i)-.5-length(couchallneurid)/2))#which(couchallneurid==i)
	# print(c(couchid,yy))
	xx <- couchid
	points(x=xx,y=yy, cex=3)
	text(x=xx,y=yy, labels=i, cex=1)
	
	# id des neurones afférents
	affNeurId <- sapply(RB, with, c(connectedfrom))[[i]]
	if(!is.null(affNeurId)){
		for(j in affNeurId){
			jnb <- which(affNeurId==j)
			
			couchidAff <- neurocouch[j]
			couchallneur_beforei <- which(neurocouch<couchidAff)
			if(length(couchallneur_beforei>0)){
				couchallneurid <- which(neurocouch<=couchidAff)[- which(neurocouch<couchidAff)]
			}else{
				couchallneurid <- which(neurocouch<=couchidAff)
			}
			yyAff <- I(-(which(couchallneurid==j)-.5-length(couchallneurid)/2))#which(couchallneurid==i)
			xxAff <- couchidAff
			
			yyAff2 <- yyAff
			yy2 <- yy
			xx2 <- xx
			xxAff2 <- xx
			
			if(couchidAff==couchid){	# bias neuron and weight
			
				yyAff2 <- yyAff-sign(yyAff-yy)*.2
				yy2 <- yy+sign(yyAff-yy)*.2
				ybiasadd <- 1/(max(couches)+2)
				
			}else{
				xxAff2 <- xxAff+.1
				xx2 <- xx2-.1
				ybiasadd <- 0
			}
			
			arrows(x0=xxAff2, y0=yyAff2, x1 = xx2, y1 = yy2, length = 0.05, angle = 30,
			code = 2, col = "black", lty = par("lty"),
			lwd = 1)
			
			wei <- round(sapply(RB, with, c(weight))[[i]][jnb],2)
			text(x=I(xxAff2+(xx2-xxAff2)/1.2), y=I(yyAff2+(yy2-yyAff2)/1.2+ybiasadd),
				labels=wei,
				cex=.5,
				col=I(3+sign(wei))
			)
			
		}
	}
	
	
}








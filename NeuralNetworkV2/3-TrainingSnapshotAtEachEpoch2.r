
regRes <- list()	# Sorties / Outputs à chaque essai
regErr <- list()	# Erreur Attendu - Output à chaque essai

notsobad <- c()

npassmax <- 100	# nombre de passage maximum sur le jeu de données d'entrainement

Ylen <- length(Y[1,])

usrx1 <- npassmax*Ylen

windows()
plot.new()
par(usr=c(0,usrx1,-1,1))	# si Y entre 0 et 1 ou comparaison des valeurs dans l'ensemble transforme entre 0 et 1
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

NNList <- list()


ntry <- 0
for(pass in 1:npassmax){


npass <- pass
END <- FALSE
for(n in 1:Ylen){
	ntry <- ntry+1
	#print(c("ntry=",ntry))
	
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
	
	
	test <- c()
	for(i in 1:length(NotInput)){
		
		from <- RB[[NotInput[i]]][["connectedfrom"]]
		if(!is.null(from)){
			
			
			weight <- RB[[NotInput[i]]][["weight"]]
			
			input <- c()
			for(k in 1:length(from)){
				input <- c(input,RB[[from[k]]][["fire"]])
			}
			
			RB[[NotInput[i]]][["weight"]] <- weight+H*Error[NotInput[i]]*input		# mise à jour des poids
			#RB[[i]][["fire"]]
			
			test <- c(test,RB[[NotInput[i]]][["weight"]])
			
			
		}
	}
	
	if(ntry==1){
		print("ntry==1")
		NNA <- list()

		# first <- ntry
		
	}
	
	
	for(k in 1:length(test)){
		#print(c("k=",k))
		#print(NNA)
		if(is.null(NNA[k][[1]])){
			#print("NULL")
			NNA[k] <- test[k]
		}else{
			#print("NOTNULL")
			NNA[[k]] <- c(NNA[[k]],test[k])
		}
		
		#print(length(NNA[[1]]))
		
		if(length(NNA[length(test)][[1]]) == 3){
			#print("NNA[[1]]) == 3")
			
			for(m in 1:length(test)){
				NNData <- data.frame(x=c(1:length(NNA[[m]])), y=NNA[[m]])
				NNmod <- lm(y~x, data=NNData)
				newW <- predict(NNmod, new=data.frame(x=4))
			
				ki <- 0
				for(i in 1:length(NotInput)){
					from <- RB[[NotInput[i]]][["connectedfrom"]]
					if(!is.null(from)){
						for(l in 1:length(from)){
							ki <- ki + 1
							if(ki == m){
								#print(c("neurone ",m,"=",RB[[NotInput[i]]][["weight"]][l]," = ", newW))
								#RB[[NotInput[i]]][["weight"]][l] <- newW
							}
						}
					}
				}
			}
			
			# réinit
			NNA <- list()
			
			
		}
		
		
	}

	
	
	
	# sauvegarde de l'état du réseau de neurones
	# if(n==Ylen){
		# NNList[[ntry]] <- test
	# }
	
	# NNList[[ntry]] <- test
	
	# sauvegarde de l'état du réseau de neurones
	# if(n==Ylen & npass%%3==0){
		# for(k in 1:){
		
		# }
		# NNList[[ntry]] <- test
	# }
	
	
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
		plot(simplify2array(regRes)[i,], col=i)
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
		plot(simplify2array(regErr)[i,], col=i)
	}else{
		plot(simplify2array(regErr))
	}
	grid()

}

# rbind(X,Y,regErr)

# test <- c()
# for(i in 1:length(NNList)){
	
	# test[i] <- NNList[[i]][[11]]$weight[1]
	
# }

# Llen <- length(NNList)
# mnilen <- length(NNList[[NotInput[1]]][["weight"]])

# mtest <- matrix(data = NA, nrow = Llen, ncol = mnilen)
# for(i in 1:Llen){
	# for(j in 1:mnilen){
	# mtest[i] <- NNList[[i]][[11]]$weight[1]
	
# }


# plot(unlist(lapply(NNList,'[[', 1))[seq(1,length(NNList),1000)], col=1, xlim=c(0,80), ylim=c(-10,10))
# for(i in 2:length(NNList[[1]])){
	# points(unlist(lapply(NNList,'[[', i))[seq(1,length(NNList),1000)], col=i)
# }


# plot(unlist(lapply(NNList,'[[', 1)), col=1, xlim=c(0,20), ylim=c(-10,10), type="b", lty="solid")
# for(i in 2:length(NNList)){
	# lines(unlist(lapply(NNList,'[[', i)), col=i, type="b")
# }

# plot(unlist(lapply(NNList,'[[', 1)), col=1, ylim=c(-10,10), type="l", lty="solid")
# for(i in 2:length(NNList)){
	# lines(unlist(lapply(NNList,'[[', i)), col=i, type="l")
# }


# test <- unlist(lapply(NNList,'[[', 1))
# testlen <- length(test)
# nls(test~A*c(1:testlen)^B)
# plot(unlist(lapply(NNList,'[[', 1)), col=1, ylim=c(-5,5), type="l", lty="solid")
# plot(c(0.73574*c(1:testlen)^0.08052)~c(1:testlen))

plot(NNA[[1]])










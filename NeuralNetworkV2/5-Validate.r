
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



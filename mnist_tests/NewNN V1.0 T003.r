source("C:/Users/hades/Documents/NeuralNetworks/BoucleNN/fonctions communes.r")


########## test 2

set.seed(1)

test <- CreateNN(couches=c(2,2,1))
test <- InitNN(test)

saveW1 <- test$Weights
# CompleteRG(NN=test,JDD_INPUT=c(0,0),ATTENDU=c(0.1),H=0.5)$NN$Weights

EnAvant(test,c(0,0))
EnAvant(test,c(0,1))
EnAvant(test,c(1,0))
EnAvant(test,c(1,1))

for(i in 1:10000){
	test <- CompleteRG(NN=test,JDD_INPUT=list(c(0,0)),ATTENDU=list(c(0.1)),H=0.1)$NN
	# print(test$Weights)
	test <- CompleteRG(NN=test,JDD_INPUT=list(c(0,1)),ATTENDU=list(c(0.3)),H=0.1)$NN
	# print(test$Weights)
	test <- CompleteRG(NN=test,JDD_INPUT=list(c(1,0)),ATTENDU=list(c(0.5)),H=0.1)$NN
	# print(test$Weights)
	test <- CompleteRG(NN=test,JDD_INPUT=list(c(1,1)),ATTENDU=list(c(0.7)),H=0.1)$NN
	print(test$Weights)
	# print(paste("############", round(c(EnAvant(test,c(0,0)),EnAvant(test,c(0,1)),EnAvant(test,c(1,0)),EnAvant(test,c(1,1))),5)))

}

EnAvant(test,c(0,0))
EnAvant(test,c(0,1))
EnAvant(test,c(1,0))
EnAvant(test,c(1,1))

# saveW2 <- test$Weights

# print(saveW1)
# print(saveW2)


# avec dact
# > EnAvant(test,c(0,0))
# [1] 0.0919334
# > EnAvant(test,c(0,1))
# [1] 0.3102424
# > EnAvant(test,c(1,0))
# [1] 0.5112465
# > EnAvant(test,c(1,1))
# [1] 0.6860493

# sans dact
# > EnAvant(test,c(0,0))
# [1] 0.1026669
# > EnAvant(test,c(0,1))
# [1] 0.2996571
# > EnAvant(test,c(1,0))
# [1] 0.4997199
# > EnAvant(test,c(1,1))
# [1] 0.6999242


########## test 3

# saveW1 <- test$Weights
# CompleteRG(NN=test,JDD_INPUT=c(0,0),ATTENDU=c(0.1),H=0.5)$NN$Weights

# EnAvant(test,c(0,0))
# EnAvant(test,c(0,1))
# EnAvant(test,c(1,0))
# EnAvant(test,c(1,1))


windows()
plot.new()
par(mfrow=c(5,5), mar=c(0,0,0,0))



Hseq <- seq(0.01,50,0.5)
Nrep <- 100
# Hseq <- seq(2,10,0.1)
# Hseq <- seq(8,15,0.1)
# Hseq <- seq(8,50,0.5)

for(k in 1:25){

	t1 <- c()
	t2 <- c()
	t3 <- c()
	t4 <- c()

	for(j in 1:length(Hseq)){
		
		hj <- Hseq[j]
		set.seed(k)
		test <- CreateNN(couches=c(2,2,1))
		test <- InitNN(test)

		for(i in 1:Nrep){
			test <- CompleteRG(NN=test,JDD_INPUT=list(c(0,0)),ATTENDU=list(c(0.1)),H=hj)$NN
			test <- CompleteRG(NN=test,JDD_INPUT=list(c(0,1)),ATTENDU=list(c(0.3)),H=hj)$NN
			test <- CompleteRG(NN=test,JDD_INPUT=list(c(1,0)),ATTENDU=list(c(0.5)),H=hj)$NN
			test <- CompleteRG(NN=test,JDD_INPUT=list(c(1,1)),ATTENDU=list(c(0.7)),H=hj)$NN
			# print(paste("############", round(c(EnAvant(test,c(0,0)),EnAvant(test,c(0,1)),EnAvant(test,c(1,0)),EnAvant(test,c(1,1))),5)))

		}
		
		t1[j] <- EnAvant(test,c(0,0))
		t2[j] <- EnAvant(test,c(0,1))
		t3[j] <- EnAvant(test,c(1,0))
		t4[j] <- EnAvant(test,c(1,1))
	}

	plot(t1~Hseq, col="black", ylim=c(0,1), main=paste(c("N = ",Nrep)))
	points(y=t2, x=Hseq,col="red")
	points(y=t3, x=Hseq,col="green")
	points(y=t4, x=Hseq,col="blue")

	abline(h=0.1,col="black")
	abline(h=0.3,col="red")
	abline(h=0.5,col="green")
	abline(h=0.7,col="blue")

}



########## test 4


windows()
par(mfrow=c(5,5))


for(k in 1:25){

	t1 <- c()
	t2 <- c()
	t3 <- c()
	t4 <- c()

	Hseq <- seq(0.1,20,0.5)
	Nrep <- 10
	# Hseq <- seq(2,10,0.1)
	# Hseq <- seq(8,15,0.1)
	# Hseq <- seq(8,50,0.5)

	for(j in 1:length(Hseq)){
		
		hj <- Hseq[j]
		set.seed(k)
		test <- CreateNN(couches=c(2,2,1))
		test <- InitNN(test)

		for(i in 1:Nrep){
			test <- CompleteRG(NN=test,JDD_INPUT=list(c(0,0),c(0,1),c(1,0),c(1,1)),ATTENDU=list(c(0.1),c(0.3),c(0.5),c(0.7)),H=hj, batchSize=4)$NN
			# test <- CompleteRG(NN=test,JDD_INPUT=list(c(0,1)),ATTENDU=list(c(0.3)),H=hj)$NN
			# test <- CompleteRG(NN=test,JDD_INPUT=list(c(1,0)),ATTENDU=list(c(0.5)),H=hj)$NN
			# test <- CompleteRG(NN=test,JDD_INPUT=list(c(1,1)),ATTENDU=list(c(0.7)),H=hj)$NN
			# print(paste("############", round(c(EnAvant(test,c(0,0)),EnAvant(test,c(0,1)),EnAvant(test,c(1,0)),EnAvant(test,c(1,1))),5)))

		}
		
		t1[j] <- EnAvant(test,c(0,0))
		t2[j] <- EnAvant(test,c(0,1))
		t3[j] <- EnAvant(test,c(1,0))
		t4[j] <- EnAvant(test,c(1,1))
	}

	plot(t1~Hseq, col="black", ylim=c(0,1), main=paste(c("N = ",Nrep)))
	points(y=t2, x=Hseq,col="red")
	points(y=t3, x=Hseq,col="green")
	points(y=t4, x=Hseq,col="blue")

	abline(h=0.1,col="black")
	abline(h=0.3,col="red")
	abline(h=0.5,col="green")
	abline(h=0.7,col="blue")

}

### bilan de ces tests : il faut plus de 10 présentations, mais moins de 100, du JDD complet pour espérer apprendre correctment le JDD sur ce réseau de neurones
### avec des coefs d'apprentissage élevés



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



# Galaxy 2
# ajuster les masses et constantes de calcul pour simuler le vrai système solaire
# passer en 3D ?

dTime <- 3600*24#*10	# pas de temps
tmax <- 20*365*24*3600#*165	# temps max de simulation
mix <- TRUE		# tracer le spoints ou pas à partir d'un certain temps
ttrace <- 5	# temps à partir duquel on trace les points
imax <- tmax/dTime
itrace <- imax*ttrace/tmax


# Galaxy
Spacedim=3
x.lim <- c(-5000000000000,5000000000000)
y.lim <- x.lim

G <- 6.6742*10^(-11)

Nstars <- 9
masse <- rep(1,Nstars)
sun <- c(1)
masse[sun] <- 2*10^30
masse[2:Nstars] <- c(
330*10^21, 
4.9*10^24, 
6*10^24, 
342*10^21, 
1.89*10^27, 
568*10^24, 
86.8*10^24, 
102*10^24
)

# masse[3] <- 500
pchsun <- rep(1,Nstars)
pchsun[sun] <- 3
pchsun2d <- pchsun
pchsun2d[which(pchsun2d==1)] <- 19

Galaxy <- array(NA, dim=c(Spacedim*2,Nstars))
# > Galaxy

# version 2D
#	   star1 star2 star3 ... jusqu'à Nstars étoiles
     # [,1] [,2] [,3]
# [1,]   NA   NA   NA	X	position en x
# [2,]   NA   NA   NA	Y	position en Y
# [3,]   NA   NA   NA	Fx	composante en X de la vitesse de l'étoile
# [4,]   NA   NA   NA	Fy	composante en Y de la vitesse de l'étoile
#	   star1 star2 star3

# version 3D
#	   star1 star2 star3 ... jusqu'à Nstars étoiles
     # [,1] [,2] [,3] 
# [1,]   NA   NA   NA	X	position en x
# [2,]   NA   NA   NA	Y	position en Y
# [3,]   NA   NA   NA	Y	position en Z
# [4,]   NA   NA   NA	Fx	composante en X de la vitesse de l'étoile
# [5,]   NA   NA   NA	Fy	composante en Y de la vitesse de l'étoile
# [6,]   NA   NA   NA	Fz	composante en Z de la vitesse de l'étoile


# assignation d'une position arbitraire, et d'une vitesse aléatoire dans une direction aléatoire, dans l'espace R^Ndim
for(star in 1:length(Galaxy[1,])){
	
	Galaxy[c(1:Spacedim),star] <- c(rnorm(Spacedim, 0,x.lim[2]/4))# c(star*10,rep(0,Spacedim-1))# c(runif(Spacedim, x.lim[1],x.lim[2]))
	Galaxy[c(I(Spacedim+1):I(Spacedim*2)),star] <- c(rnorm(Spacedim,0,200))# rnorm(2,0,500)	# c(0,0)#c(runif(2, -1,1))
	
}
#soleil n°1 au centre
Galaxy[1:Spacedim,1] <- rep(0,Spacedim)
# soleils dans le plan z=0
Galaxy[3,sun] <- 0


# position initiale sur l'axe x
Galaxy[1:3,2] <- c(58000000000,0,0)# rnorm(Nstars-1,0,100)	# runif(Nstars-1,-300,300)
Galaxy[1:3,3] <- c(108000000000,0,0)# rnorm(Nstars-1,0,100)	# runif(Nstars-1,-300,300)
Galaxy[1:3,4] <- c(150000000000,0,0)# rnorm(Nstars-1,0,100)	# runif(Nstars-1,-300,300)
Galaxy[1:3,5] <- c(228000000000,0,0)# rnorm(Nstars-1,0,100)	# runif(Nstars-1,-300,300)
Galaxy[1:3,6] <- c(778000000000,0,0)# rnorm(Nstars-1,0,100)	# runif(Nstars-1,-300,300)
Galaxy[1:3,7] <- c(1421000000000,0,0)# rnorm(Nstars-1,0,100)	# runif(Nstars-1,-300,300)
Galaxy[1:3,8] <- c(2877000000000,0,0)# rnorm(Nstars-1,0,100)	# runif(Nstars-1,-300,300)
Galaxy[1:3,9] <- c(4500000000000,0,0)# rnorm(Nstars-1,0,100)	# runif(Nstars-1,-300,300)


# vitesse initiale des planetes non nulle
# for(i in 2:9){
	# Galaxy[4:6,i] <- c(0,I(100000*runif(1,0,1)),I(200*(runif(1,0,1)-0.5)))
	# Galaxy[4:6,i] <- c(-1000,15000,0)
# }
Galaxy[4:6,2] <- c(0,40000,0)# rnorm(Nstars-1,0,100)	# runif(Nstars-1,-300,300)
Galaxy[4:6,3] <- c(0,35000,0)# rnorm(Nstars-1,0,100)	# runif(Nstars-1,-300,300)
Galaxy[4:6,4] <- c(0,30000,0)# rnorm(Nstars-1,0,100)	# runif(Nstars-1,-300,300)
Galaxy[4:6,5] <- c(0,22000,0)# rnorm(Nstars-1,0,100)	# runif(Nstars-1,-300,300)
Galaxy[4:6,6] <- c(0,12400,0)# rnorm(Nstars-1,0,100)	# runif(Nstars-1,-300,300)
Galaxy[4:6,7] <- c(0,9000,0)# rnorm(Nstars-1,0,100)	# runif(Nstars-1,-300,300)
Galaxy[4:6,8] <- c(0,6500,0)# rnorm(Nstars-1,0,100)	# runif(Nstars-1,-300,300)
Galaxy[4:6,9] <- c(0,5300,0)# rnorm(Nstars-1,0,100)	# runif(Nstars-1,-300,300)

# 2 dim case
if(Spacedim==2){

plot(Galaxy[2,]~Galaxy[1,], xlim=x.lim,ylim=y.lim, col=rainbow(Nstars)[1:length(Galaxy[1,])], pch=1)

points(Galaxy[2,]~Galaxy[1,], col=rainbow(Nstars)[1:length(Galaxy[1,])], pch=1)
text(Galaxy[2,]~Galaxy[1,], labels=c(1:Nstars))
}

# 3 dim case
if(Spacedim==3){
ordZ <- order(Galaxy[3,])
plot(Galaxy[2,][ordZ]~Galaxy[1,][ordZ], xlim=x.lim,ylim=y.lim, col=rainbow(Nstars)[1:length(Galaxy[1,])][ordZ], pch=1, cex=sqrt(exp(Galaxy[3,]))[ordZ])

points(Galaxy[2,][ordZ]~Galaxy[1,][ordZ], col=rainbow(Nstars)[1:length(Galaxy[1,])][ordZ], pch=1, cex=sqrt(exp(Galaxy[3,]))[ordZ])
text(Galaxy[2,]~Galaxy[1,], labels=c(1:Nstars))

}

# arrows(x0,y0,x1,y1)
# arrows(Galaxy[1,],Galaxy[2,],Galaxy[1,]+Galaxy[3,]/10,Galaxy[2,]+Galaxy[4,]/10)

# fonction "déplacement des étoiles dans la galaxie"
moveGalaxy <- function(m,Dimension,dT){
	nstars <- length(m[1,])
	for(i in 1:Spacedim){
		m[i,c(1:nstars)[-sun]] <- m[i,c(1:nstars)[-sun]]+m[i+Dimension,c(1:nstars)[-sun]]*dT
	}
	# for(Dim in 1:Dimension){
		# m <- m[,which(abs(m[Dim,])<x.lim[2])]
	# }
	return(m)
	
}

# Galaxy <- moveGalaxy(Galaxy,Spacedim,dTime)
# points(Galaxy[2,]~Galaxy[1,], col=rainbow(Nstars)[1:length(Galaxy[1,])], pch=1)

# fonction "action de la gravité sur la vitesse des étoiles"
Gravity <- function(m,Dimension,dT){
	
	# G <- 1	# gravité
	
	nstars <- length(m[1,])
	
	# calculer la nouvelle vitesse de chaque étoile, en prenant en compte l'effet de toutes les autres étoiles
	for(stari in 1:nstars){
		#print(m)
		for(axe in 1:Dimension){
		
			accel <- 0
			
			for(starj in c(1:nstars)[-stari]){
				#print(paste(starj,"-",stari,": axe",axe))
				
				# distance entre l'étoile i et la j
				ijdist <- sqrt(sum( (m[c(1:Dimension),starj]-m[c(1:Dimension),stari])^2 ))
				# print(ijdist)
				# vecteur unitaire dirigé de l'étoile i vers la j, projeté sur l'axe
				axeunit <- (m[axe,starj]-m[axe,stari])/ijdist
				# print(axeunit)
				# calcul de la nouvelle accélération, prenant en compte l'étoile j
				accel <- accel + G*masse[starj]*axeunit/(ijdist^2)
				# print(accel)
			}
			# calcul de la nouvelle vitesse selon l'axe
			m[axe+Dimension,stari] <- m[axe+Dimension,stari]+I(accel*dT)
			
		}
		
	}
	#print(m)
	return(m)
}

# Gravity(Galaxy,Spacedim,dTime)

# graphic
dTime2 <- dTime
for(i in 1:imax){
	Galaxy <- Gravity(Galaxy,Spacedim,dTime2)
	Galaxy <- moveGalaxy(Galaxy,Spacedim,dTime2)
	Nstars2 <- length(Galaxy[1,])
	dTime2 <- dTime*sqrt(Nstars2)


	# points(Galaxy[2,]~Galaxy[1,], col=rainbow(Nstars2)[1:length(Galaxy[1,])], pch=1)
	# plot(Galaxy[2,]~Galaxy[1,], xlimxlim=x.lim,ylim=y.lim, col=rainbow(Nstars2)[1:length(Galaxy[1,])], pch=19)

	# 2 dim case
	if(Spacedim==2 & !mix){
		plot(Galaxy[2,]~Galaxy[1,], xlim=x.lim,ylim=y.lim, col=rainbow(Nstars2)[1:length(Galaxy[1,])], pch=pchsun2d)
		# points(Galaxy[2,]~Galaxy[1,], col=rainbow(Nstars2)[1:length(Galaxy[1,])], pch=pchsun)
	}

	# 3 dim case
	if(Spacedim==3){
		ordZ <- order(Galaxy[3,])
		# plot(Galaxy[2,][ordZ]~Galaxy[1,][ordZ], xlim=x.lim,ylim=y.lim, col=rainbow(Nstars2)[1:length(Galaxy[1,])][ordZ], pch=pchsun[ordZ], cex=(sqrt(log(Galaxy[3,])))[ordZ])
		# plot(Galaxy[2,][ordZ]~Galaxy[1,][ordZ], xlim=x.lim,ylim=y.lim, col=rainbow(Nstars2)[1:length(Galaxy[1,])][ordZ], pch=pchsun[ordZ], cex=1)
		# ordz <- ordZ[length(ordZ):1]
		# points(Galaxy[2,][ordz]~Galaxy[1,][ordz], col=rainbow(Nstars2)[1:length(Galaxy[1,])][ordz], pch=1, cex=(exp(Galaxy[3,])^.02)[ordz])
		points(Galaxy[2,][ordz]~Galaxy[1,][ordz], col=rainbow(Nstars2)[1:length(Galaxy[1,])][ordz], pch=1, cex=0.1)
	}

	# 2 dim case + tracer les points en cours de route
	if(Spacedim==2 & mix){
	if(i<itrace){
		plot(Galaxy[2,]~Galaxy[1,], xlim=x.lim,ylim=y.lim, col=rainbow(Nstars2)[1:length(Galaxy[1,])], pch=pchsun)
	}else{
		points(Galaxy[2,]~Galaxy[1,], col=rainbow(Nstars2)[1:length(Galaxy[1,])], pch=pchsun)
	}
	}

	print(round(i/imax,5))
}



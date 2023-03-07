
ww <- 1	# le numéro du poids de la matrice "weights" dont on veut suivre l'évolution

# facteur d'apprentissage "mu"
mu <- 0.05

acti <- function(x){
   return(1/(1+exp(-x)))
}

deacti <- function(x){
   return(acti(x)*(1-acti(x)))
}

# couche C1 (entrée) : 1 neurone
# C2 (sortie) : 1 neurone

# test de plusieurs facteuirs d'apprentissage :
#for(mu in c(0.05)){ # for(mu in seq(0.05,0.05,0.01)){
mu <- 0.05

xxlen <- 1000
xx <- runif(xxlen)*2-1
yy <- as.numeric(xx<(-.5) | xx>.5)

# 1 2 3
# - - -
# 0 0     # bias 
# 1 1 1
#   2



#### réseau 2 "entrainé" :

couches2 <- c(2,2,2)
#couches2 <- c(2,3,3,1)
coucheswithbias2 <- couches2+1
coucheswithbias2[length(coucheswithbias2)] <- coucheswithbias2[length(coucheswithbias2)]-1

neuronesNB2 <- sum(coucheswithbias2)
neuronesID2 <- 1:neuronesNB2

# weights matrix réseau entrainé
weights2 <- matrix(NA,nrow=neuronesNB2, ncol=neuronesNB2)

# attribute random weights
ncount2 <- 1
for(i2 in 2:length(coucheswithbias2)){
   nh2 <- coucheswithbias2[i2-1]
   ni2 <- coucheswithbias2[i2]
   
   weights2[
       ncount2:(ncount2+nh2-1)
      ,(ncount2+nh2):(ncount2+nh2+ni2-1)
   ] <- rnorm(nh2*ni2)
   
   if(i2!=length(coucheswithbias2)){
      weights2[
          ncount2:(ncount2+nh2-1)
         ,(ncount2+nh2)
      ] <- NA
   }
   ncount2 <- ncount2+nh2
}
weights2 <- round(weights2,3)
#print(weights2)

nonaweights2 <- !is.na(weights2)
sumnonaweights2 <- sum(nonaweights2)



###### réseau 1 "entraineur" : 

couches <- c(
	(sumnonaweights2+couches2[1]+couches2[length(couches2)]+neuronesNB2)	# entrée = toutes les infos à propos du réseau entrainé
	, 30	# couche cachée au nombre de neurones arbitraire
	, sumnonaweights	# sortie = nouveaux poids du réseau entrainé
	)
#couches <- c(2,3,3,1)
coucheswithbias <- couches+1
coucheswithbias[length(coucheswithbias)] <- coucheswithbias[length(coucheswithbias)]-1

neuronesNB <- sum(coucheswithbias)
neuronesID <- 1:neuronesNB



# weights matrix réseau entraineur :
weights <- matrix(NA,nrow=neuronesNB, ncol=neuronesNB)

# attribute random weights
ncount <- 1
for(i in 2:length(coucheswithbias)){
   nh <- coucheswithbias[i-1]
   ni <- coucheswithbias[i]
   
   weights[
       ncount:(ncount+nh-1)
      ,(ncount+nh):(ncount+nh+ni-1)
   ] <- rnorm(nh*ni)
   
   if(i!=length(coucheswithbias)){
      weights[
          ncount:(ncount+nh-1)
         ,(ncount+nh)
      ] <- NA
   }
   ncount <- ncount+nh
}
weights <- round(weights,3)
#print(weights)

weightsnona_ww <- which(weights==weights[!is.na(weights)][ww])[1]


# \1 2 . 3 4 5 . 6
# 1
# 2
# .
# 3 
# 4
# 5
# .
# 6



totalError <- c()
nmax <- 1000

wei_followd <- c()
for(n in 1:nmax){
	
	#if(n==1){
	#	x11()
	#	plot.new()
	#	par(usr=c(0,(nmax+1),-10,110))
	#	grid()
	#	axis(1)
	#	axis(2)
	#	box()
	#}

  

   sumError <- rep(0,couches[length(couches)])

#for(i in 1:xxlen){
   
   # à chaque pas, on initialise le réseau entrainé avec des poids, entrées, sorties attendues aléatoires
	
compteur <- 1
while(compteur<=100){	# on fait 100 itérations avant de calculer les poids fictifs 
	
	weights2[nonaweights2] <- rnorm(sumnonaweights2)
	xxi2 <- rnorm(2)		 # .5*sign(rnorm(2))+.5
	yyi2 <- rnorm(2)
   
   # forward prop du signbal dans le réseau entrainé

   neurones_value2 <- rep(0,neuronesNB2)
   ncount2 <- 1 
   for(j2 in 1:(length(coucheswithbias2))){#-1)){
   
      if(j2==1){  # si couche d'entrée alors valeurs d'entrée
         neurones_value2[1:coucheswithbias2[1]] <- c(1,xxi2)
      }else{  # sinon calculer sommes pondérées et fonction d'activation
         for(k2 in (sum(coucheswithbias2[1:(j2-1)])+1):(sum(coucheswithbias2[1:j2]))){
            if(k2==(sum(coucheswithbias2[1:(j2-1)])+1) & j2!=length(couches2)){  # si bias alors 1
               neurones_value2[k2] <- 1
            }else{  # si neurone normal alors acti(somme pondérée)
                  if(j2==2){prevfirst2 <- 1}else{prevfirst2 <- (sum(coucheswithbias2[1:(j2-2)])+1)}
               neurones_value2[k2] <-
               acti(
                 sum(
                   as.vector(
                     weights2[
                       prevfirst2:(sum(coucheswithbias2[1:(j2-1)])), k2
                     ]
                   ) * 
                   neurones_value2[
                     prevfirst2:(sum(coucheswithbias2[1:(j2-1)]))
                   ]
                 )
               )
            }
         }
      }
      ncount2 <- ncount2+coucheswithbias2[j2]
   }
   




 # print(neurones_value2)
   
yyi2old <- neurones_value2[(neuronesNB2-couches2[length(couches2)]+1):neuronesNB2]







   xxi <- c(weights2[nonaweights2],xxi2,yyi2,neurones_value2)
   yyi <-  yyi2

# yyi est ici la la valeur de sortie attendue du réseau entrainé

# créer un tableau pour enregistrer, sur la même ligne, les poids mis à jour et yyiup
# calculer les nouveaux poids mis à jour du réseau entrainé, et les enregistrer
# calculer la sortie mise à jour (yyiup) du réseau entrainé et l'enregistrer
# recommencer un certain nombre de fois ces étapes
# calculer les poids fictifs entre la sortie du réseau entraineur (les nouveaux poids) et yyiup, via une régression multiple multivariée

#refaire une dernière fois le tout (entrées/poids/sorties aléatoires du réseau entrainé, mise à jour des poids par réseau entraineur, calcul sorties mises à jour
# utiliser les poids fictifs pour lancer l'algorithme de rétro-propagation du gradient et mettre à jour les poids du réseau entraineur, une fois.
# tout recommencer jusqu'à convergence des poids du réseau entraineur


# la sortie attendue du réseau entraineur est la modification idéale des sorties du réseau entrainé.
# la sortie calculée du réseau entraineur est la modification réalisée des sorties du réseau entrainé.

# pour calculer les poids fictifs, il n'est pas nécessaire de calculer ces différences. on a juste besoin des nouveaux poids et des nouvelles sorties (yyi2up) du réseau entrainé.



 # calcul des nouveaux poids :

   
   # forward propagation of signal dans le réseau entraineur
   neurones_value <- rep(0,neuronesNB)
   ncount <- 1 
   for(j in 1:(length(coucheswithbias))){#-1)){
   
      if(j==1){  # si couche d'entrée alors valeurs d'entrée
         neurones_value[1:coucheswithbias[1]] <- c(1,xxi)
      }else{  # sinon calculer sommes pondérées et fonction d'activation
         for(k in (sum(coucheswithbias[1:(j-1)])+1):(sum(coucheswithbias[1:j]))){
            if(k==(sum(coucheswithbias[1:(j-1)])+1) & j!=length(couches)){  # si bias alors 1
               neurones_value[k] <- 1
            }else{  # si neurone normal alors acti(somme pondérée)
                  if(j==2){prevfirst <- 1}else{prevfirst <- (sum(coucheswithbias[1:(j-2)])+1)}
               neurones_value[k] <-
               acti(
                 sum(
                   as.vector(
                     weights[
                       prevfirst:(sum(coucheswithbias[1:(j-1)])), k
                     ]
                   ) * 
                   neurones_value[
                     prevfirst:(sum(coucheswithbias[1:(j-1)]))
                   ]
                 )
               )
            }
         }
      }
      ncount <- ncount+coucheswithbias[j]
   }
   # print(neurones_value)

# fin de calcul des nouveaux poids...



# mise à jour des poids du réseau entrainé :

   weights2[nonaweights2] <- neurones_value[(neuronesNB-couches[length(couches)]+1):neuronesNB]


# relancer le réseau entrainé avec ces nouveaux poids
   # forward prop du signal dans le réseau entrainé

   neurones_value2 <- rep(0,neuronesNB2)
   ncount2 <- 1 
   for(j2 in 1:(length(coucheswithbias2))){#-1)){
   
      if(j2==1){  # si couche d'entrée alors valeurs d'entrée
         neurones_value2[1:coucheswithbias2[1]] <- c(1,xxi2)
      }else{  # sinon calculer sommes pondérées et fonction d'activation
         for(k2 in (sum(coucheswithbias2[1:(j2-1)])+1):(sum(coucheswithbias2[1:j2]))){
            if(k2==(sum(coucheswithbias2[1:(j2-1)])+1) & j2!=length(couches2)){  # si bias alors 1
               neurones_value2[k2] <- 1
            }else{  # si neurone normal alors acti(somme pondérée)
                  if(j2==2){prevfirst2 <- 1}else{prevfirst2 <- (sum(coucheswithbias2[1:(j2-2)])+1)}
               neurones_value2[k2] <-
               acti(
                 sum(
                   as.vector(
                     weights2[
                       prevfirst2:(sum(coucheswithbias2[1:(j2-1)])), k2
                     ]
                   ) * 
                   neurones_value2[
                     prevfirst2:(sum(coucheswithbias2[1:(j2-1)]))
                   ]
                 )
               )
            }
         }
      }
      ncount2 <- ncount2+coucheswithbias2[j2]
   }



 # print(neurones_value2)



   
# fin de calculs pour le réseau entrainé


newweights2 <- neurones_value[(neuronesNB-couches[length(couches)]+1):neuronesNB]
yyi2up <- neurones_value2[(neuronesNB2-couches2[length(couches2)]+1):neuronesNB2]




# enregistrer les nouveaux poids et la sortie finale yyiup
# si c'est le 1er enregistrement, créer la matrice "recouts"
if(compteur == 1){
	recouts <- c(
	  newweights2
	, yyi2up-yyi2old
	)
}else{

recouts <- rbind(recouts, c(
	  newweights2
	, yyi2up-yyi2old
	))

}

compteur <- compteur+1
} # fin boucle while(compteur...)


# estimation et récupération des poids fictifs :

lmtest <- (lm(recouts[,(length(newweights2)+1):ncol(recouts)]~recouts[,1:length(newweights2)]))	# régression avec intercept, qui simule le poids du neurone "bias"
#confint(lmtest)
fict.wei <- coefficients(lmtest)[-1,]	# on ne retient pas le bias qui n'existe pas dans le réseau entraineur


#}#############




# il faut maintenant exécuter l'algorithme de rétropropagation des erreurs (<=> deltas) puis mettre à jour les poids du réseau entraineur...


   # backpropagation des deltas du réseau entraineur :
   
	# commencer par calculer les erreurs sur les neurones de sortie du réseau entraineur, à partir des erreurs sur "yyi2up" et des poids fictifs :
	
	# erreur finale :
#		finalerror <- yyi2up-yyi2 # = (yyi2up-yyi2old)-(yyi2-yyi2old)
#					      -------v-------   ------v------
#				                 correction      correction
#					         réalisée         attendue
# peut-être que c'est un autre delta qu'il faut calculer...

		# finalerror <- (yyi2-yyi2old) - (yyi2up-yyi2old)	# attendu - réalisé
		# <=>
		finalerror <- yyi2-yyi2up

j <- length(coucheswithbias)

deltas <- rep(0,neuronesNB)

# on calcule d'abord les deltas de la couche de sortie du réseau entraineur
for(k in (sum(coucheswithbias[1:(j-1)])+1):sum(coucheswithbias[1:j])){
	
	deltas[k] <-
               #deacti(neurones_value[k])*
               #sum(
               #   deltas[nextfirst:nextlast]*
               #   as.vector(weights[k,nextfirst:nextlast])
               #)

		deacti(neurones_value[k])*
               sum(
                  finalerror     *
                  fict.wei[(k-sum(coucheswithbias[1:(j-1)])),]
               )
	
}



   
   
   for(j in length(coucheswithbias):2){
      ncount <- ncount-coucheswithbias[j]
   
      if(j==length(coucheswithbias)){ # si couche de sortie, ne rien faire
        # deltas[(sum(coucheswithbias[1:(j-1)])+1):sum(coucheswithbias)] <-
        #       yyi-neurones_value[(sum(coucheswithbias[1:(j-1)])+1):sum(coucheswithbias)]
            sumError <- sumError+deltas[(sum(coucheswithbias[1:(j-1)])+1):sum(coucheswithbias)]
   
      }else{ # sinon, pour chaque neurone de la couche...
         for(k in (sum(coucheswithbias[1:(j-1)])+1):sum(coucheswithbias[1:j])){
            
            # si couche juste devant celle de sortie, prendre en compte que la couche de sortie n'a pas de bias
            if(j==(length(coucheswithbias)-1)){addedbias <- 0}else{addedbias <- 1}
            
            nextfirst <- sum(coucheswithbias[1:j])+1+addedbias
            nextlast  <- sum(coucheswithbias[1:(j+1)])

            deltas[k] <-
               deacti(neurones_value[k])*
               sum(
                  deltas[nextfirst:nextlast]*
                  as.vector(weights[k,nextfirst:nextlast])
               )
               
         }
   
      }
   }
   
   # print(round(weights,2))



   # forward propagation of weights modifications (réseau entraineur)
   
   #exemple : p1021 <- p1021+mu*n10*d21
   
   matrixweightsid <- which(!is.na(weights))
   
   for(weiid in matrixweightsid){
   
      wei <- weights[weiid]
      matrixweightscolumn <- 1+weiid%/%neuronesNB
      matrixweightsrow <- weiid%%neuronesNB
      
     # weights[weiid] <- wei+mu*neurones_value[matrixweightsrow]*deltas[matrixweightscolumn]

	aaa <- neurones_value[matrixweightsrow]*deltas[matrixweightscolumn]

       weights[weiid] <- wei+mu*sign(aaa)*log(1+abs(aaa))  
   }
   
  



# suivi d'un poids :
wei_followd <- c(wei_followd, weights[weightsnona_ww])
#points(x=n, y=weights[weightsnona_ww], pch=19, col=1)

# suivi de l'évolution de l'erreur relative :
# points(x=rep(n,length(yyi2)), y=100*(yyi2up-yyi2old)/(yyi2-yyi2old), pch=19,col=rainbow(length(yyi2)))

 if(n%%(nmax/100)==0){
	print(n)
	plot(wei_followd~c(1:n))
	}

} # fin boucle "for(n in 1:nmax){" => fin de l'entrainement du réseau entraineur




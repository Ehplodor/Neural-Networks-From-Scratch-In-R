
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
for(mu in c(0.05)){ # for(mu in seq(0.05,0.05,0.01)){


xxlen <- 100
xx <- runif(xxlen)*2-1
yy <- as.numeric(xx<(-.5) | xx>.5)

# 1 2 3
# - - -
# 0 0     # bias 
# 1 1 1
#   2

couches <- c(1,10,1)
# couches <- c(1,7,7,1)
#couches <- c(2,3,3,1)
coucheswithbias <- couches+1
coucheswithbias[length(coucheswithbias)] <- coucheswithbias[length(coucheswithbias)]-1

neuronesNB <- sum(coucheswithbias)
neuronesID <- 1:neuronesNB

# weights matrix
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
nmax <- 100000
for(n in 1:nmax){
	if(n%%(nmax/100)==0){print(n)}

sumError <- rep(0,couches[length(couches)])

#for(i in 1:xxlen){
   
   
   xxi <- runif(1)*2-1 # xx[i]
   yyi <- as.numeric(xxi<(-.5) | xxi>.5) # yy[i]
   
   # forward propagation of signal
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
   
   # backpropagation des deltas :
   
   deltas <- rep(0,neuronesNB)
   
   for(j in length(coucheswithbias):2){
      ncount <- ncount-coucheswithbias[j]
   
      if(j==length(coucheswithbias)){ # si couche de sortie
         deltas[(sum(coucheswithbias[1:(j-1)])+1):sum(coucheswithbias)] <-
               yyi-neurones_value[(sum(coucheswithbias[1:(j-1)])+1):sum(coucheswithbias)]
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

   # forward propagation of weights modifications
   
   #exemple : p1021 <- p1021+.1*n10*d21
   
   matrixweightsid <- which(!is.na(weights))
   
   for(weiid in matrixweightsid){
   
      wei <- weights[weiid]
      matrixweightscolumn <- 1+weiid%/%neuronesNB
      matrixweightsrow <- weiid%%neuronesNB
      
      weights[weiid] <- wei+mu*neurones_value[matrixweightsrow]*deltas[matrixweightscolumn]
   
   }
   
   # matrixweightsid <- which(!is.na(weights))

   # matrixweightscolumn <- 1+matrixweightsid%/%neuronesNB
   # matrixweightsrow <- matrixweightsid%%neuronesNB
      # print(cbind(deltas,matrixweightscolumn,matrixweightsrow))

   # for(k in 1:neuronesNB){
     #  print(k)
	#print(weights[matrixweightsrow[k],matrixweightscolumn[k]])
	#print(neurones_value[matrixweightsrow[k]])
	#print(deltas[matrixweightscolumn[k]])

      # tmp <- weights[matrixweightsrow[k],matrixweightscolumn[k]] +
        #  0.1 *
        #  neurones_value[matrixweightsrow[k]] *
         # deltas[matrixweightscolumn[k]]
# print(paste(k,":",tmp))
  #     weights[matrixweightsrow[k],matrixweightscolumn[k]] <- tmp
   # }
   
      
   
# yyout <- acti(weights[3,6]*1+weights[4,6]*acti(weights[1,4]*1+weights[2,4]*xx)+weights[5,6]*acti(weights[1,5]*1+weights[2,5]*xx))
# plot(yyout~xx)

#}

# enregistrement de l'erreur en sortie
if(n%%(nmax/100)==0){
	totalError <- c(totalError,sumError[1])
}


}

xx <- c()
yy <- c()
out.last <- c()
for(i in 1:200){
   
   xxi <- runif(1)*2-1 # xx[i]
   yyi <- as.numeric(xxi<(-.5) | xxi>.5) # yy[i]
   
	xx <- c(xx,xxi)
	yy <- c(yy,yyi)

   # forward propagation of signal
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
      
   }
   # print(neurones_value)
   out.last <- c(out.last,neurones_value[neuronesNB])
}

x11()
plot(totalError~c(1:length(totalError)), ylim=c(-1,1), main=mu)
grid()

#xx <- xx

#yyout <- acti(weights[3,6]*1+weights[4,6]*acti(weights[1,4]*1+weights[2,4]*xx)+weights[5,6]*acti(weights[1,5]*1+weights[2,5]*xx))
#plot(yyout~xx)

plot(out.last ~ xx, ylim=c(min(c(out.last,yy)),max(c(out.last,yy))))
points(x=xx,y=yy, col="red")

}

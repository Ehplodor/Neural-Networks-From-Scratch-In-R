library(minqa)

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

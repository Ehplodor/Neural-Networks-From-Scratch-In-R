#### Jeu de Go en R

First <- function(a, b){	# first occurence of each element of vector b in vector a
	
	a1 <- c()
	for(bi in b){
		a1 <- c(a1, which(a==bi)[1])
	}
	
	return(a1)
	
}

First1 <- function(a){	# first occurence of each element of vector b in vector a
	b <- unique(a)
	a1 <- c()
	for(bi in b){
		a1 <- c(a1, which(a==bi)[1])
	}
	
	return(a1)
	
}


Last <- function(a, b){	# last occurence of each element of vector b in vector a
	
	a <- c()
	for(bi in b){
		a1 <- c(a1, which(a==bi)[length(which(a==bi))])
	}
	
	return(a1)
	
}

Last1 <- function(a){	# last occurence of each element of vector b in vector a
	b <- unique(a)
	a <- c()
	for(bi in b){
		a1 <- c(a1, which(a==bi)[length(which(a==bi))])
	}
	
	return(a1)
	
}



# Objectif : réaliser un jeu de go dans R

jeudego <- function(nbpts=9){
# étape 1 : coder le jeu de Go :
nbpts <- nbpts


# !!! ces nombres correspondent au nombre de points d'intersections !!! il y a donc n-1 cases par ligne ou colonne !!!

continuer <- TRUE	# la variable de fin de partie
joueurs <- c("Noir","Blanc")


plateau <- matrix(0,nbpts+2,nbpts+2)	# 0 = pas de pion ; 1 à nbpts² = pions par ordre de jeu ; -1 = interdit
plateau[c(1,nbpts+2),] <- -1
plateau[,c(1,nbpts+2)] <- -1




plateau2 <- matrix(1:((nbpts+2)^2),nbpts+2,nbpts+2)	# le numéro d'emplacement de chaque intersection



COLONNE <- matrix(rep(0:(nbpts+1), each=(nbpts+2)),nbpts+2,nbpts+2)
LIGNE <- matrix(rep(0:(nbpts+1), (nbpts+2)),nbpts+2,nbpts+2)




PLAYED <- unique(c(plateau2[1,], plateau2[nbpts+2,], plateau2[,1], plateau2[,nbpts+2]))	# emplacement de chaque coup, par ordre de jeu
onboard <- PLAYED



COULEUR <- rep(-1, length(PLAYED))	# couleur de chaque coup, par ordre de jeu
couleur <- COULEUR


groupe <- rep(0, length(PLAYED))	# groupe d'appartenance de chaque pion, par ordre de jeu
liberte <- rep(2, length(PLAYED))	# nb de liberté de chaque pion, par ordre de jeu


# les pions fait prisonniers chez noir et chez blanc
noir_pris <- c()
blanc_pris <- c()

# Afficher le plateau de jeu initial
print(plateau)

x11()


i <- 0
continuer <- TRUE
while(continuer){
	
	if(i>0){
		onboard_old <- onboard_save
		couleur_old <- couleur_save
	}

	onboard_save <- onboard
	couleur_save <- couleur
	groupe_save <- groupe
	liberte_save <- liberte
	
	

	# quel est le joueur actif ?
	j <- 1+i%%2
	joueur_actif <- joueurs[j]
	
	# l'autre joueur
	k <- 1+(i+1)%%2
	
	plot.new()
	title(main=paste("A",joueur_actif,"de jouer !"), col.main=j, cex.main=2, sub=paste("Noir :",length(noir_pris)," points ;\nBlanc :",length(blanc_pris),"points"), cex.sub=2)
	par( usr=c(0, nbpts+1, 0, nbpts+1) )
	abline(h=1:nbpts, col="grey")
	abline(v=1:nbpts, col="grey")
	box()
	
	#onboardX <- COLONNE[onboard[-c(1:(nbpts*4+4))]]
	#onbardY <- nbpts+1-LIGNE[onboard[-c(1:(nbpts*4+4))]]
	#onboardcouleur <- couleur[onboard[-c(1:(nbpts*4+4))]]

	onboardX <- COLONNE[onboard]
	onbardY <- nbpts+1-LIGNE[onboard]
	onboardcouleur <- couleur
	onboardcouleur[which(onboardcouleur==(-1))] <- 8


	points(x=onboardX, y=onbardY, col=onboardcouleur, pch=19, cex=3)

	Gooo <- FALSE
	while(!Gooo){
		
		Gooo <- TRUE
		
		# quel coup va-il jouer ?
		#print(plateau)
		
		#ANSWER <- readline(paste(joueur_actif,"(",j,") Which row,col ? (ex : 03,05) "))		
		#ROW <- as.numeric(substr(ANSWER,1,2))
		#COL <- as.numeric(substr(ANSWER,4,5))
		
		print(paste("A",joueur_actif,"de jouer !"))
		MOUSE_location <- locator(1)
		COL <- floor(MOUSE_location$x+.5)+1
		ROW <- nbpts+1-floor(MOUSE_location$y+.5)+1
		

		play <- plateau2[ROW,COL]
		
		if(!(play%in%onboard)){
			
			onboard <- c(onboard, play)
			couleur <- c(couleur, j)
			groupe <- c(groupe, max(groupe)+1)
			liberte <- c(liberte, 4-sum(c(play+c(-(nbpts+2),-1,+1,+(nbpts+2)))%in%onboard))
			
			# mise à jour des libertés de chaque pion lié :
			for(m in c(-(nbpts+2),-1,1,(nbpts+2))){	
				linked <- which(onboard==(play+m))
				liberte[linked] <- liberte[linked] - 1
				
				# et fusion des groupes si necessaire
				if(any(couleur[linked]==j)){
					groupe[which(groupe==groupe[length(groupe)])] <- groupe[linked]
				}
			}

			sugroup <- unique(groupe)
			libgroup <- rep(0, length(sugroup))
			coulgroup <- couleur[First1(groupe)]
			
			# ne pas tenir compte des "murs"
			sugroup <- sugroup[-which(coulgroup<=0)]
			#print("sugroup :")
			#print(sugroup)

			libgroup <- libgroup[-which(coulgroup<=0)]
			#print("libgroup :")
			#print(libgroup)

			coulgroup <- coulgroup[-which(coulgroup<=0)]
			#print("coulgroup :")
			#print(coulgroup)

			for(gg in 1:length(sugroup)){
				# calcul du nombre de libertes de chaque groupe
				libgroup[gg] <- sum(liberte[which(groupe==sugroup[gg])])
				
			} 
			#cat(paste("groupe",sugroup,"(",coulgroup,"): lib =",libgroup,"\n"))

			

			if(any(libgroup==0)){
				#print("au moins un groupe à zéro libertés")
				zerolibgroup <- sugroup[which(libgroup==0)]
				zerolibcoul <- coulgroup[which(libgroup==0)]


				# si au moins un groupe à zéro libertés est de la couleur adverse, alors éliminer les groupes adverses
				if( !all(zerolibcoul==j) ){
					
					#print("zerolibcoul")
					#print(zerolibcoul)
					#print(j)
					if(any(zerolibcoul==j)){
						#print("### ne pas supprimer le groupe du pion actif")
						zerolibgroup <- zerolibgroup[-which(zerolibcoul==j)]
					}
					#print("zerolibgroup :")
					#print(zerolibgroup)

				#print(onboard[-c(1:(nbpts*4+4))])
					
					zerolibgrouppions <- onboard[which(groupe%in%zerolibgroup)]
					#print("zerolibgrouppions :")
					#print(zerolibgrouppions)

					zid <- which(onboard%in%zerolibgrouppions)

					
					# rendre 1 liberté aux pions adjacents aux pions fait prisonniers
					for(o in zerolibgrouppions){
						libo <- which(onboard%in%c(o+c(-(nbpts+2),-1,+1,+(nbpts+2))))
						liberte[libo] <- liberte[libo] + 1
					
					}


					PLAYED <- c(PLAYED, play)
					
					#print("zid :")
					#print(zid)
					onboard <- onboard[-zid]

					COULEUR <- c(COULEUR, j)

					couleur <- couleur[-zid]

					groupe <- groupe[-zid] 
					liberte <- liberte[-zid]

					# Si le coup semble valable, mais que cela aboutit à reproduire la situation anté-précédente, le coup est annulé :
					if(i>0){
					
					#cat(paste(onboard,onboard_save,onboard_old,"\n"))
					
					onboard2 <- onboard[order(onboard)]
					couleur2 <- couleur[order(onboard)]
					onboard_old2 <- onboard_old[order(onboard_old)]
					couleur_old2 <- couleur_old[order(onboard_old)]
					
					

					if(all(onboard2==onboard_old2) & all(couleur2==couleur_old2)){
						onboard <- onboard_save
						couleur <- couleur_save
						groupe <- groupe_save
						liberte <- liberte_save
						Gooo <- FALSE
						PLAYED <- PLAYED[-length(PLAYED)]
						COULEUR <- COULEUR[-length(COULEUR)]
					}else{
						plateau[zerolibgrouppions] <- 0
						# les pions fait prisonniers chez noir et chez blanc
						if(j==1){
							noir_pris <- c(noir_pris, zerolibgrouppions)
						}else{
							blanc_pris <- c(blanc_pris, zerolibgrouppions)
						}
					}
					}
					
					
					
					

					
				}else{	# sinon, annuler les changements et recommencer le coup
					
					onboard <- onboard_save
					couleur <- couleur_save
					groupe <- groupe_save
					liberte <- liberte_save
					Gooo <- FALSE
				
				}
			}

			
			
			
		}else{
			if(play==1){
				continuer <- FALSE
				#break()
			}else{Gooo <- FALSE}
		}
		
	
	}
	
	#print(play)
	plateau[play] <- j
	
					
	
	i <- i+1
	
	print(paste("Coup",i,":",joueur_actif,"(",j,") a joué (row,col)=(",ROW,",",COL,")"))
	# cat(paste("Noir :",paste(noir_pris,collapse=", "),";\nBlanc :",paste(blanc_pris,collapse=", "),"\n"))
	cat(paste("Noir :",length(noir_pris)," points ;\nBlanc :",length(blanc_pris),"points\n"))
	# print(plateau)

	#print(onboard[-c(1:(nbpts*4+4))])

	# affichage des libertés :
	# tmp <- matrix(NA,nbpts+2,nbpts+2)
	# tmp[onboard] <- liberte
	# print(tmp)
}
}#fin fonction

jeudego()

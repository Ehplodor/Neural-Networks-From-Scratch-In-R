# jeu de tir 2D
# objectif : ajuster un tie de canon pour toucher une cible au loin.

# il y a : 
# - un canon, en bas à gauche de l'écran
# - une cible, au sol, à droite à une distance donnée.

# on voit la scène de coté.
# on peut ajuster l'angle et la puissance du canon.

# on tire

# la trajectoire du boulet de canon est calculée, puis affichée.
# la cible est atteinte, ou pas (atteinte si le boulet tombe dans la zone de la cible)
# Un message de félicitation est affiché en cas de tir réussi, sinon afficher un message d'échec

# la fonction de tir
tircanon <- function(angle_deg, canon_puissance, dtt=0.1, newplot=TRUE, boulet_masse=1){
	
	dtt <- dtt # seconde
	
	boulet_masse <- boulet_masse	# masse du boulet

	boulet_vitesse <- canon_puissance/boulet_masse
	
	canon_x <- 0	# position du canon au sol
	cible_x <- 10	# position du centre de la cible au sol
	cible_rayon <- 1	# rayon de la cible
	
	

	Grav_accel <- 9.87
	
	if(newplot){
	x11()
	plot.new()
	par(usr=c(-1,12,-1,12))
	axis(1)
	axis(2)
	grid()
	
	segments(x0=cible_x-cible_rayon,x1=cible_x+cible_rayon,y0=0,y1=0,lwd=2)
	
	}

	boulet_x <- canon_x
	boulet_y <- 0.00000001
	boulet_dx_unit <- cos(angle_deg*pi/180)
	boulet_dy_unit <- sin(angle_deg*pi/180)

	# la somme des forces est égale à la masse que multiplie l'accélération
	# bilan des forces sur le boulet :
	#	- en permanence : le poids
	#	- à l'instant initial uniquement : l'impulsion qui lui donne une vitesse
	# on peut donc préciser une vitesse initiale plutot qu'une puissance de canon
	
	boulet_dx <- boulet_dx_unit*boulet_vitesse
	boulet_dy <- boulet_dy_unit*boulet_vitesse

	boulet_ddy <- -Grav_accel	
	
	tt <- 0
	boulet_x_rec <- boulet_x
	boulet_y_rec <- boulet_y
	
	boulet_dy_rec <- boulet_dy

	# on néglige tout frottement

	while(boulet_y>0){
		
		boulet_x <- boulet_x+boulet_dx*dtt
		boulet_y <- boulet_y+boulet_dy*dtt
		
		# print(paste(c(boulet_x,boulet_y)))

		boulet_dy <- boulet_dy+boulet_ddy*dtt

		boulet_x_rec <- c(boulet_x_rec,boulet_x)
		boulet_y_rec <- c(boulet_y_rec,boulet_y)
		boulet_dy_rec <- c(boulet_dy_rec,boulet_dy)
		
		
		
		
	}
	#print(boulet_x_rec)
	#print(boulet_y_rec)
	points(x=boulet_x_rec,y=boulet_y_rec, type="b")
	
	if(boulet_x>(cible_x-cible_rayon) & boulet_x<(cible_x+cible_rayon)){
		text(x=5,y=5,labels="BRAVO")
	}else{text(x=5,y=5,labels="OUPS!")}
	
}

tircanon(angle_deg=45, canon_puissance=10, dtt=0.01, newplot=TRUE, boulet_masse=1)

















### R Othello

ROthello <- function(n=8){
	n <- n
	if(n%in%c(0:2)){n <- 4}
	if(n%%2!=0){n <- n+1}
  x11()
  plot.new()
  par(usr=c(0,n,0,n))
  abline(h=0:n)
  abline(v=0:n)
  box()
  
  board <- matrix(0,n,n)

COLONNE <- matrix(rep(c(.5+c(0:(n-1))), each=n),n,n)
LIGNE <- matrix(rep(c(n-.5-c(0:(n-1))), n),n,n)

  	board[n/2,n/2] <- 1
	board[n/2,n/2+1] <- 2
	board[n/2+1,n/2] <- 2
	board[n/2+1,n/2+1] <- 1

print(board)
	pionsTRUE <- which(board!=0)
	pionsX <- COLONNE[pionsTRUE]
	pionsY <- LIGNE[pionsTRUE]
	pionsCOULEUR <- board[pionsTRUE]
        points(x=pionsX, y=pionsY, col=pionsCOULEUR, pch=19, cex=3)
	title(main="A Noir de jouer !", col.main=1, cex.main=2, sub="Noir : 2 ; Rouge : 2", cex.sub=2)

  

  tour <- 1
	#oldtour <- 0
  while(tour<=(n^2) & any(board==0)){
	
	#if(oldtour!=tour){
		oldboard <- board
	#	oldtour <- tour
		
	#}

    current_player <- 2 - (tour%%2)
    adversaire <- (tour%%2) + 1
    print(paste("Tour", tour, ": joueur", current_player, "..."))

  # x11()
  plot.new()
  par(usr=c(0,n,0,n))
  abline(h=0:n)
  abline(v=0:n)
  box()
	pionsTRUE <- which(board!=0)
	pionsX <- COLONNE[pionsTRUE]
	pionsY <- LIGNE[pionsTRUE]
	pionsCOULEUR <- board[pionsTRUE]
        points(x=pionsX, y=pionsY, col=pionsCOULEUR, pch=19, cex=3)
	
	pionsTRUE <- which(oldboard!=0)
	pionsX <- COLONNE[pionsTRUE]
	pionsY <- LIGNE[pionsTRUE]
	pionsCOULEUR <- oldboard[pionsTRUE]
        points(x=pionsX-.4, y=pionsY+.4, col=pionsCOULEUR, pch=19, cex=.5)
	title(main=paste("Tour",tour,": A",c("Noir","Rouge")[current_player],"de jouer !"), col.main=1, cex.main=2, sub=paste("Noir :",sum(board==1),"; Rouge :",sum(board==2)), cex.sub=2)
	

    
    Gooo <- FALSE
    while(!Gooo){
      coup <- locator(1)
      coupX <- floor(coup$x)+.5
      coupY <- floor(coup$y)+.5

      if(coupX>0 & coupX<n & coupY>0 & coupY<n){ # si le coup est sur le plateau
	boardrow <- n-coupY+.5
	boardcol <- coupX+.5

	if(board[boardrow, boardcol]==0){ # si la case n'est pas occupée
	  
	    roww <- c(-1,1,0,0,-1,-1,1,1)
	    coll <- c(0,0,-1,1,-1,1,-1,1)
	 
   inboard <- which((boardrow+roww)>0 & (boardrow+roww)<=n & (boardcol+coll)>0 & (boardcol+coll)<=n)
	print(inboard)
adv <- 0
for(k in 1:length(inboard)){
	if(board[boardrow+roww[inboard][k],boardcol+coll[inboard][k]]==adversaire){adv <- adv+1}
}
	  if(adv>0){ # si un pion adverse à coté
	    
	    
	    for(i in 1:length(inboard)){
		# print(paste("i :",i))
	      j <- 1
	      currow <- boardrow+roww[inboard][i]*j
	      curcol <- boardcol+coll[inboard][i]*j

	      if(currow>0 & currow<=n & curcol>0 & curcol<=n){

	      curplayer <- board[currow,curcol]
	      
		# print(paste("currow",currow,"; curcol", curcol,": curplayer",curplayer))
		# print(paste("adv:",adversaire))

	        if(curplayer!=current_player){

			print(paste("currow",currow,"; curcol", curcol,": curplayer",curplayer))

	          while(curplayer==adversaire & currow>0 & currow<=n & curcol>0 & curcol<=n){
	      	# 	print(paste("#j :",j))
	      
	            currow <- boardrow+roww[inboard][i]*j
	            curcol <- boardcol+coll[inboard][i]*j
	            if(currow>0 & currow<=n & curcol>0 & curcol<=n){
		      curplayer <- board[currow,curcol]
		      print(paste("# currow",currow,"; curcol", curcol,": curplayer",curplayer))
	              j <- j+1
		    }
	          } # fin "while(..."
	      
	          if(curplayer==current_player){
		   # print("###")
		   # print(paste(current_player,adversaire,":",curplayer))
		    board[boardrow,boardcol] <- current_player
		    Gooo <- TRUE
		    j <- 1
	            currow <- boardrow+roww[inboard][i]*j
	            curcol <- boardcol+coll[inboard][i]*j
	            curplayer <- board[currow,curcol]
	        
		     print(paste("# # currow",currow,"; curcol", curcol,": curplayer",curplayer))

	            while(curplayer==adversaire & currow>0 & currow<=n & curcol>0 & curcol<=n){
	          
	      
	              
	              j <- j+1
	  	      board[currow,curcol] <- current_player
		      print(paste("pion",currow,curcol,"retourné"))
		      currow <- boardrow+roww[inboard][i]*j
	              curcol <- boardcol+coll[inboard][i]*j
	              curplayer <- board[currow,curcol]
		
		
		
		
		
		    } # fin "while(..."
	        
		  } # fin "if(curplayer==current_player){"
		
		
	      } # fin "if(curplayer!=current_player){"
	      } # fin "if(currow>0 & currow<=n & curcol>0 & curcol<=n){"
	    } # fin "for(i in 1:8){"
	    
	    
	    
	    
	    
	    
	    
	  }
	  
	  
	  
	  
	}






      }
    }
    
    
  plot.new()
  par(usr=c(0,n,0,n))
  abline(h=0:n)
  abline(v=0:n)
  box()
	pionsTRUE <- which(board!=0)
	pionsX <- COLONNE[pionsTRUE]
	pionsY <- LIGNE[pionsTRUE]
	pionsCOULEUR <- board[pionsTRUE]
        points(x=pionsX, y=pionsY, col=pionsCOULEUR, pch=19, cex=3)
	
	pionsTRUE <- which(oldboard!=0)
	pionsX <- COLONNE[pionsTRUE]
	pionsY <- LIGNE[pionsTRUE]
	pionsCOULEUR <- oldboard[pionsTRUE]
        points(x=pionsX-.4, y=pionsY+.4, col=pionsCOULEUR, pch=19, cex=.5)
	title(main=paste("Tour",tour+1,": A",c("Noir","Rouge")[current_player],"de jouer !"), col.main=1, cex.main=2, sub=paste("Noir :",sum(board==1),"; Rouge :",sum(board==2)), cex.sub=2)
	
  
    
    
  tour <- tour+1  
    
  }
  
  
  
  
  
  
  
  
  
  
  
}
ROthello()

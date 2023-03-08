identifiant <- setRefClass("identifiant",
	  fields = 	list(
					  id = "numeric"
				)
	, methods = list(
					  initialize = function(..., id=1){
						callSuper(..., id=id)
					  }
					, nextID = function(){
						id <<- id + 1
					  }
				)
)
identifiant()

a <- identifiant()
a$nextID()
a$id



connection <- setRefClass("connection",
	  fields = 	list(
					  id = "numeric"
					, from = "numeric"
					, to = "numeric"
					, nStep = "numeric"
					, curStep = "numeric"
					, charge = "numeric"
				)
	, methods = list(
					initialize = function(..., id=-1, from=-1, to=-1, nStep=-1, curStep=-1, charge=0){
						callSuper(..., id=id, from=from, to=to, nStep=nStep, curStep=curStep, charge=charge)
					}
					,
					oneStep = function(){
						if(curStep == nStep){
							curStep <<- -1
						}else{
							curStep <<- curStep + 1
						}
					}
					,
					isFiring = function(){
						if(curStep == -1){
							curStep <<- 0
							return(TRUE)
						}else{
							return(FALSE)
						}
					}
					,
					
					
				)
)
connection()
connection(id=1,from=2,to=3,nStep=4,curStep=5)
connection(id=1,to=3,nStep=4)







neurone <- setRefClass("neurone",
	  fields = 	list(
					  id = "numeric"
					, connections = "list"
					, nbconnections = "numeric"
					, charge = "numeric"
				)
	, methods = list(
					initialize = function(..., id=-1, connections=list(), nbconnections=0, charge=0){
						callSuper(..., id=id, connections=connections, nbconnections=nbconnections, charge=charge)
					}
					,
					addconnection = function(){
						nbconnections <<- nbconnections + 1
						a$nextID()
						connections[[nbconnections]] <<- connection(id=a$id, from=id)
					}
					,
					chargeAdd = function(x){
						charge <<- charge + x
					}
					,
					chargeSet = function(x){
						charge <<- x 
					}
					,
					isRefractaire = function(){
						if(charge < 0){
							return(TRUE)
						}else{
							return(FALSE)
						}
					}
					# ,
					# oneStep
				)
)
neurone()
neurone(id=1)



brain <- setRefClass("brain",
	  fields = 	list(
					  id = "numeric"
					, neurones = "list"
					, nbneurones = "numeric"
					, input = "numeric"
					, output = "numeric"
				)
	, methods = list(
					initialize = function(..., id=-1, neurones=list(), nbneurones=0, input=-1, output=-1){
						callSuper(..., id=id, neurones=neurones, nbneurones=nbneurones, input=input, output=output)
					}
					,
					addneurone = function(){
						nbneurones <<- nbneurones + 1
						a$nextID()
						neurones[[nbneurones]] <<- neurone(id=a$id)
					}
					,
					setCharge = function(x,y){
						for(i in 1:length(x)){
							neurones[[x[i]]]$chargeSet(y[i])
						}
					}
					,
					addCharge = function(x,y){
						for(i in 1:length(x)){
							neurones[[x[i]]]$chargeAdd(y[i])
						}
					}
					,
					getCharge = function(x){
						mydf <- data.frame(neurone=0, charge=0)
						if(x==0){
							x <- 1:nbneurones
						}
						for(i in 1:length(x)){
							mydf[i,] <- c(neurones[[i]]$id, neurones[[i]]$charge)
							
						}
						
						return(mydf)
					}
					,
					addConnection= function(x){
						neurones[[x]]$addconnection()
					}
					,
					oneStep = function(){
						
					}
				)
)
brain()
brain(id=1)

bb <- brain()
bb$addneurone()
bb$addneurone()
bb$addneurone()
bb$addneurone()
bb$addneurone()
bb$addneurone()

bb$neurones[[1]]$addconnection()
bb$neurones[[1]]$addconnection()
bb$neurones[[1]]$addconnection()
bb$neurones[[1]]$addconnection()
bb$neurones[[1]]$addconnection()

bb$neurones[[1]]$id

bb$setCharge(x=c(1,3,5),y=c(2,4,8))

bb.getCharge(0)
bb$getCharge(0)

# > bb$neurones[[1]]$isRefractaire()
# [1] FALSE
# > bb$neurones[[1]]$charge
# [1] 0
# > bb$neurones[[1]]$chargeSet(-1)
# > bb$neurones[[1]]$charge
# [1] -1
# > bb$neurones[[1]]$isRefractaire()
# [1] TRUE
# > bb$neurones[[1]]$chargeAdd(2)
# > bb$neurones[[1]]$charge
# [1] 1
# > bb$neurones[[1]]$isRefractaire()
# [1] FALSE


# ajouter 
# - initialisation aléatoire des connections.
# - exécution d'une étape de "calcul"
# - initialisation des neurones input, de leur charges / de certaines charges, sous l'effet de "l'extérieur"
# propagation du signal
# - lecture des output pour récupérer la "réponse" du brain.













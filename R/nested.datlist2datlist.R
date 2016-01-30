
##########################################################
# converts a nested list of multiply imputed
# datasets into a list of multiply imputed datasets
nested.datlist2datlist <- function(datlist){
		Nimp <- c( length(datlist) , length(datlist[[1]] ) )
		names(Nimp) <- c("Between" , "Within")
		PP <- Nimp[1] * Nimp[2]
		datlist0 <- as.list( 1:PP )
		vv <- 1
		for (bb in 1:Nimp[1] ){
			for (ww in 1:Nimp[2] ){
					datlist0[[vv]] <- datlist[[bb]][[ww]]
					vv <- vv + 1
								}
							}
		attr(datlist0,"Nimp") <- Nimp
		class(datlist0) <- "datlist"
		return(datlist0)						
				}
##########################################################				

####################################################
# datlist -> nested.datlist
datlist2nested.datlist <- function(datlist, Nimp){
		PP <- Nimp[1] * Nimp[2]
		datlist1 <- as.list( 1:Nimp[1] )
		datlist2 <- as.list( 1:Nimp[2] )		
		vv <- 1
		for (bb in 1:Nimp[1] ){		
			for (ww in 1:Nimp[2] ){
					datlist2[[ww]] <- datlist[[vv]]
					vv <- vv + 1
								}
			datlist1[[bb]] <- datlist2													
							}
		attr(datlist1,"Nimp") <- Nimp
		class(datlist1) <- "nested.datlist"
		return(datlist1)						
				}
#####################################################				
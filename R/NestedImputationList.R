
#######################################################
# list of nested of multiply imputed datasets
NestedImputationList <- function(datasets){
	  Nimp <- c( length(datasets) , length(datasets[[1]] ) )	
	  names(Nimp) <- c("Between" , "Within")
	  rval<-list(imputations=datasets, "Nimp"=Nimp)
	  class(rval)<-"NestedImputationList"
	  rval
	}
#################################################################

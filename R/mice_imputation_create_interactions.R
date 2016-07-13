


################################################
# create interactions
mice_imputation_create_interactions <- function (y_, xobs_, xall_, 
	index_int_, min_int_cor_, maxcols_ )
{ 
	base::.Call("create_interactions_cpp", 
		y_, xobs_, xall_, index_int_, min_int_cor_, maxcols_ , 
		PACKAGE = "miceadds")
}
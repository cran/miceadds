


#*********************************************************************************
# extract list argument
mice_imputation_extract_list_arguments <- function( micearg , vname , miceargdefault ){
    # micearg   ... name of mice argument
    # vname     ... variable name
    # miceargdefault    ... default for this variable
    if( base::is.list(micearg) ){
        if ( ! base::is.null(micearg[[vname]] ) ){
            micearg <- micearg[[vname]]
        } else { 
			micearg <- miceargdefault 
		}                      
    }
    if ( base::is.null(micearg) ){
		micearg <- miceargdefault 
	}       
    base::return( micearg )
}
#*****************************************************************************

#--- deprecated function
.extract.list.arguments <- mice_imputation_extract_list_arguments




#################################################
# load packages or install some packages
# if they are needed.
library_install <- function( pkg , ... ){
	PP <- base::length(pkg)
	for (vv in 1:PP){
		pp <- pkg[vv]
		ab1 <- base::do.call( what = base::require , 
					args = base::list( package=pp , quietly=TRUE ) )
		if ( ! ab1 ){
			utils::install.packages( pp , ...)
		}
		base::do.call( base::require , base::list( package=pp ) )
	}
}
################################################			
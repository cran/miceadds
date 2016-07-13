
##################################################
# back transform fisher correlations
fisher2cor <- function(z){ 
	c1 <- ( base::exp(2*z) - 1 )/ ( base::exp(2*z) + 1 )
	base::return(c1)
}
#####################################################

#####################################################
# derivative of fisher to cor
fisher2cor.D1 <- function(z, h = .001){
	( fisher2cor(z+h) - fisher2cor(z) ) / h
		}
######################################################
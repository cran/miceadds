

#################################################
# create design matrices for waldtest
create.designMatrices.waldtest <- function( pars , k ){
        NP <- base::length(pars)
        Cdes <- base::matrix( 0 , nrow=k , ncol=NP)
        base::colnames(Cdes) <- pars
        rdes <- base::rep(0,k)
        res <- base::list( Cdes = Cdes , rdes=rdes )
        base::return(res)
}
######################################################

##################################################
# converts a dataframe in longformat (produced
# in jomo) into a list of datasets

jomo2datlist <- function( jomo.dataframe , variable="Imputation" ){
    dat <- jomo.dataframe
    M1 <- base::max( base::unique( dat$Imputation ) )
    datlist <- base::as.list( 1:M1 )
    ind <- base::which( base::colnames(dat) == variable )
    for ( mm in 1:M1){
        datlist[[mm]] <- dat[ dat$Imputation == mm , - ind ]
                    }
    base::return(datlist)
}


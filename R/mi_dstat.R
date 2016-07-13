################################################
# d effect size for missingness indicators
mi_dstat <- function(dat){
	resp <- is.na(dat)
	# means of missing data
	miss_vars <- base::colnames(resp)[ base::colMeans( resp ) > 0 ]
	MV <- base::length(miss_vars)
	V <- base::ncol(dat)

	dstat <- base::matrix( 0 , nrow=MV , ncol=V )
	base::rownames(dstat) <- miss_vars
	base::colnames(dstat) <- base::colnames(dat)

	for (vv in 1:MV){
		# vv <- 5
		dat_vv0 <- dat[  resp[ , miss_vars[vv]  ] , , drop=FALSE ]
		dat_vv1 <- dat[ ! resp[ , miss_vars[vv]  ] , , drop=FALSE ]    
		m0 <- base::colMeans( dat_vv0 , na.rm=TRUE )
		m1 <- base::colMeans( dat_vv1 , na.rm=TRUE )    
		sd0 <- base::apply( dat_vv0 , 2 , stats::sd , na.rm=TRUE)
		sd1 <- base::apply( dat_vv1 , 2 , stats::sd , na.rm=TRUE)    
		d <- (m0-m1) / base::sqrt( ( sd0^2 + sd1^2 ) / 2 )
		dstat[vv,] <- d
	}
	base::return(dstat)
}
#####################################################			
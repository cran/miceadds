
################################################
# draw residuals
mice_multilevel_imputation_draw_residuals <- function(predicted, sigma ){
	N0 <- base::length(predicted)
	imp <- predicted + stats::rnorm(N0 , mean=0 , sd = sigma)	
	base::return(imp)
}	
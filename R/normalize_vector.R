
##########################################
# normalize vector to sum of length x
normalize_vector <- function(x)
{
	x  <- base::length(x) * x / base::sum(x)
	base::return(x)
}
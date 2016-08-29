
file_path <- function( dir , file)
{
	if ( base::is.null(dir) ){
		p1 <- file
	} else {
		p1 <- base::file.path(dir, file)
	}
	base::return(p1)
}
cxxfunction.copy <- function( cppfct , name ){
    base::requireNamespace("inline")
    g1 <- inline::getDynLib(cppfct)
    cppname <- base::gsub( "\\.dll" , "\\.cpp" ,  g1[["path"]] )
    h1 <- base::readLines( cppname )
    tempname <- g1[["name"]]
    h1 <- base::gsub( tempname , name , h1 )
	h1 <- base::c( base::paste0( "//  Code created: " , base::Sys.time() ) , "" , h1 )
	name1 <- base::paste0( base::tolower(name) , ".cpp" )
    base::writeLines( h1 , name1 )
	crlrem( filename1=name1 , filename2=name1 )
            }

######################################################
# remove line endings
crlrem <- function( filename1 , filename2 ){
    filename <- filename1
    con <- base::file(filename, "rb")
    bin <- base::readBin(con, base::raw(), 100000)
    bin <- bin[ base::which(bin != "0d") ]
    base::close(con)
    base::Sys.sleep(1)
    con <- base::file(filename2, "wb")
    base::writeBin(bin, con)
    base::close(con)
        }

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
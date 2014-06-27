

####################################################
# R session info
Rsessinfo <- function(){
    si <- Sys.info()
    si2 <- sessionInfo()
    paste0( si2$R.version$version.string , " " , si2$R.version$system 
             , " | nodename = " , si["nodename"] , " | login = " , si["login"] )
            }
####################################################
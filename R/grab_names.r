#' Grab Names 
#' 
#' This function gets fleet and index names from the original ASAP.DAT file.
#' @param asap.name Base name of original dat file (without the .dat extension)
#' @param asap name of the variable that read in the asap.rdat file
#' @return list of fleet names and index names
#' @export

GrabNames <- function(asap.name,asap){
  my.names <- list()
  # in case the file was created outside the GUI
  my.names$fleet.names <- paste("FLEET-",1:asap$parms$nfleets, sep="")
  my.names$index.names <- paste("INDEX-",1:asap$parms$nindices, sep="")
  gg1<-shell(paste("dir ", asap.name,".dat", sep=""), intern=T, mustWork=NA )
  gg2 <- which(gg1=="File Not Found")
  if (length(gg2)==0 )  {
    datfile <- readLines(con = paste(asap.name,".dat", sep=""))
    nlines <- length(datfile)
    nfinis <- nlines-asap$parms$nfleets-asap$parms$navailindices-3
    if (datfile[nfinis] == "###### FINIS ######"){
      my.names$fleet.names <- substr(datfile[(nfinis+2):(nfinis+2+asap$parms$nfleets-1)],3,100)
      avail.index.names <- substr(datfile[(nfinis+3+asap$parms$nfleets):(nlines-1)],3,100)
      my.names$index.names <- avail.index.names[asap$initial.guesses$index.use.flag==1]
    }  # end if-test for nfinis
  } # end if-test for length(gg2)
  
  return(my.names)
}

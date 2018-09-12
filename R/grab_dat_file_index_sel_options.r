#' Grab Dat File Index Selectivity Options 
#' 
#' This function gets index selectivity options from the original ASAP.DAT file.
#' @param asap.name Base name of original dat file (without the .dat extension)
#' @param asap name of the variable that read in the asap.rdat file
#' @return list of fleet names and index names
#' @export

GrabDatFileIndexSelOptions <- function(asap.name,asap){
  dat.file.index.sel.options <- NULL
  gg1<-shell(paste("dir ", asap.name,".dat", sep=""), intern=T, mustWork=NA )
  gg2 <- which(gg1=="File Not Found")
  if (length(gg2)==0 ){
    datfile <- readLines(con = paste(asap.name,".dat", sep=""))
    nlines <- seq(1, length(datfile))
    myval <- "# Index Selectivity Options 1=by age, 2=logisitic, 3=double logistic"
    index.sel.options.line <- nlines[datfile == myval]
    if (length(index.sel.options.line) > 0){
      dat.file.all.index.sel.options <- scan(file = paste(asap.name,".dat", sep=""),
                                             n = asap$parms$navailindices, 
                                             skip = index.sel.options.line)
      dat.file.index.sel.options <- dat.file.all.index.sel.options[asap$initial.guesses$index.use.flag==1]
    }
  } # end if-test for length(gg2)
  
  return(dat.file.index.sel.options)
  
}
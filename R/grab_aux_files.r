#' Grab Auxilliary Files
#' 
#' This function reads in values from the *.std, *.par, and *.cor files.
#' @param wd directory where ASAP run is located
#' @param asap.name Base name of original dat file (without the .dat extension)
#' @param asap name of the variable that read in the asap.rdat file
#' @param fleet.names names of fleets 
#' @param index.names names of indices
#' @return list of summary statistics about run (e.g. # parameters, correlation matrix, max gradient)
#' @export

GrabAuxFiles <- function(wd,asap.name,asap,fleet.names,index.names) {

  aux.list <-  list("npar"=-999, "asap.cor.names"=NA, "asap.cor.mat"=NA,
                    "asap.std"=NA, "max.grad"=-999, "F.rep"=NA, "Tot.B"=NA , "SSB"=NA, 
                    "Expl.B"=NA, "recr"=NA, "asap.name"=asap.name )
  
  if (file.exists(paste0(wd,"\\",asap.name,".std"))){
    # Read in std file from admb
    asap.std <- read.table(paste0(wd,"\\",asap.name, ".std"), header = F, skip=1,sep = "") 
    names(asap.std) <- c("index", "name", "value", "stdev" )
    
    years <- seq(asap$parms$styr, asap$parms$endyr)
    
    # Read in cor file from admb
    ncol.cor <- dim(asap.std) [1]
    asap.cor <- read.table(paste(wd,"\\",asap.name, ".cor",sep=""), header = F, skip=2,sep = "", 
                           col.names=c("index", "name", "value", "stdev", seq(1,ncol.cor)), fill=T) 
    asap.cor.mat <- as.matrix(asap.cor[,5:length(asap.cor[1,])])
    asap.cor.names <- as.character(as.vector(asap.cor[,2]) )
    levels.cor.names <- unique(asap.cor.names)
    F.rep <- which(asap.cor.names=="Freport") 
    asap.cor.names[F.rep] <- paste(asap.cor.names[F.rep],years, sep=".")  
    Tot.B <- which(asap.cor.names=="TotJan1B") 
    asap.cor.names[Tot.B] <- paste(asap.cor.names[Tot.B],years, sep=".")
    SSB <- which(asap.cor.names=="SSB")  
    asap.cor.names[SSB] <- paste(asap.cor.names[SSB],years, sep=".")
    Expl.B <- which(asap.cor.names=="ExploitableB")  
    asap.cor.names[Expl.B] <- paste(asap.cor.names[Expl.B],years, sep=".")
    recr <- which(asap.cor.names=="recruits") 
    asap.cor.names[recr] <- paste(asap.cor.names[recr],years, sep=".")
    
    N.yr1 <- which(asap.cor.names=="log_N_year1_devs")   
    asap.cor.names[N.yr1] <- paste(asap.cor.names[N.yr1],"Age",seq(2,asap$parms$nages), sep=".")
    
    recr.devs <- which(asap.cor.names=="log_recruit_devs")   
    asap.cor.names[recr.devs] <- paste(asap.cor.names[recr.devs],years, sep=".")
    Fmult.yr1 <- which(asap.cor.names=="log_Fmult_year1")   
    asap.cor.names[Fmult.yr1] <- paste(asap.cor.names[Fmult.yr1],fleet.names, sep=".")
    Fmult.devs <- which(asap.cor.names=="log_Fmult_devs")   
    asap.cor.names[Fmult.devs] <- paste(asap.cor.names[Fmult.devs],paste(fleet.names, rep(years[2:length(years)], each=asap$parms$nfleets),  sep="."), sep=".")
    q.yr1 <- which(asap.cor.names=="log_q_year1")   
    asap.cor.names[q.yr1] <- paste(asap.cor.names[q.yr1],index.names, sep=".")
    
    q.devs <- which(asap.cor.names=="log_q_devs")   
    #asap.cor.names[q.devs] <- paste(asap.cor.names[q.devs],index.names, sep=".")
    
    if (length(q.devs)>0) asap.cor.names[q.devs] <- paste(asap.cor.names[q.devs],years, sep=".")
    
    diag(asap.cor.mat) <- rep(NA, ncol.cor)
    
    asap.grad <- readLines(paste0(wd, "\\", asap.name, ".par"),n=1) 
    par.split<- unlist(strsplit(asap.grad, " "))
    max.grad <- par.split[length(par.split)]
    npar <- as.numeric(par.split[ 6])
    aux.list <-  list("npar"=npar, "asap.cor.names"=asap.cor.names, "asap.cor.mat"=asap.cor.mat,
                      "asap.std"=asap.std, "max.grad"=max.grad, "F.rep"=F.rep[1], "Tot.B"=Tot.B[1], 
                      "SSB"=SSB[1], "Expl.B"=Expl.B[1] , "recr"=recr[1], "asap.name"=asap.name )
  }

  return(aux.list)
}


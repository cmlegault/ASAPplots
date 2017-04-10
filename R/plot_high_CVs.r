#' Plot parameters with unusually high or low CVs
#' 
#' Identify parameters with CVs < 0.05 or CVs > 0.55.
#' @param a1 list file produced by grab.aux.files function
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @return matrix of parameters that meet criteria
#' @export

PlotHighCVs <- function(a1,save.plots,od,plotf) {

  npar <- a1$npar
  if (npar != -999){
    # drop last 5 params (or only if steepness is fixed at 1?)
    asap.std <- a1$asap.std 
    nCV1 <- length(asap.std[,1])
    asap.std <- asap.std[c(-(nCV1-4):(-nCV1) ),]
    nCV <- length(asap.std[,1])
    nCV.par <-as.numeric(a1$npar)
    
    CVmat <- matrix(NA, nrow=nCV, ncol=(length(asap.std[1,])+1)       )
    CV.vec <- rep(NA, length(asap.std[1,])  )
    log.pars <- which(substr(asap.std$name,1,3)=="log")
    
    for (p in 1:nCV) {
      
      CV.vec[p] <- asap.std[p,4]/asap.std[p,3]
      if (substr(asap.std$name[p],1,3)=="log")  CV.vec[p] <- sqrt( exp(asap.std[p,4]) -1)
      
    }  #end p loop
    CVmat <- cbind(asap.std, "CV"= CV.vec)
    trouble.pars1 <- CVmat[CVmat$CV<0.05 | CVmat$CV>0.55,]
    
    if (length(trouble.pars1[,1]>0) ) {
      trouble.pars <- trouble.pars1[trouble.pars1$index<(nCV.par+1),]
      cex.par <- rep(1, length(trouble.pars[,1])  )
      pch.par <- rep(4, length(trouble.pars[,1])  )
      pch.par[which(trouble.pars$CV<0.05)]<- 8
      cex.par[which(trouble.pars$CV<0.05)]<- 1.5
      trouble.pars2 <- trouble.pars
      trouble.pars2$CV[which(trouble.pars$CV>1)]<- 1  #truncate CV at 1 so you can read plot
      
      write.csv(trouble.pars, file=paste(od, "CV.Check.Params.",asap.name,".csv",sep=""),   row.names=F)
      write.csv(CVmat, file=paste(od, "CV.All.Params.",asap.name,".csv",sep=""),   row.names=F)
      
      plot(trouble.pars$index, trouble.pars$CV, type='p', pch=pch.par, col='red', cex=cex.par,
           xlab="Parameter number", ylab="CV (parameter estimate)", ylim=c(0,1.05) )
      
      if (save.plots) savePlot(paste(od, "CV.Check.Params.",plotf, sep=""), type=plotf) 
    }  #end check for length(trouble.pars)>0

  } else { # if npar == -999
    
    plot(1:10,1:10,type='n',axes=FALSE,xlab="",ylab="")
    text(5,5,"Problem with run, no .std file")
    
  } # end if npar check
  
  return()
}     

#' Standard SR plot with Beverton-Holt curve 
#' 
#' Scatterplot of SSB, R with underlying stock recruitment relationship (x-axis extends to S0).
#' @param asap name of the variable that read in the asap.rdat file
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @export

PlotSRpredLine <- function(asap,save.plots,od,plotf){
  par(mfrow=c(1,1) )
  
  a<- asap$SR.parms$SR.alpha 
  b<- asap$SR.parms$SR.beta
  S0.yr <- asap$SR.annual.parms[,2]
  ssb <- asap$SSB
  recr <- asap$N.age[,1]
  years <- seq(asap$parms$styr, asap$parms$endyr)
  nyears <- length(years)
  SR <- matrix(NA, (nyears-1), 3)
  SR[,1] <- years[1:(nyears-1)]
  SR[,2] <- ssb[1:(nyears-1)]
  SR[,3] <- recr[2:nyears]
  
  seq.ssb= seq(0, 1.1*max(S0.yr, SR[,2]), length.out=300)
  pred.r = a*seq.ssb/(b+seq.ssb)
  
  plot( SR[,2], SR[,3], type='p', col='black', pch=19,
        xlab="SSB (mt)", ylab="Recruits (000s)", 
        ylim=c(0, 1.1*max(SR[,3])), xlim=c(0,1.2*max(SR[,2], S0.yr))  )    
  
  lines(seq.ssb , pred.r,  col='red', lwd=2) 
  
  
  
  if (save.plots==T) savePlot(paste0(od, "S_R.Pred.Line.",plotf), type=plotf)
  return()
}   # end function

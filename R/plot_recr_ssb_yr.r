#' Scatterplot of SSB, R with 2-digit year  
#' 
#' Standard stock-recruitment plot with years labeled. Also creates R/SSB time series plot.
#' @param asap name of the variable that read in the asap.rdat file
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @export
 
PlotRecrSSByr <- function(asap,save.plots,od,plotf){
  
  par(mfrow=c(1,1) )
  
  ssb <- asap$SSB
  recr <- asap$N.age[,1]
  years <- seq(asap$parms$styr, asap$parms$endyr)
  nyears <- length(years)
  SR <- matrix(NA, (nyears-1), 4)
  SR[,1] <- years[1:(nyears-1)]
  SR[,2] <- ssb[1:(nyears-1)]
  SR[,3] <- recr[2:nyears]
  SR[,4] <- SR[,3] / SR[,2]  # R per SSB
  
  yr.text=substr(SR[,1],3,4)
  npts<-length(years)-1
  
  plot( SR[,2], SR[,3], type='n', col='black',
        xlab="SSB ", ylab="Recruits ", 
        ylim=c(0, 1.1*max(SR[,3])), xlim=c(0,1.2*max(SR[,2]))  )    
  
  points (SR[npts,2], SR[npts,3], pch=19, col="#ffaa22", cex=2.5)
  text(SR[,2], SR[,3], yr.text[1:(nyears-1)], cex=0.9, col="black")  
  
  if (save.plots==T) savePlot( paste(od, "S_R.Yr.",plotf, sep=""), type=plotf)
  
  plot(SR[,1],SR[,4], type='o', col='black', pch=16, 
       xlab="Year", ylab="R/SSB")
  abline(h=mean(SR[,4]), lty=2)
  if(save.plots==TRUE) savePlot(paste0(od, "RpSSB_time_series.",plotf), type=plotf)
  
  return()
  
}  #end function

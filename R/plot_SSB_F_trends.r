#' Plot SSB and F time series
#' 
#' Two measures of F plotted (max F and Freport) and plotted separately below SSB time series.
#' @param asap name of the variable that read in the asap.rdat file
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @export

PlotSSBFtrend<-function(asap,save.plots,od,plotf){
  par(mfrow=c(2,1), mar=c(4,4,2,2) )
  
  ssb <- asap$SSB
  fmult <- apply(asap$F.age,1, max) 
  f.report <- asap$F.report
  years<-  seq(asap$parms$styr, asap$parms$endyr)
  
  
  
  plot(years, ssb, type='l', lwd=2, col='blue', xlab="Year", ylab="SSB", 
       ylim=c(0,1.05*max(ssb)) )
  
  plot(years, f.report, type='l', lwd=2, col='black', xlab="Year", ylab="Fishing Mortality", 
       ylim=c(0,1.25*max(fmult, f.report)) )
  lines(years, fmult, col='red', lty=2, lwd=2)
  
  legend('topleft', horiz=T, legend=c('F.report', 'F.full'), col=c('black', 'red'),
         lwd=c(2,2), lty=c(1,2), cex=0.85)
  
  if (save.plots==T) savePlot(paste0(od, "SSB.F.Timeseries.Line.",plotf), type=plotf)
  return()
}  #end function

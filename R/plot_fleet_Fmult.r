#' Plot fleet F multipliers 
#' 
#' Line plots of Fmult (maximum F at age) by fleet, remember that selectivity can change over time.
#' @param asap name of the variable that read in the asap.rdat file
#' @param fleet.names names of fleets 
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param liz.palette color definitions
#' @export

PlotFleetFmult <- function(asap,fleet.names,save.plots,od,plotf,liz.palette){
  
  par(mfrow=c(1,1))
  
  years<-  seq(asap$parms$styr, asap$parms$endyr)
  nyears <- length(years)
  nfleets = asap$parms$nfleets
  
  Fmax<-max(asap$fleet.Fmult)
  
  for (i in 1:nfleets) {
    
    if (i==1) plot(years, asap$fleet.Fmult[i,], xlab="Year", ylab="Fmult by fleet", ylim=c(0,1.2*Fmax),
                   type='l', lty=1, lwd=2)
    if (i>1) lines(years, asap$fleet.Fmult[i,],lty=i, lwd=2, col=liz.palette[i] )
  }
  
  leg.names <- fleet.names
  legend('topleft', legend=leg.names, col=liz.palette[1:nfleets],lwd=rep(2, nfleets),
         lty=seq(1, nfleets), horiz=T, bty='n' ) 
  
  if (save.plots==T) savePlot(paste0(od, 'Fmult.by.fleet.', plotf), type=plotf)
  
}   # end function


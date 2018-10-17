#' Plot all biomass types
#' 
#' Comparison of Total, SSB, and Exploitable Biomass calculated on Jan-1. The Total B is direct from ASAP, while exploitable B and SSB are calculated for this comparison.
#' @param asap name of the variable that read in the asap.rdat file
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @export

PlotAllBiomassTypes<-function(asap,save.plots,od,plotf){
  par(mfrow=c(1,1) )
  
  sel.aa <- asap$F.age/apply(asap$F.age,1, max) 
  exp.b <- apply(asap$N.age*asap$WAA.mats$WAA.jan1*sel.aa,1,sum)
  #exp.b <- asap$exploitable.B
  ssb.jan1 <- apply(asap$N.age*asap$maturity*asap$WAA.mats$WAA.jan1,1,sum)
  tot.b <-asap$tot.jan1.B
  years<-  seq(asap$parms$styr, asap$parms$endyr)
  
  
  plot(years, tot.b, type='l', col='maroon', lwd=2, lty=1, xlab="Year", ylab="Biomass",
       ylim=c(0, 1.1*max(exp.b, ssb.jan1, tot.b)) )
  lines(years, ssb.jan1, col='blue', lwd=2, lty=2)
  lines(years, exp.b, col='green2', lwd=2, lty=4)
  legend('top', horiz=T, legend=c("Total", "SSB", "Exploitable"), lty=c(1,2,4),
         lwd=c(2,2,2), col=c("maroon", "blue", "green2") )
  title ( "Comparison of January 1 Biomass", outer=T, line=-1 )
  
  if (save.plots==T) savePlot( paste0(od, "Biomass.Comparisons.",plotf), type=plotf)
  return()
} #end function

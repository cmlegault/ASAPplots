#' Plot effective sample size (Neff)
#' 
#' Effective sample size is a measure of how precisely age composition is measured. Plots made by fleet.
#' @param asap name of the variable that read in the asap.rdat file
#' @param fleet.names names of fleets 
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param liz.palette color definitions
#' @param is.catch.flag true means only catch plotted, false also plots discards (defaults to TRUE)
#' @export

PlotFleetNeff <- function(asap,fleet.names,save.plots,od,plotf,liz.palette,is.catch.flag=TRUE){
  par(mfrow=c(1,1) )
  
  years <- seq(asap$parms$styr, asap$parms$endyr) 
  Neff.init <- asap$fleet.catch.Neff.init
  Neff.est <- asap$fleet.catch.Neff.est
  my.title <- "Catch Neff Fleet "
  my.save <- "Catch.Neff.F"
  if (!is.catch.flag){
    Neff.init <- asap$fleet.discard.Neff.init
    Neff.est <- asap$fleet.discard.Neff.est
    my.title <- "Discard Neff Fleet "
    my.save <- "Discard.Neff.F"
  }
  for (i in 1:asap$parms$nfleets) {
    if (sum(Neff.init[i,]) > 0){
      plot(years,Neff.init[i,], type='p', col=liz.palette[i], pch=1, xlab="Year", ylab="Effective Sample Size", 
           ylim=c(0, 1.1*max(Neff.init[i,],Neff.est[i,])) )    
      lines(years, Neff.est[i,], col=liz.palette[i], lwd=2)
      title (paste(my.title, i, " (", fleet.names[i], ")", sep="") )
      if (save.plots) savePlot(paste(od, my.save, i, ".", plotf, sep=""), type=plotf) 
    }
  } #end loop nfleets
  return()
}

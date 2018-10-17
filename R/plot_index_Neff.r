#' Plot effective sample size (Neff) by index
#' 
#' Effective sample size is a measure of how precisely age composition is measured.
#' @param asap name of the variable that read in the asap.rdat file
#' @param index.names names of indices 
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param liz.palette color definitions
#' @export

PlotIndexNeff  <- function(asap,index.names,save.plots,od,plotf,liz.palette){
  par(mfrow=c(1,1) )
  
  years <- seq(asap$parms$styr, asap$parms$endyr) 
  Neff.init <- asap$index.Neff.init
  Neff.est <- asap$index.Neff.est
  index.acomp.flags <- asap$control.parms$index.age.comp.flag
  my.title <- "Index Neff "
  my.save <- "Index.Neff."
  
  for (i in 1:asap$parms$nindices) {
    #cum.age.comp.flag<-0
    if( index.acomp.flags[i]>0) {
      plot(years,Neff.init[i,], type='p', col=liz.palette[i], pch=1, xlab="Year", ylab="Effective Sample Size", 
           ylim=c(0, 1.1*max(Neff.init[i,],Neff.est[i,])) )    
      lines(years, Neff.est[i,], col=liz.palette[i], lwd=2)
      title (paste0(my.title, i, " (", index.names[i], ")") )
      if (save.plots) savePlot(paste0(od, my.save, i, ".", plotf), type=plotf) 
    } #end test for age-comp
  } #end loop nindices
  return()
}   #end function


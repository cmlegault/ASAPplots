#' Plot SSB at age 
#' 
#' Plots both estimated SSB and proportions as stacked barplot.
#' @param asap name of the variable that read in the asap.rdat file
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param liz.palette color definitions
#' @export
 
PlotSSBatAge <- function(asap,save.plots,od,plotf,liz.palette){
  
  par(mfrow=c(1,1) )
  
  years<-  seq(asap$parms$styr, asap$parms$endyr)
  ssb.aa.1 <- asap$N.age*exp(asap$options$frac.yr.spawn*(-asap$F.age-asap$M.age))
  if (asap$options$isfecund == 1){
    ssb.aa <- ssb.aa.1*asap$maturity
  }else{
    ssb.aa <- ssb.aa.1*asap$maturity*asap$WAA.mats$WAA.ssb #ssb at age
  }
  ssb.max=max(apply(ssb.aa,1,sum))
  
  barplot( t(ssb.aa), beside=F  ,  cex.names=0.75 ,  
           width=1, space=rep(0,(asap$parms$nyears)),
           xlab = 'Year', ylab ='ASAP Estimated SSB.AA', ylim = c(0,1.15*ssb.max), 
           xlim=c(0.5,asap$parms$nyears+1-0.5), col=liz.palette[1:asap$parms$nages] )
  
  legend('top', horiz=T, legend=seq(1, asap$parms$nages), 
         pch=15, col=liz.palette[1:asap$parms$nages], cex=0.8 )
  
  if (save.plots==T) savePlot(paste0(od, 'SSB.AA.barplot.stacked.', plotf), type=plotf)
  
  #----same information, but proportion at age each year
  barplot( t(ssb.aa/apply(ssb.aa,1,sum)), beside=F  ,  cex.names=0.75 ,  
           width=1, space=rep(0,(asap$parms$nyears)),
           xlab = 'Year', ylab ='ASAP Estimated Proportion (SSB.AA)', ylim = c(0,1.1), 
           xlim=c(0.5,asap$parms$nyears+1-0.5), col=liz.palette[1:asap$parms$nages] )
  
  legend('top', horiz=T, legend=seq(1, asap$parms$nages), 
         pch=15, col=liz.palette[1:asap$parms$nages], cex=0.8 )
  
  if (save.plots==T) savePlot(paste0(od, 'SSB.AA.proportion.barplot.stacked.', plotf), type=plotf)

  return()
}  #end funciton

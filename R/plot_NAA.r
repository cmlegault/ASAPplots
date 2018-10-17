#' Plot numbers at age 
#' 
#' Plots both estimated numbers and proportions as stacked barplot.
#' @param asap name of the variable that read in the asap.rdat file
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param liz.palette color definitions
#' @export

PlotNAA <- function(asap,save.plots,od,plotf,liz.palette){
  par(mfrow=c(1,1) )
  ## stacked barplot of NAA      
  
  N.max=max(apply(asap$N.age,1,sum))
  
  barplot( t(asap$N.age), beside=F  ,  cex.names=0.75 ,  
           width=1, space=rep(0,(asap$parms$nyears)),
           xlab = 'Year', ylab ='ASAP Estimated NAA', ylim = c(0,1.15*N.max), 
           xlim=c(0.5,asap$parms$nyears+1-0.5), col=liz.palette[1:asap$parms$nages] )
  
  legend('top', horiz=T, legend=seq(1, asap$parms$nages), 
         pch=15, col=liz.palette[1:asap$parms$nages], cex=0.8 )
  
  if (save.plots==T) savePlot(paste0(od, 'NAA.barplot.stacked.', plotf), type=plotf)
  
  #----same information, but proportion at age each year
  barplot( t(asap$N.age/apply(asap$N.age,1,sum)), beside=F  ,  cex.names=0.75 ,  
           width=1, space=rep(0,(asap$parms$nyears)),
           xlab = 'Year', ylab ='ASAP Estimated Proportion (NAA)', ylim = c(0,1.1), 
           xlim=c(0.5,asap$parms$nyears+1-0.5), col=liz.palette[1:asap$parms$nages] )
  
  legend('top', horiz=T, legend=seq(1, asap$parms$nages), 
         pch=15, col=liz.palette[1:asap$parms$nages], cex=0.8 )
  
  if (save.plots==T) savePlot(paste0(od, 'NAA.proportion.barplot.stacked.', plotf), type=plotf)
  
  return()
} # end function

#' Indices 4 panel plots
#' 
#' Shows observed and predicted indices regular and log scale, residuals over time, and resid histogram.
#' @param asap name of the variable that read in the asap.rdat file
#' @param index.names names of indices
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param liz.palette color definitions
#' @export

PlotIndices4Panel <- function(asap,index.names,save.plots,od,plotf,liz.palette){
  par(mar=c(4,4,3,2), oma=c(1,1,1,1), mfrow=c(2,2)  )
  
  for (i in 1:asap$parms$nindices) {
    index.year <- unlist(asap$index.year[i])
    index.obs <- unlist(asap$index.obs[i])
    index.pred <- unlist(asap$index.pred[i])
    index.std.resid <- unlist(asap$index.std.resid[i])
    
    plot(index.year, index.obs, type='p', col=liz.palette[i], pch=1, 
         xlab="Year", ylab="Index Value", xlim=c(asap$parms$styr,asap$parms$endyr),
         ylim=c(0, 1.1*max(index.obs)) )    
    lines(index.year, index.pred, col=liz.palette[i], lwd=2)
    
    log.ob.min <- log(index.obs)-1.96*unlist(asap$index.sigma[i])
    log.ob.max <- log(index.obs)+1.96*unlist(asap$index.sigma[i])
    plot(index.year, log(index.obs), type='p', col=liz.palette[i], pch=1, 
         xlab="Year", ylab="Ln(Index)", xlim=c(asap$parms$styr,asap$parms$endyr),
         ylim=c(min(log.ob.min,log(index.obs)), 1.1*max(log.ob.max,log(index.pred))) )    
    lines(index.year, log(index.pred), col=liz.palette[i], lwd=2)
    arrows(index.year, log.ob.min, index.year, log.ob.max, length=0)
    title (paste("Index ", i, " (", index.names[i], ")", sep=""), outer=T, line=-1 )
    
    plot(index.year, index.std.resid, type='h', lwd=2, col=liz.palette[i], 
         xlab="Year", ylab="Log-scale Std. Residual", xlim=c(asap$parms$styr,asap$parms$endyr)) 
    abline(h=0)
    
    hist(index.std.resid, plot=T, xlab="Std. Residual", ylab="Probability Density", freq=F, main=NULL) 
    if (save.plots) savePlot(paste(od, "Index.4panel.",i,".",plotf, sep=""), type=plotf) 
  }   # end i-loop over nindices
  
  par(mfrow=c(1,1))
  return()
}

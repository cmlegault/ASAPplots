#' Catch 4 panel plots by fleet
#' 
#' Shows observed and predicted catch regular and log scale, residuals over time, and resid histogram.
#' @param asap name of the variable that read in the asap.rdat file
#' @param fleet.names names of fleets 
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param liz.palette color definitions
#' @export

PlotCatch4Panel <- function(asap,fleet.names,save.plots,od,plotf,liz.palette){
  par(mar=c(4,4,3,2), oma=c(1,1,1,1), mfrow=c(2,2)  )
  for (i in 1:asap$parms$nfleets) {
    catch.yrs = which(asap$catch.obs[i,]>0)
    x.yrs = as.numeric(names(catch.yrs))
    plot(x.yrs, asap$catch.obs[i,catch.yrs] ,
         type='p', col=liz.palette[i], pch=1, xlab="Year", ylab="Total Catch", 
         ylim=c(0, 1.1*max(asap$catch.obs[i,catch.yrs] )) )    
    lines(x.yrs, asap$catch.pred[i,catch.yrs], col=liz.palette[i], lwd=2)
    
    sigma <- sqrt(log(asap$control.parms$catch.tot.cv[catch.yrs,i]^2+1.0))
    log.ob.min <- log(asap$catch.obs[i,catch.yrs])-1.96*sigma
    log.ob.max <- log(asap$catch.obs[i,catch.yrs])+1.96*sigma
    plot(x.yrs, log(asap$catch.obs[i,catch.yrs]) ,
         type='p', col=liz.palette[i], pch=1, xlab="Year", ylab="Ln(Total Catch)", 
         ylim=c(min(log.ob.min,log(asap$catch.pred[i,catch.yrs])), 
                1.1*max(log.ob.max,log(asap$catch.pred[i,catch.yrs]))) )    
    lines(x.yrs, log(asap$catch.pred[i,catch.yrs]), col=liz.palette[i], lwd=2)
    arrows(x.yrs, log.ob.min, x.yrs, log.ob.max, length=0)
    title (paste("Fleet ",i, " Catch  (", fleet.names[i], ")", sep=""), outer=T, line=-1 )
    
    c.resid <- rep(NA, length(catch.yrs))
    c.resid <- asap$catch.std.resid[i, catch.yrs]
    plot(x.yrs, c.resid, type='h', lwd=2, col=liz.palette[i], 
         xlab="Year", ylab="Log-scale Std. Residual") 
    abline(h=0)
    
    hist(c.resid, plot=T, xlab="Std. Residual", ylab="Probability Density", freq=F, main=NULL) 
    if (save.plots) savePlot(paste(od, "Catch.4panel.",i,".",plotf, sep=""), type=plotf) 
  } #end loop nfleets
  par(mfrow=c(1,1))
}

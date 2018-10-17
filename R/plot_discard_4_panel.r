#' Discard 4 panel plots by fleet
#' 
#' Shows observed and predicted discards regular and log scale, residuals over time, and resid histogram.
#' @param asap name of the variable that read in the asap.rdat file
#' @param fleet.names names of fleets 
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param liz.palette color definitions
#' @export

PlotDiscard4Panel <- function(asap,fleet.names,save.plots,od,plotf,liz.palette){
  par(mar=c(4,4,3,2), oma=c(1,1,1,1), mfrow=c(2,2)  )
  for (i in 1:asap$parms$nfleets) {
    discard.yrs = which(asap$discard.obs[i,]>0)
    if(length(discard.yrs) > 0){
      x.yrs = as.numeric(names(discard.yrs))
      plot(as.numeric(names(discard.yrs)), asap$discard.obs[i,discard.yrs] ,
           type='p', col=liz.palette[i], pch=1, xlab="Year", ylab="Total Discards", 
           ylim=c(0, 1.1*max(asap$discard.obs[i,discard.yrs] )) )    
      lines( as.numeric(names(discard.yrs)), asap$discard.pred[i,discard.yrs], col=liz.palette[i], lwd=2)
      
      sigma <- sqrt(log(asap$control.parms$catch.tot.cv[discard.yrs,i]^2+1.0))
      log.ob.min <- log(asap$discard.obs[i,discard.yrs])-1.96*sigma
      log.ob.max <- log(asap$discard.obs[i,discard.yrs])+1.96*sigma
      plot(as.numeric(names(discard.yrs)), log(asap$discard.obs[i,discard.yrs]) ,
           type='p', col=liz.palette[i], pch=1, xlab="Year", ylab="Ln(Total Discards)", 
           ylim=c(min(log.ob.min,log(asap$discard.pred[i,discard.yrs])), 
                  1.1*max(log.ob.max,log(asap$discard.pred[i,discard.yrs]) )) )   
      lines( as.numeric(names(discard.yrs)), log(asap$discard.pred[i,discard.yrs]), col=liz.palette[i], lwd=2)
      arrows(x.yrs, log.ob.min, x.yrs, log.ob.max, length=0)
      title (paste0("Fleet ",i, " Discards  (", fleet.names[i], ")"), outer=T, line=-1 )
      
      c.resid <- rep(NA, length(discard.yrs))
      c.resid <- asap$discard.std.resid[i, discard.yrs]
      plot( as.numeric(names(discard.yrs)), c.resid, type='h', lwd=2, col=liz.palette[i], 
            xlab="Year", ylab="Log-scale Std. Residual") 
      abline(h=0)
      
      hist(c.resid, plot=T, xlab="Std. Residual", ylab="Probability Density", freq=F, main=NULL) 
      if (save.plots) savePlot(paste0(od, "Discard.4panel.",i,".",plotf), type=plotf) 
    }
  } #end loop nfleets
  par(mfrow=c(1,1))
  return()
}

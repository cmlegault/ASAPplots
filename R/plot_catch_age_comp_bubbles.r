#' Bubble plots of catch age comps
#' 
#' Makes bubble plots of fishery catch age composition by fleet.
#' @param asap name of the variable that read in the asap.rdat file
#' @param fleet.names names of fleets 
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param scale.catch.bubble.data larger values increase size of catch age comp bubbles (defaults to 6)
#' @param is.catch.flag true means only catch plotted, false also plots discards (defaults to TRUE)
#' @export

PlotCatchAgeCompBubbles <- function(asap,fleet.names,save.plots,od,plotf,scale.catch.bubble.data=6,
                                    is.catch.flag=T){
  
  par(mar=c(4,4,2,2), oma=c(1,1,1,1), mfrow=c(1,1))
  
  ages=seq(1, asap$parms$nages)
  nages=length(ages)
  years=seq(asap$parms$styr, asap$parms$endyr)
  nyrs=asap$parms$nyears
  bubble.color <- "#11aaffaa"
  
  for (i in 1:asap$parms$nfleets) {
    acomp.obs <- as.data.frame(asap$catch.comp.mats[4*(i-1)+1])
    catch.yrs <- which(asap$fleet.catch.Neff.init[i,]>0)
    missing.catch.yrs <- which(asap$fleet.catch.Neff.init[i,] <= 0)
    acomp.obs[missing.catch.yrs,] <- rep(NA, nages)
    my.title <- "Age Comps for Catch by Fleet "
    my.save <- "obs.catch.bubble.plots."
    if (!is.catch.flag){
      acomp.obs <- as.data.frame(asap$catch.comp.mats[4*(i-1)+3])
      catch.yrs <- which(asap$fleet.discard.Neff.init[i,]>0)
      missing.catch.yrs <- which(asap$fleet.catch.Neff.init[i,] <= 0)
      acomp.obs[missing.catch.yrs,] <- rep(NA, nages)
      my.title <- "Age Comps for Discards by Fleet "
      my.save <- "obs.discard.bubble.plots."
      
    }
    if (length(catch.yrs)>0){
      
      z3 <- as.matrix(acomp.obs) * scale.catch.bubble.data
      
      plot(ages, rev(ages),  xlim = c(1, nages), ylim = c(years[nyrs],(years[1]-2)), 
           xlab = "Age", ylab = "", type = "n", axes=F)
      axis(1, at= ages, lab=ages)
      axis(2, at = rev(years), lab = rev(years), cex.axis=0.75, las=1)
      box()
      abline(h=years, col="lightgray")
      segments(x0=seq(ages[1], nages), y0=rep(years[1],nages),
               x1=seq(ages[1], nages), y1=rep(years[nyrs],nages), col = "lightgray", lty = 1)
      
      for (j in 1:nyrs){
        points(ages, rep((years[1]+j-1), nages), cex=z3[j,], col="black",
               bg = bubble.color, pch = 21)
      }
      
      bubble.legend1 <- c(0.05,0.2,0.4)
      bubble.legend2 <- bubble.legend1 * scale.catch.bubble.data
      legend("topright", xpd=T, legend=bubble.legend1, pch=rep(21, 3), 
             pt.cex=bubble.legend2, horiz=T , col='black', pt.bg = bubble.color  )
      title (paste(my.title,i, " (", fleet.names[i], ")", sep=""), 
             outer=T, line=-1 ) 
      if (save.plots) savePlot(paste(od, my.save, i, ".", plotf, sep=''), type=plotf)
    } # end catch.yrs test  
  }   #end loop nfleets
  return()
}

#' Bubble plots of index age comps
#' 
#' Makes bubble plots of age composition by index.
#' @param asap name of the variable that read in the asap.rdat file
#' @param index.names names of indices 
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param scale.index.bubble.data larger values increase size of index age comp bubbles (defaults to 6)
#' @export

PlotIndexAgeCompBubbles <- function(asap,index.names,save.plots,od,plotf,scale.index.bubble.data=6){
  
  par(mar=c(4,4,2,2), oma=c(1,1,1,1), mfrow=c(1,1))
  
  ages=seq(1, asap$parms$nages)
  nages=length(ages)
  years=seq(asap$parms$styr, asap$parms$endyr)
  nyrs=asap$parms$nyears
  bubble.color <- "#11aaffaa"
  for (i in 1:asap$parms$nindices) {
    acomp.obs <- as.data.frame(asap$index.comp.mats[2*i-1])
    index.yrs <- which(asap$index.Neff.init[i,]>0)
    my.title <- "Age Comps for Index "
    my.save <- "obs.index.bubble.plots."
    if (length(index.yrs)>0){
      z3 <- as.matrix(acomp.obs) * scale.index.bubble.data
      
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
      bubble.legend2 <- bubble.legend1 * scale.index.bubble.data
      legend("topright", xpd=T, legend=bubble.legend1, pch=rep(21, 3), 
             pt.cex=bubble.legend2, horiz=T , col='black', pt.bg = bubble.color  )
      # NEW 9/23/2016 changed fleet.names[i] to index.names[i] in title
      title (paste0(my.title,i, " (", index.names[i], ")"), outer=T, line=-1 ) 
      if (save.plots) savePlot(paste0(od, my.save, i, ".", plotf), type=plotf)
    } # end index.yrs test  
  }   #end loop nfleets
  par(mar=c(5.1,4.1,4.1,2.1), oma=c(0,0,0,0))
  return()
}

#' Plot fleet selectivities 
#' 
#' Line plots of selectivities by block.
#' @param asap name of the variable that read in the asap.rdat file
#' @param fleet.names names of fleets 
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param liz.palette color definitions
#' @export

PlotFleetSelBlocks <- function(asap,fleet.names,save.plots,od,plotf,liz.palette){
  par(mfrow=c(1,1) )
  cc=0
  years <- 1:asap$parms$nyears
  for (i in 1:asap$parms$nfleets) {
    a1 <- asap$fleet.sel.start.age[i]
    a2 <- asap$fleet.sel.end.age[i]
    blocks <- unique(asap$fleet.sel.blocks[i,])
    n.blocks <- length(blocks)
    sel.mat <- as.data.frame(asap$fleet.sel.mats[i])
    sel <- matrix(0, nrow=n.blocks, ncol=asap$parms$nages)
    yr <- rep(NA, n.blocks)
    my.col <- rep(NA, n.blocks)
    for (j in 1:n.blocks){
      cc=cc+1
      my.col[j] <- liz.palette[cc]
      yr[j] <- min(years[asap$fleet.sel.blocks[i,]==blocks[j]])
      sel[j,] <- as.numeric(sel.mat[yr[j],a1:a2])
      if (j==1){
        plot(1:asap$parms$nages, sel[j,], type='l', col=my.col[j], 
             xlim=c(0,asap$parms$nages+3), ylim=c(0,1.1), 
             xlab="Age", ylab="Selectivity at Age", lwd=2) 
      }
      if (j>1){
        lines(1:asap$parms$nages, sel[j,], type='l', col=my.col[j], lwd=2)
      }
    }
    title(paste("Fleet ",i," (",fleet.names[i],")", sep=""))
    legend("topright", col=my.col, legend=asap$parms$styr+yr-1, lwd=2)
    if (save.plots) savePlot(paste(od, "Catch.Sel.Blocks.Fleet.",i,".",plotf, sep=""), type=plotf)
  }
  return()
}

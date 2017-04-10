#' Plot weight at age matrices
#' 
#' Line plots of all different weight at age matrices.
#' @param asap name of the variable that read in the asap.rdat file
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param liz.palette color definitions
#' @export

PlotWAAmatrices <- function(asap,save.plots,od,plotf,liz.palette){
  
  years <- seq(asap$parms$styr,asap$parms$endyr)
  n.mats <- length(asap$WAA.mats)
  counter <- 1:n.mats
  waa.set <- rep(NA,n.mats)
  waa.set[1] <- 1
  for (i in 2:n.mats){
    is.new <- T
    for (j in 1:(i-1)){
      if(sum(abs(asap$WAA.mats[[i]]-asap$WAA.mats[[j]]))==0){
        waa.set[i] = waa.set[j]
        is.new <- F
      }  
    }
    if (is.new) waa.set[i] <- max(waa.set, na.rm=T) + 1
  } # end i-loop over n.mats
  
  n.unique <- unique(waa.set)
  for (k in 1:length(n.unique)){
    kk <- min(counter[waa.set==k])
    WAA.plot <- asap$WAA.mats[[kk]]
    plot(years,years,xlab="Year",ylab="Weight",ylim=c(0,1.1*max(WAA.plot)),type='n')
    for (i in 1:asap$parms$nages){
      lines(years,WAA.plot[,i],col=liz.palette[i],lwd=2)
      lines(years,rep(mean(WAA.plot[,i]),length(years)),lty=2,col=liz.palette[i])
    }
    legend('top',legend=names(asap$WAA.mats[counter[waa.set==k]]),cex=0.7,horiz=T)
    title(main=paste("WAA matrix ",k, sep=""), outer=F)
    if (save.plots) savePlot(paste(od, "waa.matrix.", k, ".", plotf, sep=''), type=plotf)
  }  # end k-loop
  return()
}  # end function

#' Plot maturity
#' 
#' Line plot of mean maturity at age with colored dots showing annual values (if differ from mean).
#' @param asap name of the variable that read in the asap.rdat file
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @export

PlotMaturity <- function(asap,save.plots,od,plotf){
  
  ages <- seq(1,asap$parms$nages)
  meanmaturity <- apply(asap$maturity,2,mean)
  nyears <- asap$parms$nyears
  yr.col <- rainbow(nyears, start = 0, end = 0.75)
  
  plot(ages,meanmaturity,type='l',lwd=2,xlab="Age",ylab="Maturity",ylim=c(0,max(asap$maturity)))
  if (length(unique(asap$maturity)) > asap$parms$nages) {         
    for (i in 1:length(asap$maturity[,1])){
      points(jitter(ages, factor=0.25),asap$maturity[i,],col=yr.col[i] )
    }
    legend('topleft', horiz=F, legend=c("First Year", "Last Year"), pch=c(1,1),
           col=c(yr.col[1], yr.col[nyears])  )
  } # end if-test for time-varying maturity
  title(main="Maturity", outer=F)
  if (save.plots) savePlot(paste(od, "maturity.", plotf, sep=''), type=plotf)
  return()
}

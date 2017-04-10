#' Plot natural mortality rate
#' 
#' Plot mean natural mortality rate as line, colored dots show annual values (if differ from mean).
#' @param asap name of the variable that read in the asap.rdat file
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @export

PlotM <- function(asap,save.plots,od,plotf){
  ages <- seq(1,asap$parms$nages)
  meanM <- apply(asap$M,2,mean)
  nyears <- asap$parms$nyears
  yr.col <- rainbow(nyears, start = 0, end = 0.75)
  
  
  plot(ages,meanM,type='l',lwd=2,xlab="Age",ylab="Natural Mortality Rate",
       ylim=c(0,1.1*max(asap$M)))
  
  if (length(unique(asap$M.age)) > asap$parms$nages) {         
    for (i in 1:length(asap$M[,1])){
      points(jitter(ages, factor=0.25), asap$M[i,],col=yr.col[i])
    }
    legend('top', horiz=T, legend=c("First Year", "Last Year"), pch=c(1,1),
           col=c(yr.col[1], yr.col[nyears])  )
  } # end if-test for time varying M
  title(main="M", outer=F)
  if (save.plots) savePlot(paste(od, "M.", plotf, sep=''), type=plotf)
  return()
}

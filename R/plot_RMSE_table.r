#' Plot of RMSE table
#' 
#' Summarizes root mean square error by likelihood component.
#' @param asap.name Base name of original dat file (without the .dat extension)
#' @param asap name of the variable that read in the asap.rdat file
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @export
 
PlotRMSEtable <- function(asap.name,asap,save.plots,od,plotf){
  par(mfrow=c(1,1) )
  max.txt<-16
  n.rmse<-length(asap$RMSE)
  par(oma=rep(2,4), mar=c(0,0,1,0) )
  plot(seq(1,15 ), seq(1,15), type='n', axes=F,
       xlab="", ylab="", xlim=c(1,(max.txt+2+ 6+2+ 8+2) ), ylim=c(n.rmse+4, 1) )
  text(rep(1, n.rmse), seq(3,n.rmse+2), labels=substr(names(asap$RMSE),6,100), pos=4 )
  text(rep(max.txt+2, n.rmse), seq(3,n.rmse+2), labels=asap$RMSE.n, pos=4 )
  text(rep(max.txt+2+ 6+2, n.rmse), seq(3,n.rmse+2), labels=signif(as.numeric(asap$RMSE),3), pos=4 )
  text( c(1, max.txt+2, max.txt+2+ 6+2), rep( 2, 3), 
        labels=c("Component","# resids","RMSE"), font=2, pos=4)
  title(main="Root Mean Square Error computed from Standardized Residuals", outer=T, cex=0.85)
  if (save.plots) savePlot(paste(od, "RMSE.Comp.Table.",plotf, sep=""), type=plotf)
  
  ###NEW  
  rmse.table <- as.data.frame(matrix(NA, nrow=length(asap$RMSE), ncol=3)  )
  rmse.table[,1] <- substr(names(asap$RMSE),6,50)
  rmse.table[,2] <- as.numeric(asap$RMSE.n)
  rmse.table[,3] <- as.numeric(asap$RMSE)
  colnames(rmse.table) <- c(asap.name, "N", "RMSE")
  
  write.csv(rmse.table, file=paste(od, "RMSE.Table.", asap.name, ".csv", sep=""), 
            row.names=F)
  
  return()
}

#' Plot recruitment deviations 
#' 
#' Shows both line and bar plots of SR residuals.
#' @param asap name of the variable that read in the asap.rdat file
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @export

PlotRecruitmentDevs <- function(asap,save.plots,od,plotf){
  par(mfrow=c(2,1), mar=c(4,4,2,2) )
  #__annual esitmates of Recruitment deviations (2 panel plot)____
  
  
  years<-  asap$SR.resids[,1]
  recr <- asap$SR.resids[,2]
  recr.no.devs <- asap$SR.resids[,3]
  resids <- asap$SR.resids[,4]
  
  plot( years, recr.no.devs, type='l', col='#114466', lty=1, lwd=2,
        xlab="Year", ylab="Recruits ", 
        ylim=c(0, 1.1*max(recr, recr.no.devs))  )    
  lines(years, recr, col="grey35", lwd=2)
  points (years, recr, pch=19)
  
  
  plot( years, resids, type='h', col='black',
        xlab="Year", ylab="Ln(Recruitment deviations)", lwd=2,
        ylim=c(1.1*min(resids) , 1.1*max(resids))  )    
  abline(h=0, lwd=1)
  
  if (save.plots==T) savePlot( paste(od, "Recruit.Yr.Devs.",plotf, sep=""), type=plotf)
  
  return()
} # end function

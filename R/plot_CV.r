#' Plot CV for F, SSB, and Recruitment 
#' 
#' Summary plot of estimated precision over time.
#' @param asap name of the variable that read in the asap.rdat file
#' @param a1 list file produced by grab.aux.files function
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @export

PlotCV <- function(asap,a1,save.plots,od,plotf){
  if (any(!is.na(a1$asap.std))){
    par(mfrow=c(1,1), mar=c(4,4,2,2)  )
    
    years = seq(asap$parms$styr, asap$parms$endyr)
    asap.std <- a1$asap.std
    F.rep = asap.std[asap.std$name=="Freport",3]  
    cv.F.rep = asap.std[asap.std$name=="Freport",4]/F.rep
    ssb.vec = asap.std[ asap.std$name=="SSB",3]
    cv.ssb.vec = asap.std[ asap.std$name=="SSB",4]/ssb.vec
    rec.vec = asap.std[ asap.std$name=="recruits", 3]
    cv.rec.vec = asap.std[asap.std$name=="recruits", 4]/rec.vec
    
    plot(years,   cv.rec.vec, type='l', lwd=2, col='black', xlab="Year", ylab="CV",
         ylim=c(0, 1.1*max(cv.rec.vec, cv.ssb.vec, cv.F.rep))  )
    lines(years, cv.ssb.vec, lwd=2, col="blue")
    lines(years, cv.F.rep, lwd=2, col="green3")
    legend('top', legend=c("Recr.", "SSB", "Freport"), col=c("black", "blue", "green3"),
           lty=rep(1,3), lwd=rep(2,3), horiz=T)
    
    if (save.plots==T) savePlot(paste(od, 'CV.estimates.', plotf, sep=''), type=plotf)
  }
  return()
}  # end function

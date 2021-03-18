#' Plot annual SPR targets
#' 
#' Computes and plots SPR targets using selectivity, M, WAA, and maturity by year.
#' @param asap name of the variable that read in the asap.rdat file
#' @param pspr user defined value(s) of spr, e.g. 0.4 or c(0.4, 0.43, 0.46); if empty (default), calculated at seq(0.2, 0.5, by=0.1) 
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @export

PlotAnnualSPRtargets <- function(asap, pspr=c(), save.plots,od,plotf){
  
  if(length(pspr)>0) spr.targ.values <- pspr
  if(length(pspr)==0) spr.targ.values <- seq(0.2, 0.5, by=0.1)
  n.spr <- length(spr.targ.values)
  nages<- asap$parms$nages
  nyears <- asap$parms$nyears
  years <- seq(asap$parms$styr,asap$parms$endyr)  
  fec.age <- asap$WAA.mats$WAA.ssb
  mat.age <- asap$maturity
  wgt.age <- asap$WAA.mats$WAA.catch.all
  M.age <- asap$M.age
  sel.age <- asap$F.age/apply(asap$F.age,1,max)
  
  spawn.time <- asap$options$frac.yr.spawn
  
  spr0.vals<- asap$SR.annual.parms$s.per.r.vec
  F.start <-0.11  # starting guess for optimization routine to find F_SPR%
  
  f.spr.vals <- matrix(NA, nyears, n.spr)
  ypr.spr.vals <- matrix(NA, nyears, n.spr)
  conv.vals <- matrix(NA, nyears, n.spr)
  
  for (i in 1:n.spr) {
    for (j in 1:nyears) {
      t.spr <- spr.targ.values[i]
      
      spr.f <- function(F.start) {
        abs(s.per.recr(nages=nages, fec.age=fec.age[j,], mat.age=mat.age[j,], M.age= M.age[j,], F.mult=F.start, sel.age=sel.age[j,], spawn.time=spawn.time)/spr0.vals[j] - t.spr )
      }
      yyy <- nlminb(start=F.start, objective=spr.f, lower=0, upper=3)
      f.spr.vals[j,i] <- yyy$par
      ypr.spr.vals[j,i] <- ypr(nages, wgt.age=wgt.age[j,], M.age=M.age[j,],  F.mult=f.spr.vals[j,i], sel.age=sel.age[j,] )
      conv.vals[j,i] <- ifelse(yyy$convergence==0, TRUE, FALSE)
    }  # end j-loop over nyears
  }  #end i-loop over SPR values
  
 # if(length(pspr)==0) {
  par(mfrow=c(1,1), mar=c(4,4,2,4) )
  lty.seq=c(1,2,4,6)
  plot(years, f.spr.vals[,1], type='n', xlab="Year", ylab="Full F (%SPR)", lwd=2,
       col="blue3", ylim=c(0,1.2*max(f.spr.vals)) )
  for (i in 1:n.spr) {
    lines(years, f.spr.vals[,i], lwd=2, col=i, lty=lty.seq[i] )     
  }
  if(length(pspr)==0) legend('top', legend=c("F20%", "F30%", "F40%", "F50%"), col=seq(1,4), lty=lty.seq, 
         horiz=T,lwd=rep(2,4), cex=0.9)
  if(length(pspr)>0) legend('top', legend=c(paste0("F", round(100*pspr,0), "%")), col=seq(1,4), lty=lty.seq, 
                             horiz=T,lwd=rep(2,4), cex=0.9)
  
  title (main="Annual F(%SPR) Reference Points", outer=T, line=-1 ) 
  
  if (save.plots) savePlot(paste0(od, "Annual.FSPR.", plotf), type=plotf)
  
  
  plot(years, ypr.spr.vals[,1], type='n', xlab="Year", ylab="YPR (%SPR)", lwd=2,
       col="blue3", ylim=c(0,1.2*max(ypr.spr.vals)) )
  for (i in 1:n.spr) {
    lines(years, ypr.spr.vals[,i], lwd=2, col=i, lty=lty.seq[i] )     
  }
  if(length(pspr)==0) legend('top', legend=c("YPR20%", "YPR30%", "YPR40%", "YPR50%"), col=seq(1,4), lty=lty.seq, 
         horiz=T,lwd=rep(2,4), cex=0.9)
  if(length(pspr)>0) legend('top', legend=c(paste0("YPR", round(100*pspr,0), "%")), col=seq(1,4), lty=lty.seq, 
                             horiz=T,lwd=rep(2,4), cex=0.9)
  
  title (main="Annual YPR(%SPR) Reference Points", outer=T, line=-1 ) 
  
  if (save.plots) savePlot(paste0(od, "Annual.YPR.", plotf), type=plotf)
  
  if(length(pspr)==0) {
  f.hist.20 <- hist(f.spr.vals[,1], plot=F)
  f.hist.30 <- hist(f.spr.vals[,2], plot=F)
  f.hist.40 <- hist(f.spr.vals[,3], plot=F)
  f.hist.50 <- hist(f.spr.vals[,4], plot=F)
  
  par(mfrow=c(2,2), mar=c(4,4,2,2), oma=c(0,0,2,0) )
  
  plot(f.hist.20$mids, f.hist.20$counts, xlab="Full F20%", ylab="Frequency", type='h', lwd=2, 
       ylim=c(0, max(f.hist.20$counts)), col='blue4'  )
  lines(f.hist.20$mids, f.hist.20$counts, lwd=2, col='blue4')
  plot(f.hist.30$mids, f.hist.30$counts, xlab="Full F30%", ylab="Frequency", type='h', lwd=2, 
       ylim=c(0, max(f.hist.30$counts)), col='blue4'  )
  lines(f.hist.30$mids, f.hist.30$counts, lwd=2, col='blue4')
  plot(f.hist.40$mids, f.hist.40$counts, xlab="Full F40%", ylab="Frequency", type='h', lwd=2, 
       ylim=c(0, max(f.hist.40$counts)), col='blue4'  )
  lines(f.hist.40$mids, f.hist.40$counts, lwd=2, col='blue4')
  plot(f.hist.50$mids, f.hist.50$counts, xlab="Full F50%", ylab="Frequency", type='h', lwd=2, 
       ylim=c(0, max(f.hist.50$counts)), col='blue4'  )
  lines(f.hist.50$mids, f.hist.50$counts, lwd=2, col='blue4')
  
  title (main="Annual F (%SPR) Reference Points", outer=T, line=-1 ) 
  
  if (save.plots) savePlot(paste0(od, "Annual.F_SPR.4panel.hist.", plotf), type=plotf)  
  
  
  ypr.hist.20 <- hist(ypr.spr.vals[,1], plot=F)
  ypr.hist.30 <- hist(ypr.spr.vals[,2], plot=F)
  ypr.hist.40 <- hist(ypr.spr.vals[,3], plot=F)
  ypr.hist.50 <- hist(ypr.spr.vals[,4], plot=F)
  
  par(mfrow=c(2,2), mar=c(4,4,2,2), oma=c(0,0,2,0) )
  
  plot(ypr.hist.20$mids, ypr.hist.20$counts, xlab="YPR (F20%)", ylab="Frequency", type='h', lwd=2, 
       ylim=c(0, max(ypr.hist.20$counts)), col='blue4'  )
  lines(ypr.hist.20$mids, ypr.hist.20$counts, lwd=2, col='blue4')
  plot(ypr.hist.30$mids, ypr.hist.30$counts, xlab="YPR (F30%)", ylab="Frequency", type='h', lwd=2, 
       ylim=c(0, max(ypr.hist.30$counts)), col='blue4'  )
  lines(ypr.hist.30$mids, ypr.hist.30$counts, lwd=2, col='blue4')
  plot(ypr.hist.40$mids, ypr.hist.40$counts, xlab="YPR (F40%)", ylab="Frequency", type='h', lwd=2, 
       ylim=c(0, max(ypr.hist.40$counts)), col='blue4'  )
  lines(ypr.hist.40$mids, ypr.hist.40$counts, lwd=2, col='blue4')
  plot(ypr.hist.50$mids, ypr.hist.50$counts, xlab="YPR (F50%)", ylab="Frequency", type='h', lwd=2, 
       ylim=c(0, max(ypr.hist.50$counts)), col='blue4'  )
  lines(ypr.hist.50$mids, ypr.hist.50$counts, lwd=2, col='blue4')
  
  title (main="Annual YPR (%SPR) Reference Points", outer=T, line=-1 ) 
  
  if (save.plots) savePlot(paste0(od, "Annual.YPR_SPR.4panel.hist.", plotf), type=plotf)  
  par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1), oma=c(0,0,0,0))
  } # end test if pspr has length 0
  
  spr.list <- list(f.spr.vals=f.spr.vals, ypr.spr.vals=ypr.spr.vals, conv.vals=conv.vals)
  return(spr.list)
} # end function

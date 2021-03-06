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
  nrepltyseq <- ceiling(n.spr/4) # allows more than 4 line types by repeats
  lty.seq=rep(c(1,2,4,6), nrepltyseq)
  plot(years, f.spr.vals[,1], type='n', xlab="Year", ylab="Full F (%SPR)", lwd=2,
       col="blue3", ylim=c(0,1.2*max(f.spr.vals)) )
  for (i in 1:n.spr) {
    lines(years, f.spr.vals[,i], lwd=2, col=i, lty=lty.seq[i] )     
  }
  legend('top', legend=c(paste0("F", round(100*spr.targ.values,0), "%")), col=seq(1,n.spr), lty=lty.seq, horiz=T,lwd=2, cex=0.9)
  
  title (main="Annual F(%SPR) Reference Points", outer=T, line=-1 ) 
  
  if (save.plots) savePlot(paste0(od, "Annual.FSPR.", plotf), type=plotf)
  
  
  plot(years, ypr.spr.vals[,1], type='n', xlab="Year", ylab="YPR (%SPR)", lwd=2,
       col="blue3", ylim=c(0,1.2*max(ypr.spr.vals)) )
  for (i in 1:n.spr) {
    lines(years, ypr.spr.vals[,i], lwd=2, col=i, lty=lty.seq[i] )     
  }
  legend('top', legend=c(paste0("YPR", round(100*spr.targ.values,0), "%")), col=seq(1,n.spr), lty=lty.seq, horiz=T,lwd=2, cex=0.9)
  
  title (main="Annual YPR(%SPR) Reference Points", outer=T, line=-1 ) 
  
  if (save.plots) savePlot(paste0(od, "Annual.YPR.", plotf), type=plotf)
  
  f.hist.list <- list()
  for (i in 1:n.spr){
    f.hist.list[[i]] <- hist(f.spr.vals[,i], plot=FALSE)
  }

  mynrows <- ceiling(n.spr/2)  
  par(mfrow=c(mynrows,2), mar=c(4,4,2,2), oma=c(0,0,2,0) )
  
  for (i in 1:n.spr){
    myxlab <- paste0("Full F", round(spr.targ.values[i]*100, 0), "%")
    plot(f.hist.list[[i]]$mids, f.hist.list[[i]]$counts, xlab=myxlab, ylab="Frequency", type='h', lwd=2, ylim=c(0, max(f.hist.list[[i]]$counts)), col='blue4')
    lines(f.hist.list[[i]]$mids, f.hist.list[[i]]$counts, lwd=2, col='blue4')
  }

  title (main="Annual F (%SPR) Reference Points", outer=T, line=-1 ) 
  
  if (save.plots) savePlot(paste0(od, "Annual.F_SPR.4panel.hist.", plotf), type=plotf)  
  
  ypr.hist.list <- list()
  for (i in 1:n.spr){
    ypr.hist.list[[i]] <- hist(ypr.spr.vals[,i], plot = FALSE)
  }

  par(mfrow=c(mynrows,2), mar=c(4,4,2,2), oma=c(0,0,2,0) )

  for (i in 1:n.spr){
    myxlab <- paste0("YPR(", round(spr.targ.values[i]*100, 0), "%)")
    plot(ypr.hist.list[[i]]$mids, ypr.hist.list[[i]]$counts, xlab=myxlab, ylab="Frequency", type='h', lwd=2, ylim=c(0, max(ypr.hist.list[[i]]$counts)), col='blue4'  )
    lines(ypr.hist.list[[i]]$mids, ypr.hist.list[[i]]$counts, lwd=2, col='blue4')
  }
  
  title (main="Annual YPR (%SPR) Reference Points", outer=T, line=-1 ) 
  
  if (save.plots) savePlot(paste0(od, "Annual.YPR_SPR.4panel.hist.", plotf), type=plotf)  
  
  par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1), oma=c(0,0,0,0))

  spr.list <- list(f.spr.vals=f.spr.vals, ypr.spr.vals=ypr.spr.vals, conv.vals=conv.vals)
  return(spr.list)
} # end function

#' Plot MCMC
#' 
#' Makes a number of diagnostic plots to evaluate convergence in the Monte Carlo Markov Chain results.
#' @param wd directory where ASAP run is located
#' @param asap.name Base name of original dat file (without the .dat extension)
#' @param asap name of the variable that read in the asap.rdat file
#' @param mcmc.burn number of realizations to remove from start of MCMC (defaults to 0)
#' @param mcmc.thin thinning parameter for MCMC (defaults to 1)
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @export

PlotMCMC  <- function(wd,asap.name,asap,mcmc.burn=0,mcmc.thin=1,save.plots,od,plotf){
  
  # kk5 <- library(plotMCMC, logical.return=T)
  # if (kk5==F) install.packages("plotMCMC" )
  # library(plotMCMC)
  
  f.chain <- paste0(asap.name, ".MCM" )
  #f.chain <- "asap3MCMC.dat"
  # don't run this function if chains are not present
  # just in case Enable MCMC Calculations box checked in General Tab of GUI but MCMC not actually run
  if (file.exists(paste0(wd,"\\",f.chain)) == FALSE){
    return()
  }
  chain1c <- read.table(paste0(wd,"\\",f.chain), header=T)
  ## new stuff
  niter <-  dim(chain1c)[1]
  if (niter == 0) return() # in case asap$options$do.mcmc>0 but MCMC not actually run
  chain1b <-  chain1c[(mcmc.burn+1):niter,]
  niter <-  dim(chain1b)[1]
  chain1a <-  chain1b[seq(1,niter,by=mcmc.thin),]
  
  bsn1c <- read.table(paste0(wd,"\\",asap.name, ".BSN"), header=T)
  niter <-  dim(bsn1c)[1]
  bsn1b <-  bsn1c[(mcmc.burn):niter,]
  niter <-  dim(bsn1b)[1]
  bsn1a <-  bsn1b[seq(1,niter,by=mcmc.thin),]
  write.table(bsn1a, file=paste0(od, "New_BSN_file.BSN"), row.names=T)
  write.table(bsn1a, file=paste0(wd, "\\New_", asap.name,".BSN"), row.names=F, col.names=F)
  
  niter <-  dim(chain1a)[1]
  nyears <- asap$parms$nyears
  years <- seq(asap$parms$styr, asap$parms$endyr)
  
  f.chain <- chain1a[,seq(1,nyears)]
  ssb.cols1 <- which(substr(names(chain1a),1,3)=="SSB")
  ssb.chain <- chain1a[,ssb.cols1[1:(length(ssb.cols1)-2)] ]
  fmult.chain <- chain1a[,which(substr(names(chain1a),1,3)=="Fmu")]
  totB.chain <- chain1a[,which(substr(names(chain1a),1,3)=="tot")]
  MSY.chain <- chain1a[,which(substr(names(chain1a),1,3)=="MSY")]
  MSY.col <- which(substr(names(chain1a),1,3)=="MSY")
  SSBmsy.chain <- chain1a[,(MSY.col+1)]
  Fmsy.chain <- chain1a[,(MSY.col+2) ]
  SSBmsy.ratio.chain <- chain1a[,(MSY.col+3) ]
  Fmsy.ratio.chain <- chain1a[,(MSY.col+4 ) ]
  
  
  # examine Trace in first and last year
  
  par(mfcol=c(2,1),mar=c(4,4,2,2), oma=c(1,1,1,1))
  plot(seq(1,niter), ssb.chain[,1], type='l', xlab="Iteration", ylab=paste0("SSB",years[1]) )
  plot(seq(1,niter), ssb.chain[,nyears], type='l', xlab="Iteration", ylab=paste0("SSB",years[nyears]))
  if (save.plots==T)  savePlot(paste0(od,'Trace.SSB.first.last.yr.',plotf),type=plotf)
  
  par(mfcol=c(2,1),mar=c(4,4,2,2), oma=c(1,1,1,1))
  plot(seq(1,niter), f.chain[,1], type='l', xlab="Iteration", ylab=paste0("Freport",years[1]) )
  plot(seq(1,niter), f.chain[,nyears], type='l', xlab="Iteration", ylab=paste0("Freport",years[nyears]))
  if (save.plots==T)  savePlot(paste0(od,'Trace.Freport.first.last.yr.',plotf),type=plotf)
  
  
  ##NEW
  # examine cumuplot in first and last year to determine if chain long enough (quartiles stabilized)
  mcmc.outs <- cbind( f.chain[,c(1,nyears)],  ssb.chain[,c(1,nyears)])
  plotMCMC::plotCumu(mcmc.outs,probs=c(0.05,0.95), div=1, xlab="Iterations",
           ylab="Median with 90% PI", lty.median=1, lwd.median=2, col.median="black", lty.outer=2, lwd.outer=1,
           col.outer="black"  )
  
  if (save.plots==T)  savePlot(paste0(od,'Cumu.F_and_SSB.first.last.yr.',plotf),type=plotf)

  
  
  # look at auto-correlation plot
  par(mfcol=c(2,1),mar=c(4,4,2,2))
  ac.ssb1<-acf(ssb.chain[,1], lag.max=10, plot=F)
  ac.ssb2<-acf(ssb.chain[,nyears], lag.max=10, plot=F)
  ylims <- c(1.1*min(ac.ssb1$acf,-2/sqrt(niter)), 1 ) 
  plot(seq(0,10), ac.ssb1$acf, xlab=paste0("Lag SSB",years[1]), ylab="ACF", type="h",ylim=ylims )
  abline(h=0, lwd=2, col='black')
  abline(h=2/sqrt(niter), col='red', lty=2)
  abline(h=-2/sqrt(niter), col='red', lty=2)
  
  plot(seq(0,10), ac.ssb2$acf, xlab=paste0("Lag SSB",years[nyears]), ylab="ACF", type="h",ylim=ylims )
  abline(h=0, lwd=2, col='black')
  abline(h=2/sqrt(niter), col='red', lty=2)
  abline(h=-2/sqrt(niter), col='red', lty=2)
  
  if (save.plots==T)  savePlot(file=paste0(od, "lag.autocorrelation.SSB.",plotf), type=plotf)
  
  par(mfcol=c(2,1),mar=c(4,4,2,2))
  ac.f1<-acf(f.chain[,1], lag.max=10, plot=F)
  ac.f2<-acf(f.chain[,nyears], lag.max=10, plot=F)
  ylims <- c(1.1*min(ac.f1$acf,-2/sqrt(niter)), 1 ) 
  plot(seq(0,10), ac.f1$acf, xlab=paste0("Lag F",years[1]), ylab="ACF", type="h",ylim=ylims )
  abline(h=0, lwd=2, col='black')
  abline(h=2/sqrt(niter), col='red', lty=2)
  abline(h=-2/sqrt(niter), col='red', lty=2)
  
  plot(seq(0,10), ac.f2$acf, xlab=paste0("Lag F",years[nyears]), ylab="ACF", type="h",ylim=ylims )
  abline(h=0, lwd=2, col='black')
  abline(h=2/sqrt(niter), col='red', lty=2)
  abline(h=-2/sqrt(niter), col='red', lty=2)
  
  if (save.plots==T)  savePlot(file=paste0(od, "lag.autocorrelation.Freport.",plotf), type=plotf)
  
  
  # examine Distribution in first and last year
  ssb1.hist<-hist(ssb.chain[,1],breaks = "Sturges", include.lowest = TRUE, right = TRUE, plot=F)
  ssb2.hist<-hist(ssb.chain[,nyears],breaks = "Sturges", include.lowest = TRUE, right = TRUE, plot=F)
  xlims <- c(min(ssb1.hist$mids, ssb2.hist$mids), max(ssb1.hist$mids, ssb2.hist$mids))
  
  x1 <- (ssb1.hist$mids)
  y1 <- (ssb1.hist$counts) 
  x2 <- (ssb2.hist$mids)
  y2 <- (ssb2.hist$counts) 
  
  par(mfrow=c(2,1) )
  plot(x1,y1, type="l",lty=2,col="blue",lwd=4,xlab=paste0("SSB", years[1]), ylab="Freq", 
       ylim=c(0, max( 1.02*y1)), xlim=c(0.98*xlims[1], 1.02*xlims[2]) )
  abline(v=asap$SSB[1], col='red', lty=4)
  legend('topleft', legend=c("MCMC", "Point Est."), col=c("blue", "red"), lwd=c(2,2),
         lty=c(1,4), cex=0.85 )
  
  plot(x2,y2, type="l",lty=2,col="blue",lwd=4,xlab=paste0("SSB", years[nyears]), ylab="Freq", 
       ylim=c(0, max( 1.02*y2)), xlim=c(0.98*xlims[1], 1.02*xlims[2]) )
  abline(v=asap$SSB[nyears], col='red', lty=4)
  if (save.plots==T)  savePlot(paste0(od, 'Distribution.SSB.first.last.yr.',plotf), type=plotf)
  
  
  
  f1.hist<-hist(f.chain[,1],breaks = "Sturges", include.lowest = TRUE, right = TRUE, plot=F)
  f2.hist<-hist(f.chain[,nyears],breaks = "Sturges", include.lowest = TRUE, right = TRUE, plot=F)
  xlims <- c(min(f1.hist$mids, f2.hist$mids), max(f1.hist$mids, f2.hist$mids))
  
  x1 <- (f1.hist$mids)
  y1 <- (f1.hist$counts) 
  x2 <- (f2.hist$mids)
  y2 <- (f2.hist$counts) 
  
  par(mfrow=c(2,1) )
  plot(x1,y1, type="l",lty=2,col="blue",lwd=4,xlab=paste0("Freport", years[1]), ylab="Freq", 
       ylim=c(0, max( 1.02*y1)), xlim=c(0.98*xlims[1], 1.02*xlims[2]) )
  abline(v=asap$F.report[1], col='red', lty=4)
  legend('topleft', legend=c("MCMC", "Point Est."), col=c("blue", "red"), lwd=c(2,2),
         lty=c(1,4), cex=0.85 )
  
  
  plot(x2,y2, type="l",lty=2,col="blue",lwd=4,xlab=paste0("Freport", years[nyears]), ylab="Freq", 
       ylim=c(0, max( 1.02*y2)), xlim=c(0.98*xlims[1], 1.02*xlims[2]) )
  abline(v=asap$F.report[nyears], col='red', lty=4)
  
  if (save.plots==T)  savePlot(paste0(od, 'Distribution.Freport.first.last.yr.',plotf), type=plotf)
  
  
  
  fm1.hist<-hist(fmult.chain[,1],breaks = "Sturges", include.lowest = TRUE, right = TRUE, plot=F)
  fm2.hist<-hist(fmult.chain[,nyears],breaks = "Sturges", include.lowest = TRUE, right = TRUE, plot=F)
  xlims <- c(min(fm1.hist$mids, fm2.hist$mids), max(fm1.hist$mids, fm2.hist$mids))
  
  x1 <- (fm1.hist$mids)
  y1 <- (fm1.hist$counts) 
  x2 <- (fm2.hist$mids)
  y2 <- (fm2.hist$counts) 
  full.f <- apply(asap$F.age,1,max)
  
  par(mfrow=c(2,1) )
  plot(x1,y1, type="l",lty=2,col="blue",lwd=4,xlab=paste0("Full F", years[1]), ylab="Freq", 
       ylim=c(0, max( 1.02*y1)), xlim=c(0.98*xlims[1], 1.02*xlims[2]) )
  abline(v=full.f[1], col='red', lty=4)
  legend('topleft', legend=c("MCMC", "Point Est."), col=c("blue", "red"), lwd=c(2,2),
         lty=c(1,4), cex=0.85 )
  
  
  plot(x2,y2, type="l",lty=2,col="blue",lwd=4,xlab=paste0("Full F", years[nyears]), ylab="Freq", 
       ylim=c(0, max( 1.02*y2)), xlim=c(0.98*xlims[1], 1.02*xlims[2]) )
  abline(v=full.f[nyears], col='red', lty=4)
  
  if (save.plots==T)  savePlot(paste0(od, 'Distribution.Fmult.first.last.yr.',plotf), type=plotf)
  
  
  b1.hist<-hist(totB.chain[,1],breaks = "Sturges", include.lowest = TRUE, right = TRUE, plot=F)
  b2.hist<-hist(totB.chain[,nyears],breaks = "Sturges", include.lowest = TRUE, right = TRUE, plot=F)
  xlims <- c(min(b1.hist$mids, b2.hist$mids), max(b1.hist$mids, b2.hist$mids))
  
  x1 <- (b1.hist$mids)
  y1 <- (b1.hist$counts) 
  x2 <- (b2.hist$mids)
  y2 <- (b2.hist$counts) 
  tot.B <- asap$tot.jan1.B
  
  par(mfrow=c(2,1) )
  plot(x1,y1, type="l",lty=2,col="blue",lwd=4,xlab=paste0("Jan-1 B", years[1]), ylab="Freq", 
       ylim=c(0, max( 1.02*y1)), xlim=c(0.98*xlims[1], 1.02*xlims[2]) )
  abline(v=tot.B[1], col='red', lty=4)
  legend('topleft', legend=c("MCMC", "Point Est."), col=c("blue", "red"), lwd=c(2,2),
         lty=c(1,4), cex=0.85 )
  
  
  plot(x2,y2, type="l",lty=2,col="blue",lwd=4,xlab=paste0("Jan-1 B", years[nyears]), ylab="Freq", 
       ylim=c(0, max( 1.02*y2)), xlim=c(0.98*xlims[1], 1.02*xlims[2]) )
  abline(v=tot.B[nyears], col='red', lty=4)
  
  if (save.plots==T)  savePlot(paste0(od, 'Distribution.Jan1.B.first.last.yr.',plotf), type=plotf)
  
  
  ####   Probability Interval Plots
  #plot 95% PI
  
  #sort 
  par(mfrow=c(1,1), mar=c(4,4,2,3), oma=c(1,1,1,1)  )
  
  ssb.sort<- (apply(ssb.chain,2,sort ))
  p5 <- trunc( dim(ssb.sort)[1] *.05)
  p95 <-  trunc( dim(ssb.sort)[1] *.95)
  
  p50 <- median(dim(ssb.sort)[1])
  p10 <-  trunc( dim(ssb.sort)[1] *.10)
  p90 <-  trunc( dim(ssb.sort)[1] *.90)
  
  
  plot(years, ssb.sort[p5,], type='l', col='grey35', lwd=2, xlab='Year',
       ylab='', ylim=c(0,1.03*max(ssb.sort[p95,])), axes=F  )
  axis(side=1, at=years[seq(1,nyears,by=2)], labels=years[seq(1,nyears,by=2)], las=2)
  axis(side=2, at=pretty(seq(0,1.01*max(ssb.sort)), n=10) , 
       labels=format(pretty(seq(0,1.01*max(ssb.sort)), n=10), scientific=T), las=1)
  axis(side=4, at=pretty(seq(0,1.01*max(ssb.sort)), n=10) , 
       labels=format(pretty(seq(0,1.01*max(ssb.sort)), n=10), scientific=T), las=1)
  box()
  mtext(side=2, text="SSB", outer=T)    
  lines( years, ssb.sort[p95,] , col='grey35', lwd=2)  
  lines( years, apply(ssb.sort,2,median) , col='red', lwd=2)      
  lines( years, asap$SSB , col='green3', lwd=1)      
  points( years, asap$SSB , col='green3', pch=17, cex=0.7)  
  
  legend('topleft', horiz=T, legend=c("5th, 95th", "Median","Point Est."), 
         col=c("grey35", "red", "green3"), lwd=c(2,2,2), lty=c(1,1,1), cex=0.85,
         pch=c(1,1,17), pt.cex=c(0,0,1) )
  
  if (save.plots==T)  savePlot(paste0(od, "SSB.90PI.", plotf), type=plotf)
  
  
  ssb.pi <- cbind(years, "5th"=ssb.sort[p5,], "Median"=apply(ssb.sort,2,median), "95th"=ssb.sort[p95,])
  write.csv(ssb.pi, file=paste0(od, "ssb.90pi_",asap.name,".csv"), 
            row.names=F )
  
  
  
  f.sort <- (apply(f.chain,2,sort ))
  p5 <- trunc( dim(f.sort)[1] *.05)
  p95 <- trunc( dim(f.sort)[1] *.95)
  
  p50 <- median(dim(f.sort)[1])
  p10 <- trunc( dim(f.sort)[1] *.10)
  p90 <- trunc( dim(f.sort)[1] *.90)
  
  par(mfrow=c(1,1), mar=c(4,4,2,3), oma=c(1,1,1,1)  )
  plot(years, f.sort[p5,], type='l', col='grey35', lwd=2, xlab='Year',
       ylab='Freport', ylim=c(0,1.1*max(f.sort[p95,])), axes=F  )
  axis(side=1, at=years[seq(1,nyears,by=2)], labels=years[seq(1,nyears,by=2)], las=2)
  axis(side=2, at=pretty(seq(0,1.01*max(f.sort), by=0.1), n=10) , 
       labels=pretty(seq(0,1.01*max(f.sort), by=0.1), n=10), las=1)
  axis(side=4, at=pretty(seq(0,1.01*max(f.sort), by=0.1), n=10) , 
       labels=pretty(seq(0,1.01*max(f.sort), by=0.1), n=10), las=1)
  box()
  
  lines( years, f.sort[p95,] , col='grey35', lwd=2)  
  lines( years, apply(f.sort,2,median) , col='red', lwd=2)      
  lines( years, asap$F.report , col='green3', lwd=1)      
  points( years, asap$F.report , col='green3', pch=17, cex=0.7)      
  legend('topleft', horiz=T, legend=c("5th, 95th", "Median","Point Est."), 
         col=c("grey35", "red", "green3"), lwd=c(2,2,2), lty=c(1,1,1), cex=0.85,
         pch=c(1,1,17), pt.cex=c(0,0,1) )
  
  if (save.plots==T)  savePlot(paste0(od, "Freport.90PI.", plotf), type=plotf)
  
  
  Freport.pi <- cbind(years, "5th"=f.sort[p5,], "Median"=apply(f.sort,2,median), "95th"=f.sort[p95,])
  write.csv(Freport.pi, file=paste0(od, "Freport.90pi_",asap.name,".csv"),   row.names=F)
  
  
  
  
  fm.sort <- (apply(fmult.chain,2,sort ))
  p5 <- trunc( dim(fm.sort)[1] *.05)
  p95 <- trunc( dim(fm.sort)[1] *.95)
  
  p50 <- median(dim(fm.sort)[1])
  p10 <- trunc( dim(fm.sort)[1] *.10)
  p90 <- trunc( dim(fm.sort)[1] *.90)
  
  par(mfrow=c(1,1), mar=c(4,4,2,3), oma=c(1,1,1,1)  )
  plot(years, fm.sort[p5,], type='l', col='grey35', lwd=2, xlab='Year',
       ylab='Full F', ylim=c(0,1.1*max(fm.sort[p95,])), axes=F  )
  axis(side=1, at=years[seq(1,nyears,by=2)], labels=years[seq(1,nyears,by=2)], las=2)
  axis(side=2, at=pretty(seq(0,1.01*max(fm.sort), by=0.1), n=10) , 
       labels=pretty(seq(0,1.01*max(fm.sort), by=0.1), n=10), las=1)
  axis(side=4, at=pretty(seq(0,1.01*max(fm.sort), by=0.1), n=10) , 
       labels=pretty(seq(0,1.01*max(fm.sort), by=0.1), n=10), las=1)
  box()
  
  lines( years, fm.sort[p95,] , col='grey35', lwd=2)  
  lines( years, apply(fm.sort,2,median) , col='red', lwd=2)      
  lines( years, full.f , col='green3', lwd=1)      
  points( years, full.f, col='green3', pch=17, cex=0.7)     
  legend('topleft', horiz=T, legend=c("5th, 95th", "Median","Point Est."), 
         col=c("grey35", "red", "green3"), lwd=c(2,2,2), lty=c(1,1,1), cex=0.85,
         pch=c(1,1,17), pt.cex=c(0,0,1) )
  
  if (save.plots==T)  savePlot(paste0(od, "Full.F.90PI.", plotf), type=plotf)
  
  
  Full.F.pi <- cbind(years, "5th"=fm.sort[p5,], "Median"=apply(fm.sort,2,median), "95th"=fm.sort[p95,])
  write.csv(Full.F.pi, file=paste0(od, "Full.F.90pi_",asap.name,".csv"), row.names=F)
  
  
  
  tb.sort <- (apply(totB.chain,2,sort ))
  p5 <- trunc( dim(tb.sort)[1] *.05)
  p95 <- trunc( dim(tb.sort)[1] *.95)
  
  p50 <- median(dim(tb.sort)[1])
  p10 <- trunc( dim(tb.sort)[1] *.10)
  p90 <- trunc( dim(tb.sort)[1] *.90)
  
  par(mfrow=c(1,1), mar=c(4,4,2,3), oma=c(1,1,1,1)  )
  plot(years, tb.sort[p5,], type='l', col='grey35', lwd=2, xlab='Year',
       ylab='', ylim=c(0,1.03*max(tb.sort[p95,])), axes=F  )
  axis(side=1, at=years[seq(1,nyears,by=2)], labels=years[seq(1,nyears,by=2)], las=2)
  axis(side=2, at=pretty(seq(0,1.01*max(tb.sort), by=max(tb.sort)/10), n=10) , 
       labels=format(pretty(seq(0,1.01*max(tb.sort), by=max(tb.sort)/10), n=10), scientific=T), las=1)
  axis(side=4, at=pretty(seq(0,1.01*max(tb.sort), by=max(tb.sort)/10), n=10) , 
       labels=format(pretty(seq(0,1.01*max(tb.sort), by=max(tb.sort)/10), n=10), scientific=T), las=1)
  
  box()
  mtext(side=2, text="Jan-1 Biomass", outer=T)    
  lines( years, tb.sort[p95,] , col='grey35', lwd=2)  
  lines( years, apply(tb.sort,2,median) , col='red', lwd=2)      
  lines( years, asap$tot.jan1.B , col='green3', lwd=1)      
  points( years, asap$tot.jan1.B, col='green3', pch=17, cex=0.7) 
  legend('topleft', horiz=T, legend=c("5th, 95th", "Median","Point Est."), 
         col=c("grey35", "red", "green3"), lwd=c(2,2,2), lty=c(1,1,1), cex=0.85,
         pch=c(1,1,17), pt.cex=c(0,0,1) )
  
  if (save.plots==T)  savePlot(paste0(od, "Jan1.B.90PI.", plotf), type=plotf)
  
  
  Tot.B.pi <- cbind(years, "5th"=tb.sort[p5,], "Median"=apply(tb.sort,2,median), "95th"=tb.sort[p95,])
  write.csv(Tot.B.pi, file=paste0(od, "Jan1.B.90pi_",asap.name,".csv"), row.names=F)
  
  return()
}  # end function

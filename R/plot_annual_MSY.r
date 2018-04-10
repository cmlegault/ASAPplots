#' Plot annual MSY
#' 
#' Plot values associated with MSY using year-specific inputs.
#' @param asap name of the variable that read in the asap.rdat file
#' @param a1 list file produced by grab.aux.files function
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @export

PlotAnnualMSY <- function(asap,a1,save.plots,od,plotf){
  
  h.vals <- asap$SR.annual.parms$steepness.vec
  recr.par <- h.vals
  
  
  if (asap$control.parms$phases$phase.steepness>0 & max(h.vals)<1 ) { # test to make sure steepness was estimated
    
    nages<- asap$parms$nages
    nyears <- asap$parms$nyears
    years <- seq(asap$parms$styr,asap$parms$endyr)  
    fec.age <- asap$WAA.mats$WAA.ssb
    mat.age <- asap$maturity
    wgt.age <- asap$WAA.mats$WAA.catch.all
    M.age <- asap$M.age
    sel.mat <- asap$F.age/apply(asap$F.age,1,max)
    
    spawn.time <- asap$options$frac.yr.spawn
    
    
    
    ahat.vals <- 4*h.vals/(1-h.vals)
    R0.vals <- asap$SR.annual.parms$R0.vec
    sr0.vals <- asap$SR.annual.parms$s.per.r.vec
    MSY.soln <- matrix(NA, nrow=9, ncol=nyears)
    rownames(MSY.soln) <- c("Fmsy", "MSY", "SPRmsy", "SSBmsy", "Rmsy", "YPRmsy",
                            "Rel.SSBmsy", "Rel.Rmsy", "Conv.Code")
    colnames(MSY.soln) <-  seq(asap$parms$styr, asap$parms$endyr)
    
    for (i in 1:nyears) { 
      F.start=0.25
      get.yield.f.min <- function(F.start) {
        temp.spr = s.per.recr(nages=nages, fec.age=fec.age[i,], mat.age=mat.age[i,], M.age= M.age[i,], F.mult=F.start, sel.age=sel.mat[i,], spawn.time=spawn.time)   
        temp.ypr = ypr(nages=nages, wgt.age=wgt.age[i,], M.age=M.age[i,],  F.mult=F.start, sel.age=sel.mat[i,] )
        temp.SSB = ssb.eq( recr.par=ahat.vals[i], R0.BH=R0.vals[i], spr=temp.spr, spr0=sr0.vals[i], is.steepness=F )
        yield = temp.ypr*temp.SSB/temp.spr           #harvest in weight
        yy=-1*yield
        return(yy)  
      }   # end get.yield.f.min function
      
      F.nlmin <- nlminb( start=F.start , objective=get.yield.f.min, lower=0.01, upper=3.0,
                         control=list(eval.max=500, iter.max=200  )   )
      
      
      
      MSY.soln[1, i] <- F.nlmin$par  #Fmsy
      MSY.soln[2, i] <- -1*F.nlmin$objective #MSY
      
      # calculate other MSY quantities
      spr.nlmin <- s.per.recr(nages=nages, fec.age=fec.age[i,], mat.age=mat.age[i,], M.age= M.age[i,], F.mult=F.nlmin$par, sel.age=sel.mat[i,], spawn.time=spawn.time)   
      MSY.soln[3, i] <- spr.nlmin/sr0.vals[i] #SPRmsy
      
      ssb.nlmin <- ssb.eq( recr.par=ahat.vals[i], R0.BH=R0.vals[i], spr=spr.nlmin, spr0=sr0.vals[i], is.steepness=F )
      MSY.soln[4, i] <-  ssb.nlmin #SSBmsy
      
      
      Rmsy.nlmin <- bev.holt.alpha(S=ssb.nlmin,R0=R0.vals[i],recr.par=ahat.vals[i],spr0=sr0.vals[i])
      MSY.soln[5,i ] <- Rmsy.nlmin #Rmsy
      
      ypr.nlmin <- ypr(nages=nages, wgt.age=wgt.age[i,], M.age=M.age[i,],  F.mult=F.nlmin$par, sel.age=sel.mat[i,] )
      MSY.soln[6,i ] <- ypr.nlmin #YPRmsy
      
      rel.ssb.nlmin <- ssb.nlmin/(R0.vals[i]*sr0.vals[i])
      MSY.soln[7, i] <-  rel.ssb.nlmin #SSBmsy/SSB0
      
      MSY.soln[8, i] <- Rmsy.nlmin/R0.vals[i] #Rmsy/R0
      
      MSY.soln[9, i] <-  F.nlmin$convergence  #code=0 indicates convergence
    }  # end i-loop over nyears
    
    f.hist <- hist(MSY.soln[1,], plot=F)
    MSY.hist <- hist(MSY.soln[2,], plot=F)
    R.hist <- hist(MSY.soln[5,], plot=F ) 
    S.hist <- hist(MSY.soln[4,], plot=F ) 
    
    h.hist <- hist(h.vals, plot=F)
    SPR.hist <- hist(MSY.soln[3,], plot=F)
    R0.hist <- hist(R0.vals, plot=F ) 
    S0.hist <- hist(asap$SR.annual.parms$S0.vec, plot=F ) 
    
    par(mfrow=c(2,2), mar=c(4,4,2,2), oma=c(0,0,2,0) )
    
    plot(f.hist$mids, f.hist$counts, xlab="Full F", ylab="Frequency", type='h', lwd=2, ylim=c(0, max(f.hist$counts))  )
    lines(f.hist$mids, f.hist$counts, lwd=2)
    plot(S.hist$mids, S.hist$counts, xlab="SSB.MSY", ylab="Frequency", type='h', lwd=2, ylim=c(0, max(S.hist$counts))  )
    lines(S.hist$mids, S.hist$counts, lwd=2)
    plot(MSY.hist$mids, MSY.hist$counts, xlab="MSY", ylab="Frequency", type='h', lwd=2, ylim=c(0, max(MSY.hist$counts)) ) 
    lines(MSY.hist$mids, MSY.hist$counts, lwd=2)
    plot(R.hist$mids, R.hist$counts, xlab="R.MSY", ylab="Frequency", type='h', lwd=2, ylim=c(0, max(R.hist$counts))  )
    lines(R.hist$mids, R.hist$counts, lwd=2)
    title (paste("Annual MSY Reference Points (from S-R curve)", sep=""), 
           outer=T, line=-1 ) 
    if (save.plots) savePlot(paste(od, "MSY.4panel.hist1.", plotf, sep=''), type=plotf)  
    
    
    plot(h.hist$mids, h.hist$counts, xlab="steepness", ylab="Frequency", type='h', lwd=2, ylim=c(0, max(h.hist$counts))  )
    lines(h.hist$mids, h.hist$counts, lwd=2)
    plot(S0.hist$mids, S0.hist$counts, xlab="SSB0", ylab="Frequency", type='h', lwd=2, ylim=c(0, max(S0.hist$counts))  )
    lines(S0.hist$mids, S0.hist$counts, lwd=2)
    plot(SPR.hist$mids, SPR.hist$counts, xlab="SPR.MSY", ylab="Frequency", type='h', lwd=2, ylim=c(0, max(SPR.hist$counts)) )
    lines(SPR.hist$mids, SPR.hist$counts, lwd=2)
    plot(R0.hist$mids, R0.hist$counts, xlab="R0", ylab="Frequency", type='h', lwd=2, ylim=c(0, max(R0.hist$counts))  )
    lines(R0.hist$mids, R0.hist$counts, lwd=2)
    title (paste("Annual MSY Reference Points (from S-R curve)", sep=""), 
           outer=T, line=-1 ) 
    if (save.plots) savePlot(paste(od, "MSY.4panel.hist2.", plotf, sep=''), type=plotf)  
    
    
    par(mfrow=c(1,1), mar=c(4,4,2,2) )
    
    h.order <- order(h.vals)
    steep.spr <- cbind(h.vals[h.order], MSY.soln[3,h.order])
    
    plot(steep.spr[,1], steep.spr[,2], xlab="Steepness", ylab="SPR.MSY", xlim=c(0.2,1), ylim=c(0,1),
         type='p', pch=16, col='blue3' )
    points(h.vals[1], MSY.soln[3,1], pch=16, cex=1.5, col="green3")       
    points(h.vals[nyears], MSY.soln[3,nyears], pch=16, cex=1.5, col="orange2")       
    legend('top', legend=c("First Year", "Last Year"), pch=c(16,16), col=c("green3", "orange2") )
    title (paste("Annual Steepness and SPR.MSY (from S-R curve)", sep=""), 
           outer=T, line=-1 ) 
    
    if (save.plots) savePlot(paste(od, "MSY.h.vs.SPR.", plotf, sep=''), type=plotf)         
    
    plot(years, MSY.soln[1,], xlab="Year", ylab="Full F at MSY", ylim=1.1*c(0, max(MSY.soln[1,])), type='l', lwd=2  ) 
    points(years, MSY.soln[1,], pch=10)
    title (paste("Annual MSY Reference Points (from S-R curve)", sep=""), 
           outer=T, line=-1 ) 
    
    if (save.plots) savePlot(paste(od, "Annual.Fmsy.", plotf, sep=''), type=plotf)  
    
    plot(years, MSY.soln[2,], xlab="Year", ylab="MSY", ylim=1.1*c(0, max(MSY.soln[2,])), type='l', lwd=2  ) 
    points(years, MSY.soln[2,], pch=10)
    title (paste("Annual MSY Reference Points (from S-R curve)", sep=""), 
           outer=T, line=-1 ) 
    
    if (save.plots) savePlot(paste(od, "Annual.MSY.", plotf, sep=''), type=plotf)  
    
    plot(years, MSY.soln[3,], xlab="Year", ylab="%SPR at MSY", ylim=1.1*c(0, 1), type='l', lwd=2  ) 
    points(years, MSY.soln[3,], pch=10)
    title (paste("Annual MSY Reference Points (from S-R curve)", sep=""), 
           outer=T, line=-1 ) 
    
    if (save.plots) savePlot(paste(od, "Annual.SPR.MSY.", plotf, sep=''), type=plotf)  
    
    plot(years, MSY.soln[4,], xlab="Year", ylab="SSB at MSY", ylim=1.1*c(0, max(MSY.soln[4,])), type='l', lwd=2  ) 
    points(years, MSY.soln[4,], pch=10)
    title (paste("Annual MSY Reference Points (from S-R curve)", sep=""), 
           outer=T, line=-1 ) 
    
    if (save.plots) savePlot(paste(od, "Annual.SSB.MSY.", plotf, sep=''), type=plotf) 
    
    plot(years, MSY.soln[5,], xlab="Year", ylab="Recruitment at MSY", ylim=1.1*c(0, max(MSY.soln[5,])), type='l', lwd=2  ) 
    points(years, MSY.soln[5,], pch=10)
    title (paste("Annual MSY Reference Points (from S-R curve)", sep=""), 
           outer=T, line=-1 ) 
    
    if (save.plots) savePlot(paste(od, "Annual.Recr.MSY.", plotf, sep=''), type=plotf) 
    
    frep1 <-asap$options$Freport.agemin
    frep2 <-asap$options$Freport.agemax
    if (frep1==frep2) freport <-MSY.soln[1,frep1:frep2]*sel.mat[,frep1:frep2]
    if (frep2>frep1) freport <-apply(MSY.soln[1,frep1:frep2]*sel.mat[,frep1:frep2],1,mean)
    freport.label <- paste("Freport_",frep1,"-",frep2,sep="")
    MSY.soln<-  rbind(MSY.soln, freport)
    rownames(MSY.soln) <- 
      c("Fmsy", "MSY", "SPRmsy", "SSBmsy", "Rmsy", "YPRmsy",
        "Rel.SSBmsy", "Rel.Rmsy", "Conv.Code",paste("Freport_",frep1,"-",frep2,sep=""))
    
    asap.name <- a1$asap.name
    write.csv( t(MSY.soln), file=paste(od, "MSY.soln.values_", asap.name, ".csv", sep=""), row.names=T )

  } # end test for steepness phase>0
  
  return()
}  # end function

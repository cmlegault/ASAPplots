#' Plot expected spawnings
#' 
#' Plot expected number of spawnings as a function of fishing mortality rate.
#' @param asap name of the variable that read in the asap.rdat file
#' @param nyrs.ave number of years to average for calculating Reference Points 
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @export

PlotExpSpawn <- function(asap,nyrs.ave,save.plots,od,plotf){
  
  nages<- asap$parms$nages
  nyears <- asap$parms$nyears
  fec.age <- apply(asap$WAA.mats$WAA.ssb[(nyears-nyrs.ave+1):nyears,],2,mean)
  mat.age <- apply(asap$maturity[(nyears-nyrs.ave+1):nyears,],2,mean)
  wgt.age <- apply(asap$WAA.mats$WAA.catch.all[(nyears-nyrs.ave+1):nyears,],2,mean)
  M.age <- apply(asap$M.age[(nyears-nyrs.ave+1):nyears,],2,mean)
  sel.mat <- asap$F.age[(nyears-nyrs.ave+1):nyears,]/apply(asap$F.age[(nyears-nyrs.ave+1):nyears,],1,max)
  sel.age<- apply(sel.mat,2,mean)
  spawn.time <- asap$options$frac.yr.spawn
  
  F.range <- seq(0,2.0, by=0.01)
  nF <- length(F.range)
  exp.spawn.vec <- rep(0, nF)
  spr.vec <- rep(0, nF)
  
  spr0<- s.per.recr(nages=nages, fec.age=fec.age, mat.age=mat.age, M.age= M.age, F.mult=0, sel.age=sel.age, spawn.time=spawn.time)
  spawn0<- s.per.recr(nages=nages, fec.age=rep(1,nages), mat.age=mat.age, M.age= M.age, F.mult=0, sel.age=sel.age, spawn.time=spawn.time)
  # plot maturity vs selectivity based on "nyrs.ave" average
  
  par(mfrow=c(1,1), mar=c(4,4,2,4) )
  plot( seq(1,nages), mat.age, type='l', col='black', lwd=2, ylim=c(0,1.1),
        xlab="Age", ylab="Selectivity or Maturity at age")
  lines( seq(1,nages), sel.age, col='orange2', lwd=1)
  points( seq(1,nages), sel.age, col='orange2', pch=16)
  legend('topleft', legend=c("Maturity", "Selectivity"), col=c("black", "orange2"),
         lwd=c(2,1), pch=c(NA, 16) )
  if (save.plots) savePlot(paste(od, "Selectivity.vs.Maturity.", plotf, sep=''), type=plotf)
  
  for(j in 1:nF) {
    
    spawn.per.recr = s.per.recr(nages=nages, fec.age=fec.age, mat.age=mat.age, M.age= M.age, F.mult=F.range[j], sel.age=sel.age, spawn.time=spawn.time)
    spr.vec[j] = spawn.per.recr/spr0
    exp.spawn =  s.per.recr(nages=nages, fec.age=rep(1,nages), mat.age=mat.age, M.age= M.age, F.mult=F.range[j], sel.age=sel.age, spawn.time=spawn.time)
    #exp.spawn.vec[j] = exp.spawn/spawn0
    exp.spawn.vec[j] = exp.spawn
  } # end loop over F
  
  par(mfrow=c(1,1), mar=c(4,4,2,4) )
  
  plot(F.range, exp.spawn.vec, type='n', xlab="Full F", ylab="Expected Spawnings", lwd=2,
       col="skyblue3", ylim=c(0,1.2*max(exp.spawn.vec)))
  abline(v=seq(0.1,2.0, by=0.1), col="grey85")
  abline(h=c(1,2,3), col="skyblue1")
  lines(F.range, exp.spawn.vec, lwd=2, col="skyblue3" )
  points(F.range, exp.spawn.vec, pch=19, col="skyblue3" )
  scale.spr.vec <- max(spr.vec)/max(exp.spawn.vec)
  
  lines(F.range, spr.vec/scale.spr.vec, col="red", lwd=2 )
  axis(side=4, at=seq(0,1,by=0.1)/scale.spr.vec, lab=seq(0,1,by=0.1), las=2,
       col='black', col.axis="black")
  mtext(side=4, "% SPR", line=3, col="red")
  title (paste("Expected Spawnings and SPR Reference Points (Years Avg = ", nyrs.ave,")",
               sep=""), cex=0.9, outer=T, line=-1 )
  
  if (save.plots) savePlot(paste(od, "Exp.Spawn.SPR.Curves.", plotf, sep=''), type=plotf)
  
  exp.spawn.table <- as.data.frame(matrix(NA, nrow=nF, ncol=3) )
  exp.spawn.table[,1] <- F.range
  exp.spawn.table[,2] <- exp.spawn.vec
  exp.spawn.table[,3] <- spr.vec
  
  par(mfrow=c(1,1), mar=c(2,2,2,2))
  plot(seq(1,42), seq(-3,38), type='n', axes=F, bty='n',xlab="",ylab="")
  
  
  text(x=0,y=36, labels="F", font=2, pos=4)
  text(x=4, y=36, labels="E[Sp]" , font=2, pos=4,cex=0.9)
  text(x=9, y=36, labels="SPR", font=2, pos=4 )
  
  text(x=15,y=36, labels="F", font=2, pos=4)
  text(x=19, y=36, labels="E[Sp]" , font=2, pos=4,cex=0.9)
  text(x=24, y=36, labels="SPR", font=2, pos=4 )
  
  text(x=30,y=36, labels="F", font=2, pos=4)
  text(x=34, y=36, labels="E[Sp]" , font=2, pos=4,cex=0.9)
  text(x=39, y=36, labels="SPR", font=2, pos=4 )
  
  for (i in 1:35) {
    text(x=0, y=seq(35,1, by=-1), labels=F.range[1:35], cex=0.82, pos=4, font=1 )
    text(x=4, y=seq(35,1, by=-1), labels=round(exp.spawn.vec[1:35],4), cex=0.82, pos=4, font=1 )
    text(x=9, y=seq(35,1, by=-1), labels=round(spr.vec[1:35],4), cex=0.82, pos=4, font=1 )
  }
  for (i in 36:70) {
    text(x=15, y=seq(35,1, by=-1), labels=F.range[36:70], cex=0.82, pos=4, font=1)
    text(x=19, y=seq(35,1, by=-1), labels=round(exp.spawn.vec[36:70],4), cex=0.82, pos=4, font=1)
    text(x=24, y=seq(35,1, by=-1), labels=round(spr.vec[36:70],4), cex=0.82, pos=4, font=1)
  }
  for (i in 71:105) {
    text(x=30, y=seq(35,1, by=-1), labels=F.range[71:105], cex=0.82, pos=4, font=1)
    text(x=34, y=seq(35,1, by=-1), labels=round(exp.spawn.vec[71:105],4), cex=0.82, pos=4, font=1)
    text(x=39, y=seq(35,1, by=-1), labels=round(spr.vec[71:105],4), cex=0.82, pos=4, font=1)
  }
  title (paste("Expected Spawnings & SPR Reference Points (Years Avg = ", nyrs.ave,")",
               sep=""), cex=0.9, outer=T, line=-1 )
  
  if (save.plots) savePlot(paste(od, "Exp.Spawn.SPR.Table.", plotf, sep=''), type=plotf)
  
  frep1 <-asap$options$Freport.agemin
  frep2 <-asap$options$Freport.agemax
  if (frep1==frep2) freport <-F.range*sel.age[frep1:frep2]
  if (frep2>frep1) freport <-F.range*mean(sel.age[frep1:frep2])
  
  
  exp.spawn.table<- as.data.frame(cbind(F.range, exp.spawn.vec, spr.vec, freport))
  colnames(exp.spawn.table) <- c("Full.F", "Exp.Spawn", "SPR", paste("Freport_",frep1,"-",frep2,sep=""))
  write.csv( exp.spawn.table, file=paste(od,"Exp.Spawn.Table.csv", sep=""), row.names=F )
  
  return()
} # end function

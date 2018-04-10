#' Plot SPR table
#' 
#' Plot table of values associated with spawners per recruit and compute a number of per recruit reference points.
#' @param asap name of the variable that read in the asap.rdat file
#' @param a1 list file produced by grab.aux.files function
#' @param nyrs.ave number of years to average for calculating Reference Points 
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @export

PlotSPRtable <- function(asap,a1,nyrs.ave,save.plots,od,plotf){
  
  asap.name <- a1$asap.name
  spr.targ.values <- seq(0.2, 0.8, by=0.05)
  n.spr <- length(spr.targ.values)
  nages<- asap$parms$nages
  years <- seq(asap$parms$styr,asap$parms$endyr)  
  nyears <- asap$parms$nyears
  fec.age <- apply(asap$WAA.mats$WAA.ssb[(nyears-nyrs.ave+1):nyears,],2,mean)
  mat.age <- apply(asap$maturity[(nyears-nyrs.ave+1):nyears,],2,mean)
  wgt.age <- apply(asap$WAA.mats$WAA.catch.all[(nyears-nyrs.ave+1):nyears,],2,mean)
  M.age <- apply(asap$M.age[(nyears-nyrs.ave+1):nyears,],2,mean)
  sel.mat <- asap$F.age[(nyears-nyrs.ave+1):nyears,]/apply(asap$F.age[(nyears-nyrs.ave+1):nyears,],1,max)
  sel.age1<- apply(sel.mat,2,mean) 
  sel.age <- sel.age1/max(sel.age1)
  spawn.time <- asap$options$frac.yr.spawn
  jan1.age <- apply(asap$WAA.mats$WAA.jan1[(nyears-nyrs.ave+1):nyears,],2,mean)
  
  ave.pars.mat <- matrix(NA, nrow= 12 , ncol=nages)
  rownames(ave.pars.mat) <- c("M.age", "mat.age", "SSB.waa", "catch.waa",
                              "jan1.waa" , "sel.age", "spawn.time", "F30%", "F35%", "F40%","F45%","F50%")
  colnames(ave.pars.mat) <- (paste0("age",seq(1,nages) ) )
  ave.pars.mat[1,] <- M.age
  ave.pars.mat[2,] <- mat.age
  ave.pars.mat[3,] <- fec.age
  ave.pars.mat[4,] <- wgt.age
  ave.pars.mat[5,] <- jan1.age
  ave.pars.mat[6,] <- sel.age
  ave.pars.mat[7,] <- c(spawn.time, rep(NA, (nages-1))   )
  
  
  spr0<- s.per.recr(nages=nages, fec.age=fec.age, mat.age=mat.age, M.age= M.age, F.mult=0, sel.age=sel.age, spawn.time=spawn.time)
  F.start <-0.11  # starting guess for optimization routine to find F_SPR%
  
  f.spr.vals <- rep(NA, n.spr)
  ypr.spr.vals <- rep(NA, n.spr)
  ssb.spr.vals <- rep(NA, n.spr)
  conv.vals <- rep(NA, n.spr)
  
  for (i in 1:n.spr) {
    t.spr <- spr.targ.values[i]
    
    spr.f <- function(F.start) {
      abs(s.per.recr(nages=nages, fec.age=fec.age, mat.age=mat.age, M.age= M.age, F.mult=F.start, sel.age=sel.age, spawn.time=spawn.time)/spr0 - t.spr )
    }
    yyy <- nlminb(start=F.start, objective=spr.f, lower=0, upper=3)
    f.spr.vals[i] <- yyy$par
    ypr.spr.vals[i] <- ypr(nages, wgt.age=wgt.age, M.age=M.age,  F.mult=f.spr.vals[i], sel.age=sel.age )
    ssb.spr.vals[i] <- s.per.recr(nages=nages, fec.age=fec.age, mat.age=mat.age, M.age= M.age, F.mult=f.spr.vals[i], sel.age=sel.age, spawn.time=spawn.time)
    
  }  #end i-loop over SPR values
  
  ave.pars.mat[8,] <- c(f.spr.vals[3], rep(NA, (nages-1))   )    #F30%
  ave.pars.mat[9,] <- c(f.spr.vals[4], rep(NA, (nages-1))   )    #F35%
  ave.pars.mat[10,] <- c(f.spr.vals[5], rep(NA, (nages-1))   )    #F40%
  ave.pars.mat[11,] <- c(f.spr.vals[6], rep(NA, (nages-1))   )    #F45%
  ave.pars.mat[12,] <- c(f.spr.vals[7], rep(NA, (nages-1))   )    #F50%
  write.csv(ave.pars.mat, file=paste0(od, "AGEPRO_ave_params_",asap.name,".csv"), row.names=T)
  
  
  par(mfrow=c(1,1), mar=c(4,4,2,4) )
  
  plot(spr.targ.values, ypr.spr.vals, type='n', xlab="% SPR Target", ylab="Yield per Recruit", lwd=2,
       col="blue3", ylim=c(0,1.2*max(ypr.spr.vals)) )
  abline(v=seq(0.2,0.8, by=0.05), col="grey85")
  lines(spr.targ.values, ypr.spr.vals, lwd=2, col="blue3" )       
  points(spr.targ.values, ypr.spr.vals, pch=19, col="blue3" )  
  scale.f.spr <- max(f.spr.vals)/max(ypr.spr.vals)
  
  lines(spr.targ.values, f.spr.vals/scale.f.spr, col="red", lwd=2 )     
  axis(side=4, at=seq(0,1,by=0.1)/scale.f.spr, lab=seq(0,1,by=0.1), las=2,
       col='black', col.axis="black") 
  mtext(side=4, "F (%SPR)", line=3, col="red")
  
  title (paste("SPR Target Reference Points (Years Avg = ", nyrs.ave,")", sep=""), 
         outer=T, line=-1 ) 
  
  if (save.plots) savePlot(paste(od, "SPR.Target.Curves.", plotf, sep=''), type=plotf)
  
  par(mfrow=c(1,1), mar=c(2,2,2,2))
  plot(seq(1,15), seq(1,15), type='n', axes=F, bty='n',xlab="",ylab="")
  
  text(x=2,y=14, labels="% SPR", font=2, pos=4)
  text(x=5, y=14, labels="F(%SPR)" , font=2, pos=4)
  text(x=9, y=14, labels="YPR", font=2, pos=4 )
  for (i in 1:n.spr) {
    text(x=2, y=seq(n.spr,1, by=-1), labels=round(spr.targ.values,2), cex=1.0, pos=4, font=1)
    text(x=5, y=seq(n.spr,1, by=-1), labels=round(f.spr.vals,4), cex=1.0, pos=4, font=1)
    text(x=9, y=seq(n.spr,1, by=-1), labels=round(ypr.spr.vals,4), cex=1.0, pos=4, font=1)   
  }
  title (paste("SPR Target Reference Points (Years Avg = ", nyrs.ave,")", sep=""), 
         outer=T, line=-1 ) 
  
  if (save.plots) savePlot(paste(od, "SPR.Target.Table.", plotf, sep=''), type=plotf)
  
  frep1 <-asap$options$Freport.agemin
  frep2 <-asap$options$Freport.agemax
  if (frep1==frep2) freport <-f.spr.vals*sel.age[frep1:frep2]
  if (frep2>frep1) freport <-f.spr.vals*mean(sel.age[frep1:frep2])
  
  spr.target.table<- as.data.frame(cbind(spr.targ.values, f.spr.vals, ypr.spr.vals, ssb.spr.vals, freport))
  colnames(spr.target.table) <- c("%SPR", "F(%SPR)", "YPR", "SSB.PR", paste("Freport_",frep1,"-",frep2,sep=""))
  
  write.csv( spr.target.table, file=paste(od,"SPR.Target.Table_",asap.name,".csv", sep=""),  row.names=F )
  return()
} # end function

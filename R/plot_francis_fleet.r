#' Francis effective sample size plots by fleet
#' 
#' Compares input ESS with Francis (2011) estimates. Based on Francis 2011 CJFAS 68: 1124-1138.
#' @param asap name of the variable that read in the asap.rdat file
#' @param a1 list file produced by grab.aux.files function
#' @param fleet.names names of fleets 
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param is.catch.flag true means only catch plotted, false also plots discards (defaults to TRUE)
#' @export

PlotFrancisFleet<-function(asap,a1,fleet.names,save.plots,od,plotf,is.catch.flag=T) { 
  par(mfrow=c(2,1), mar=c(4,4,2,4) )
  
  years <- seq(asap$parms$styr, asap$parms$endyr) 
  nyrs <- length(years)
  
  Neff.init <- asap$fleet.catch.Neff.init
  Neff.est <- asap$fleet.catch.Neff.est
  my.title <- "Catch "
  my.save <- "Catch."
  if (!is.catch.flag){
    Neff.init <- asap$fleet.discard.Neff.init
    Neff.est <- asap$fleet.discard.Neff.est
    my.title <- "Discard "
    my.save <- "Discard."
  }
  
  for (i in 1:asap$parms$nfleets) {
    ages <- seq(asap$fleet.sel.start.age[i], asap$fleet.sel.end.age[i])
    
    t.obs<-   as.data.frame(asap$catch.comp.mats [4*(i-1)+1] )
    t.pred<-   as.data.frame(asap$catch.comp.mats [4*(i-1)+2] ) 
    catch.yrs <- which(asap$fleet.catch.Neff.init[i,]>0)
    scale.Neff= max(asap$fleet.catch.Neff.init[i,])/10
    if (!is.catch.flag){
      scale.Neff= max(asap$fleet.discard.Neff.init[i,])/10
      catch.yrs = which(asap$fleet.discard.Neff.init[i,]>0)       
      t.obs <- as.data.frame(asap$catch.comp.mats[4*(i-1)+3] )
      t.pred <- as.data.frame(asap$catch.comp.mats[4*(i-1)+4] )
    }  
    
    t.res <- matrix(NA, nrow=0, ncol=6)
    ESS1 <- Neff.init[i,]
    ESS <- ESS1[ESS1>0] 
    num.acomp.yrs <- length(catch.yrs)
    
    if (length(catch.yrs)>0){  
      for (j in catch.yrs) {
        obsf <- CalcFreqMeanSE(ages,ESS,t.obs[j,])
        prdf <- CalcFreqMeanSE(ages,ESS,t.pred[j,])
        t.res <- rbind(t.res,c(obsf[1],obsf[1]-1.96*obsf[2],obsf[1]+1.96*obsf[2],
                               prdf[1],prdf[1]-1.96*prdf[2],prdf[1]+1.96*prdf[2]))
      } # end j-loop
      
      
      #res
      Neff.bars <-  ESS1/scale.Neff
      ess.uniques <- unique(ESS)  
      len.uniques<- length(ess.uniques)
      
      #-- First plot
      par(mfrow=c(2,1), mar=c(4,4,2,4) )
      
      plot(years[catch.yrs],t.res[,4],lty=1,lwd=2,col="blue", ylim=c(0,10), xlab="Year",ylab="Mean Age", type='l')
      if (len.uniques>1) {
        segments(x0=years[catch.yrs], y0=rep(0,num.acomp.yrs), x1=years[catch.yrs], 
                 y1=Neff.bars[catch.yrs], lwd=5, col='#66BB77aa')
        axis(side=4, at=seq(0,10,2), lab=scale.Neff*seq(0,10,2), las=2 )
        mtext(side = 4, "Eff. Sample Size", line = 3, col='#66BB77')    
        lines(years[catch.yrs],t.res[,4],lty=1,lwd=2,col="blue")
      } #end test for >1 unique ESS
      
      Hmisc::errbar(years[catch.yrs],t.res[,1],t.res[,3],t.res[,2],ylim=c(0,10),add=T)
      
      
      if(len.uniques==1) title(main=paste0(my.title,"Fleet ",i, " (", fleet.names[i], ")"," ESS = ",ESS[1]), outer=F)
      if(len.uniques>1) title(main=paste0(my.title,"Fleet ",i, " (", fleet.names[i], ")"), outer=F)
      
      sdres <- (t.res[,4]-t.res[,1])/((t.res[,1]-t.res[,2])/1.96)
      sdres[sdres==Inf]  <- NA
      sdres[sdres==-Inf]  <- NA
      
      sdnr <- sd(sdres, na.rm=T)
      rmse <- sqrt(mean(sdres^2, na.rm=T))
      plotrix::barp(sdres,names.arg=years[catch.yrs],col="grey50",ylab="Std Resids",xlab="Year")
      legend('topleft',legend=c(paste0("SDNR=",round(sdnr,2)),paste0("RMSE=",round(rmse,2))),cex=0.7,h=T)
      if (save.plots) savePlot(paste0(od,my.save,"ESS_Mean_Age_Fleet_",i,".",plotf), type=plotf)
      
      
      #-- Second plot
      par(mfrow=c(1,1), mar=rep(4,4) )        
      aa <- sort(sdres)
      bb <- qqnorm(aa, plot.it = F)
      qqplot(bb$x,bb$y,xlab="Theoretical Quantile",ylab="Sample Quantiles")
      qqline(aa,col="red")
      abline(a=0,b=1,col="blue",lty=2)
      legend('topleft',legend=c("y=x","1st-3rd quartiles"),lty=c(2,1),col=c("blue","red"))
      
      if(len.uniques==1) title(main=paste0(my.title,"Fleet ",i, " (", fleet.names[i], ")"," ESS = ",ESS[1]), outer=F)
      if(len.uniques>1) title(main=paste0(my.title,"Fleet ",i, " (", fleet.names[i], ")"), outer=F)
      
      if (save.plots) savePlot(paste0(od,my.save,"ESS_QQplot_Fleet_",i,".",plotf), type=plotf)
      
    } #end test for catch.yrs
  }   # end loop on number of fleets
  
  # write csv file of Francis multipliers
  asap.name <- a1$asap.name
  
  francis.mult <- unlist(asap$Neff.stage2.mult)
  write.csv(francis.mult, file=paste0(od, "NEFF.Mult.Stage2_",asap.name,".csv"), row.names=T)
  
  return()
} #end function   


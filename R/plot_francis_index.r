#' Francis effective sample size plots by index
#' 
#' Compares input ESS with Francis (2011) estimates. Based on Francis 2011 CJFAS 68: 1124-1138.
#' @param asap name of the variable that read in the asap.rdat file
#' @param index.names names of indices 
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @export

PlotFrancisIndex <- function(asap,index.names,save.plots,od,plotf){
  
  par(mfrow=c(2,1), mar=c(4,4,2,4) )
  
  years <- seq(asap$parms$styr, asap$parms$endyr)
  ages <- seq(1,asap$parms$nages)
  cum.age.comp.flag<-0
  index.acomp.flags <- asap$control.parms$index.age.comp.flag
  ind.use <- asap$initial.guesses$index.use.flag  # NOTE: this would make more sense in the control.parm list
  
  if( sum(index.acomp.flags)>0) {
    
    
    i.seq.obs<-seq(1,2*asap$parms$nindices, by=2)
    i.seq.obs<-i.seq.obs[which(index.acomp.flags>0)]
    i.seq.pred<-seq(2,2*asap$parms$nindices, by=2)
    i.seq.pred<-i.seq.pred[which(index.acomp.flags>0)]
    i.neff.seq.obs<-seq(1,sum(index.acomp.flags))
    
    for (i in 1:asap$parms$nindices) {
      
      #if (index.acomp.flags[i]==1)  {
      if (index.acomp.flags[i]==1 & sum(asap$index.Neff.init[i,])>1)  {
        
        cum.age.comp.flag <- sum(index.acomp.flags[1:i])
        
        t.obs<-   as.data.frame(asap$index.comp.mats [i.seq.obs[cum.age.comp.flag] ] )
        t.pred<-   as.data.frame(asap$index.comp.mats [i.seq.pred[cum.age.comp.flag] ] ) 
        
        t.res <- matrix(NA, nrow=0, ncol=6)
        
        #ESS1 <- asap$index.Neff.init [i.neff.seq.obs[cum.age.comp.flag], ] 
        ESS1 <- asap$index.Neff.init [i, ] 
        ESS <- ESS1[ESS1>0] 
        scale.Neff= max(ESS, na.rm=T)/10
        
        
        iyrs <- as.matrix(as.data.frame(asap$index.year [i]) )  
        num.ind.yrs <- length(iyrs)  # total number of years in model                                                   
        acyrs1 <-  which(years %in% iyrs)  # position of iyrs in vector of years 
        acyrs2 <- acyrs1[iyrs %in% years[ESS1>0] ] # position of iyrs in years where age comp exists
        
        num.acyrs2 <- length(acyrs2)  # number of age comp years 
        
        
        for (j in 1:num.acyrs2 ){
          #for (j in 1:num.ind.yrs ){  
          jj <- acyrs2[j]
          obsf <- CalcFreqMeanSE(ages,ESS[j],as.matrix(t.obs[jj,]) )
          prdf <- CalcFreqMeanSE(ages,ESS[j],t.pred[jj,])
          t.res <- rbind(t.res,c(obsf[1],obsf[1]-1.96*obsf[2],obsf[1]+1.96*obsf[2],
                                 prdf[1],prdf[1]-1.96*prdf[2],prdf[1]+1.96*prdf[2]))
        } # end j loop
        
        ess.uniques <- unique(ESS)  
        len.uniques<- length(ess.uniques)
        Neff.bars <-  ESS/scale.Neff
        
        
        #-- First plot
        par(mfrow=c(2,1), mar=c(4,4,2,4) )    
        plot(years[acyrs2],t.res[,4],lty=1,lwd=2,col="blue", ylim=c(0,10), ylab="Mean Age", 
             type='l', xlab="Years")
        if (len.uniques>1) {
          segments(x0=years[acyrs2], y0=rep(0,num.acyrs2), x1=years[acyrs2], y1=Neff.bars, lwd=5, col='#66BB77aa')
          axis(side=4, at=seq(0,10,2), lab=scale.Neff*seq(0,10,2), las=2 )
          mtext(side = 4, "Eff. Sample Size", line = 3, col='#66BB77')    
          lines(years[acyrs2],t.res[,4],lty=1,lwd=2,col="blue")
        } #end test for >1 unique ESS
        
        Hmisc::errbar(years[acyrs2], t.res[,1],t.res[,3],t.res[,2],ylim=c(0,10),ylab="Mean Age", 
               xlab="Years", add=T)
        
        if(len.uniques==1)    title(main=paste("Index ",i, " (",index.names[i],")", " ESS = ",ESS[i], sep=""), outer=F)
        if(len.uniques>1)    title(main=paste("Index ",i, " (",index.names[i],")",  sep=""), outer=F)
        
        
        sdres <- (t.res[,4]-t.res[,1])/((t.res[,1]-t.res[,2])/1.96)
        sdres[sdres==Inf]  <- NA
        sdres[sdres==-Inf]  <- NA
        sdnr <- sd(sdres, na.rm=T)
        rmse <- sqrt(mean(sdres^2, na.rm=T))
        plotrix::barp(sdres,names.arg=years[acyrs2],col="grey50",ylab="Std Resids",xlab="Year")
        legend('topleft',legend=c(paste("SDNR=",round(sdnr,2), sep=""),paste("RMSE=",round(rmse,2), sep="")),cex=0.7,h=T)
        
        if (save.plots) savePlot(paste(od,"Francis_Mean_Age_Orig_Ind_",i,".",plotf, sep=""), type=plotf)
        
        
        #-- Second plot
        par(mfrow=c(1,1), mar=rep(4,4) )      
        aa <- sort(sdres)
        bb <- qqnorm(aa, plot.it=F)
        qqplot(bb$x,bb$y,xlab="Theoretical Quantile",ylab="Sample Quantiles")
        qqline(aa,col="red")
        abline(a=0,b=1,col="blue",lty=2)
        legend('topleft',legend=c("y=x","1st-3rd quartiles"),lty=c(2,1),col=c("blue","red"))
        
        if(len.uniques==1)    title(main=paste("Index ",i, " (",index.names[i],")", " ESS = ",ESS[i], sep=""), outer=F)
        if(len.uniques>1)    title(main=paste("Index ",i, " (",index.names[i],")",  sep=""), outer=F)
        if (save.plots) savePlot(paste(od,"Francis_QQ_Orig_Ind_",i,".",plotf, sep=""), type=plotf)
        
        
      }  #end test for presence/absence of age-comp  
    } # end loop on nindices
  }  #end test for index age comp  
  
  return()
} # end function

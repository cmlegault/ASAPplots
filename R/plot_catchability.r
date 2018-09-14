#' Plot survey catchability      
#' 
#' Barplots of survey q with approximate confidence intervals and time series plots of q values (if variable).
#' @param asap name of the variable that read in the asap.rdat file
#' @param index.names names of indices
#' @param a1 list file produced by grab.aux.files function
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param liz.palette color definitions
#' @export
  
PlotCatchability <- function(asap,index.names,a1,save.plots,od,plotf,liz.palette){
  if (is.na(a1$asap.std)){
    return() # skip plot if no CI available
  }
  
  par(mfrow=c(1,1), mar=c(6,4,2,2) )
  
  years=seq(asap$parms$styr, asap$parms$endyr)
  nyrs=asap$parms$nyears
  ind.use <-  which(asap$initial.guesses$index.use.flag>0)
  num.q <- length(ind.use)
  n.ind=asap$parms$nindices
  yaxis.q<-  ifelse(num.q<7, 1.1, 1.2)
  q.info <- asap$q.indices
  asap.std <- a1$asap.std
  log.q.pars = asap.std[asap.std$name=="log_q_year1",]  
  log.q.mean <- log.q.pars[,3]
  log.q.var <- log.q.pars[,4]^2
  q.std <- sqrt((exp(log.q.var)-1)*(exp(2*log.q.mean+log.q.var))  )
  
  if(asap$control.parms$phases$phase.q.devs<=0) {
    
    se.lines=matrix(NA, nrow=num.q, ncol=2)
    
    
    se.lines[,1]=q.info-2*q.std
    se.lines[,2]=q.info+2*q.std
    
    barplot( q.info,  beside=T, axisnames=F, cex.names=0.70 , 
             width=1.0, space=rep(0,num.q), xpd=F,
             xlab = '', ylab ='Catchability (q +/- 2 SE)', 
             ylim = c(0,1.05*max(se.lines[,2], na.rm=TRUE)), 
             xlim=c(0,num.q), col="lightblue3")
    box()
    axis(side=1, las=2, at=seq(0.5, (num.q-0.5) ),  
         labels=index.names, cex=0.70, las=2)
    
    Hmisc::errbar( seq(0.5,(num.q -0.5)  , by=1), yplus=se.lines[,2], 
            as.numeric(q.info),
            yminus=se.lines[,1], add=T, lty=1, pch='.' )
    
    if (save.plots==T) savePlot( paste(od, "Q.Index.Err.Bars.",plotf, sep=""), type=plotf)      
  } # end block for no q-devs
  
  
  if(asap$control.parms$phases$phase.q.devs>0) { 
    cc <- 0  
    num.x.lims <- trunc(n.ind/5) +1
    x.lims <- matrix(NA, num.x.lims, 2)
    for (ix in 1:num.x.lims) {
      if (ix < num.x.lims)  x.lims[ix,1] <- min( as.vector(unlist(asap$index.year [seq((5*(ix-1)+1),5*ix)]) ))
      if (ix==num.x.lims) x.lims[ix,1] <- min( as.vector(unlist(asap$index.year [seq((5*(ix-1)+1),n.ind)])) )
      
      if (ix < num.x.lims)  x.lims[ix,2] <- max( as.vector(unlist(asap$index.year [seq((5*(ix-1)+1),5*ix)]) ))
      if (ix==num.x.lims) x.lims[ix,2] <- max( as.vector(unlist(asap$index.year [seq((5*(ix-1)+1),n.ind)])) )
      
    }  # end ix loop
    
    i.lty <- seq(1,5)
    
    for (i in 1: num.q) {
      t.yrs <- as.vector(unlist(asap$index.year [i]))
      t.q  <- unlist(asap$q.random.walk [i])
      if (i%%5 == 1 )   {
        i.x.lims <- trunc(i/5) +1
        plot( t.yrs, t.q, type='l', lty=i.lty[i%%5],xlim=c(x.lims[i.x.lims,1], x.lims[i.x.lims,2]),
              col=liz.palette[i], lwd=2, xlab="Year", ylab="Catchability", 
              ylim=c(0, yaxis.q*max(unlist(asap$q.random.walk )) )   )
        cc = cc+1      
      }
      
      if (i %% 5 %in% c(2,3,4) )  lines(t.yrs, t.q, lty=i.lty[i%%5], col=liz.palette[i], lwd=2 )
      if (i %% 5 ==0 )  lines(t.yrs, t.q, lty=i.lty[5], col=liz.palette[i], lwd=2 )   
      
      
      if(i %% 5==0 )  {
        #legend("top", legend=ind.use[(i-4):i], lty=seq(1,5), 
        #              col=liz.palette[(i-4):i], horiz=T , lwd=rep(2,5))   
        legend("top", legend=index.names[(i-4):i], lty=seq(1,5), 
               col=liz.palette[(i-4):i], horiz=T , lwd=rep(2,5), cex=0.8)    
        
        title ( "Index q estimates", outer=T, line=-1 )
        if (save.plots==T) savePlot( paste(od, "Q.Index.Yr.",cc,".",plotf, sep=""), type=plotf)      
      }  # end modulo test of 5 q's per plot
      
      if(i == n.ind & n.ind%%5>0 )  {
        #legend("top", legend=ind.use[(i-(n.ind%%5)+1):i], lty=seq(1,5), 
        #              col=liz.palette[(i-(n.ind%%5)+1):i], horiz=T, lwd=rep(2,5) )    
        legend("top", legend=index.names[(i-(n.ind%%5)+1):i], lty=seq(1,5), 
               col=liz.palette[(i-(n.ind%%5)+1):i], horiz=T, lwd=rep(2,5), cex=0.8 )    
        
        title ( "Index q estimates", outer=T, line=-1 )
        if (save.plots==T) savePlot( paste(od, "Q.Index.Yr.",cc,".",plotf, sep=""), type=plotf)      
      }  # end modulo test of 5 q's per plot
      
    } #end loop over number of q parameters    
    
  }  #end if test for q.dev phase >0
  
  return()
} # end function


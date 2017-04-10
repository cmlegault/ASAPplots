#' Bubble plots of catch age comp resids 
#' 
#' Pearson residuals for age composition for all years in bubble plot.
#' @param asap name of the variable that read in the asap.rdat file
#' @param fleet.names names of fleets 
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param scale.catch.bubble.resid larger values increase size of catch age comp bubbles (defaults to 2)
#' @param is.catch.flag true means only catch plotted, false also plots discards (defaults to TRUE)
#' @export

PlotCatchAgeCompResids <- function(asap,fleet.names,save.plots,od,plotf,
                                   scale.catch.bubble.resid=2,is.catch.flag=TRUE){
  par(mar=c(4,4,2,2), oma=c(1,1,1,1), mfrow=c(1,1))
  
  pos.resid.col <- "#ffffffaa"
  neg.resid.col <- "#ff1111aa"
  
  ages=seq(1, asap$parms$nages)
  nages=length(ages)
  years=seq(asap$parms$styr, asap$parms$endyr)
  nyrs=asap$parms$nyears
  
  for (i in 1:asap$parms$nfleets) {
    acomp.obs1 <- as.data.frame(asap$catch.comp.mats[4*(i-1)+1])
    ##New - replace observed zeros with NA
    #acomp.obs <- apply(acomp.obs1, 2, function(x)  replace(x, which(x==0 ), NA) )
    acomp.obs <- acomp.obs1
    
    acomp.pred <- as.data.frame(asap$catch.comp.mats[4*(i-1)+2]) 
    catch.yrs <- which(asap$fleet.catch.Neff.init[i,]>0)
    my.title <- "Age Comp Residuals for Catch by Fleet "
    my.save <- "catch.resid.bubble.plots."
    if (!is.catch.flag){
      acomp.obs <- as.data.frame(asap$catch.comp.mats[4*(i-1)+3])
      acomp.pred <- as.data.frame(asap$catch.comp.mats[4*(i-1)+4]) 
      catch.yrs <- which(asap$fleet.discard.Neff.init[i,]>0)
      my.title <- "Age Comp Residuals for Discards by Fleet "
      my.save <- "discard.resid.bubble.plots."
    }
    acomp.catch.resids <- matrix(NA, nrow=nyrs, ncol=nages)
    ###NEW  
    acomp.sd <- matrix(NA, nrow=nyrs, ncol=nages)
    if (length(catch.yrs)>0){
      s.age <- asap$fleet.sel.start.age[i]
      e.age <- asap$fleet.sel.end.age[i]
      for (j in 1:length(catch.yrs)){
        resids <- as.numeric(acomp.obs[catch.yrs[j],] - acomp.pred[catch.yrs[j],])  # NOTE obs-pred
        acomp.catch.resids[as.numeric(catch.yrs[j]),s.age:e.age] <- resids[s.age:e.age]
        ###NEW  
        tmp.sd <- as.numeric(sqrt(acomp.pred[catch.yrs[j],]*(1-acomp.pred[catch.yrs[j],])/asap$fleet.catch.Neff.init[i, catch.yrs[j]] ) )
        acomp.sd[as.numeric(catch.yrs[j]),s.age:e.age] <- tmp.sd[s.age:e.age]
      }
      z1 <- acomp.catch.resids
      range.resids<-range(abs((as.vector(z1))), na.rm=T)
      scale.resid.bubble.catch <- 25
      
      
      
      z3 <- z1 * scale.resid.bubble.catch *scale.catch.bubble.resid
      resid.col=matrix(NA, nrow=nyrs, ncol=nages)   # set color for residual bubbles
      resid.col <- ifelse(z3 > 0.0,pos.resid.col, neg.resid.col)
      
      ###NEW  
      ##   calculate age comp bubble resids as pearson standardized resids
      zr <- z1/acomp.sd     

      ###NEW  
      par(mar=c(5,4,2,2), oma=c(1,1,1,1), mfrow=c(1,1))     
      ##   plot age comp bubble resids as pearson standardized resids
      plot(ages, rev(ages),  xlim = c(1, nages), ylim = c(years[nyrs],(years[1]-2)), 
           xlab = "Age", ylab = "Pearson Residuals (Obs-Pred)/SQRT(Pred*(1-Pred)/NESS)", type = "n", axes=F)
      axis(1, at= ages, lab=ages)
      axis(2, at = rev(years), lab = rev(years), cex.axis=0.75, las=1)
      box()
      abline(h=years, col="lightgray")
      segments(x0=seq(ages[1], nages), y0=rep(years[1],nages),
               x1=seq(ages[1], nages), y1=rep(years[nyrs],nages), col = "lightgray", lty = 1)
      
      for (j in 1:nyrs){
        ## New  - use triangle instead of circle for resids where Obs=0
        #pch.resid <- ifelse(acomp.obs[j,]>0, 21, 24)  #use diff symbol
        pch.resid <- ifelse(acomp.obs[j,]>0, 21, 21)   #use same symbol
        
        points(ages, rep((years[1]+j-1), nages), cex=abs(zr[j,])* scale.catch.bubble.resid,
               col="black", bg = resid.col[j,],  pch = pch.resid )
      }
      
      tmp.zr <- matrix((abs(zr)),nrow=1, ncol=length(zr[,1])*length(zr[1,])  )
      tmp.zr1 <- tmp.zr[1,]
      bubble.legend.pearson<- summary(tmp.zr1, na.rm=T) [c(2,3,5)]
      
      legend("topright", xpd=T, legend=round(bubble.legend.pearson,2), pch=rep(1, 3), 
             pt.cex=as.numeric(bubble.legend.pearson)*  scale.catch.bubble.resid,
             horiz=T , col='black'  )
      legend("topleft", xpd=T, legend=c("Neg.", "Pos."), pch=rep(21, 2), pt.cex=3,
             horiz=T , pt.bg=c(neg.resid.col, pos.resid.col), col="black"  )
      text(x= trunc(nages/2), y=(years[1]-1),   cex=0.8,
           label=paste("Max(resid)=",round(max(abs(zr), na.rm=T),2), sep="") )
      
      title (paste(my.title,i, " (", fleet.names[i], ")", sep=""), 
             outer=T, line=-1 ) 
      title(sub=paste("Mean resid = ", round(mean(zr, na.rm=T ),2), "   SD(resid) = ", 
                      round(sd(as.vector(zr), na.rm=T ),2), sep=""), col.sub='blue', cex.sub=0.8)
      
      if (save.plots) savePlot(paste(od, "Pearson.", my.save, i, ".", plotf, sep=''), type=plotf)
      
      
      
    } # end catch.yrs test  
  }   #end loop nfleets
  return()
}


#' Plot of RMSE values with 95 percent CI cone
#' 
#' Root mean square error by likelihood component relative to 95 percent CI from N(0,1) distribution.
#' @param asap name of the variable that read in the asap.rdat file
#' @param index.names names of indices
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param liz.palette color definitions
#' @export
 
PlotRMSE95CI <- function(asap,index.names,save.plots,od,plotf,liz.palette){
  par(mfrow=c(2,1), mar=c(4,4,2,2) )#need to specify 2/3 layout
  
  max.txt<-16
  ci.vals <- matrix(NA, 9,3)
  ci.vals[,1] <- c(1,3,5,10,20,30,40,50, 100)
  ci.vals[,2] <- c(0.063, 0.348, 0.473, 0.634, 0.737, 0.786, 0.815, 0.832, 0.883)
  ci.vals[,3] <- c(1.96, 1.619, 1.487, 1.351, 1.253, 1.211, 1.183, 1.162, 1.116)
  
  #-- plot of just indices
  rmse.val <- as.vector(unlist(asap$RMSE))
  rmse.num <- as.vector(unlist(asap$RMSE.n))
  rmse.names <- names(asap$RMSE)
  rmse.ind1 <- which(substr(rmse.names,1,8)=="rmse.ind")
  rmse.ind <- rmse.ind1[which(rmse.val[rmse.ind1]>0)]
  tname1 <- rmse.names[rmse.ind]
  tname <- substr(tname1,6, max(nchar(tname1))  )
  tname[1:length(index.names)] <- index.names
  
  
  if (length(rmse.ind)>0) {
    
    plot(seq(0,100), rep(1, 101), lty=1, type='l', ylim=c(0,2), xlab="Number of Residuals",
         ylab="RMSE")
    lines(ci.vals[,1], ci.vals[,2], lty=2, lwd=2)
    lines(ci.vals[,1], ci.vals[,3], lty=2, lwd=2)
    
    points(rmse.num[rmse.ind], rmse.val[rmse.ind], pch=(seq(1,length(rmse.ind)) %% 20), 
           col=liz.palette[seq(1,length(rmse.ind))], cex=1.5 )
    
    
    plot(seq(1,20), seq(1,20), type='n', axes=F, xlab="", ylab="")
    if (length(rmse.ind)<=20) {
      text( x= rep(1,length(rmse.ind)), y=seq(1, length(rmse.ind)), 
            labels=tname,   pos=4, cex=0.8)
      points(x=rep(0.5, length(tname)), y=seq(1, length(tname)), 
             pch=seq(1,length(tname)), col=liz.palette[seq(1,length(tname))] )
    }
    if (length(rmse.ind)>20 & length(rmse.ind)<=40) {
      text( x= rep(1,20), y=seq(1, 20), labels=tname[1:20],
            pos=4, cex=0.8)
      text( x= rep(7,(length(tname)-20)), y=seq(1, (length(tname)-20)), 
            labels=tname[21:length(tname)], pos=4, cex=0.8)
      points(x=rep(0.5, 20), y=seq(1, 20), 
             pch=seq(1,20), col=liz.palette[seq(1,20)] )
      points(x=rep(6.5, (length(tname)-20)), y=seq(1,(length(tname)-20)), 
             pch=(seq(21,length(tname)) %% 20), col=liz.palette[seq(21, length(tname))] )
    }
    if (length(rmse.ind)>40 & length(rmse.ind)<=60) {
      text( x= rep(1,20), y=seq(1, 20), labels=tname[1:20], pos=4, cex=0.8)
      text( x= rep(7,20), y=seq(1,20), labels=tname[21:40], pos=4, cex=0.8)
      text( x= rep(13,(length(tname)-40)), y=seq(1,(length(tname)-40)), 
            labels=tname[41:length(tname)], pos=4, cex=0.8)
      points(x=rep(0.5, 20), y=seq(1, 20), 
             pch=seq(1,20), col=liz.palette[seq(1,20)] )
      points(x=rep(6.5,20), y=seq(1,20), 
             pch=(seq(21, 40) %% 20), col=liz.palette[seq(21,40)] )
      points(x=rep(12.5, (length(tname)-40)), y=seq(1,(length(tname)-40)), 
             pch=(seq(41, length(tname)) %% 20), col=liz.palette[seq(41, length(tname))] )
    }
    
    title(main="Root Mean Square Error for Indices", outer=T, cex=0.85, line=-1)
    if (save.plots) savePlot(paste(od, "RMSE.95CI.Indices.",plotf, sep=""), type=plotf)
  }    # end if-statement for rmse.ind
  
  
  #-- plot of just catch
  rmse.val <- as.vector(unlist(asap$RMSE))
  rmse.num <- as.vector(unlist(asap$RMSE.n))
  rmse.names <- names(asap$RMSE)
  rmse.cat1 <- which(substr(rmse.names,1,8)=="rmse.cat" | substr(rmse.names,1,8)=="rmse.dis")
  rmse.cat <- rmse.cat1[which(rmse.val[rmse.cat1]>0)]
  tname1 <- rmse.names[rmse.cat]
  tname <- substr(tname1,6, max(nchar(tname1))  )
  #if (asap$parms$nfleets>1 ) tname <- c(fleet.names, "catch.tot")
  
  if (length(rmse.cat)>0)  {
    
    par(mfrow=c(2,1) )#need to specify 2/3 layout
    plot(seq(0,100), rep(1, 101), lty=1, type='l', ylim=c(0,2), xlab="Number of Residuals",
         ylab="RMSE")
    lines(ci.vals[,1], ci.vals[,2], lty=2, lwd=2)
    lines(ci.vals[,1], ci.vals[,3], lty=2, lwd=2)
    
    points(rmse.num[rmse.cat], rmse.val[rmse.cat], pch=seq(1,length(rmse.cat)), 
           col=liz.palette[seq(1,length(rmse.cat))], cex=1.5 )
    
    
    plot(seq(1,20), seq(1,20), type='n', axes=F, xlab="", ylab="")
    if (length(rmse.cat)<=20) {
      text( x= rep(1,length(rmse.cat)), y=seq(1, length(rmse.cat)), 
            labels=tname,   pos=4, cex=0.8)
      points(x=rep(0.5, length(tname)), y=seq(1, length(tname)), 
             pch=seq(1,length(tname)), col=liz.palette[seq(1,length(tname))] )
    }
    if (length(rmse.cat)>20 & length(rmse.cat)<=40) {
      text( x= rep(1,20), y=seq(1, 20), labels=tname[1:20],
            pos=4, cex=0.8)
      text( x= rep(7,(length(tname)-20)), y=seq(1, (length(tname)-20)), 
            labels=tname[21:length(tname)], pos=4, cex=0.8)
      points(x=rep(0.5, 20), y=seq(1, 20), 
             pch=seq(1,20), col=liz.palette[seq(1,20)] )
      points(x=rep(6.5, (length(tname)-20)), y=seq(1,(length(tname)-20)), 
             pch=(seq(21,length(tname)) %% 20), col=liz.palette[seq(21,length(tname))] )
    }
    if (length(rmse.cat)>40 & length(rmse.cat)<=60) {
      text( x= rep(1,20), y=seq(1, 20), labels=tname[1:20], pos=4, cex=0.8)
      text( x= rep(7,20), y=seq(1,20), labels=tname[21:40], pos=4, cex=0.8)
      text( x= rep(13,(length(tname)-40)), y=seq(1,(length(tname)-40)), 
            labels=tname[41:length(tname)], pos=4, cex=0.8)
      points(x=rep(0.5, 20), y=seq(1, 20), 
             pch=seq(1,20), col=liz.palette[seq(1,20)] )
      points(x=rep(6.5,20), y=seq(1,20), 
             pch=(seq(21,40) %% 20), col=liz.palette[seq(21,40)] )
      points(x=rep(12.5, (length(tname)-40)), y=seq(1,(length(tname)-40)), 
             pch=(seq(41, length(tname)) %% 20), col=liz.palette[seq(41, length(tname))] )
    }
    
    title(main="Root Mean Square Error for Catch", outer=T, cex=0.85, line=-1)
    if (save.plots) savePlot(paste(od, "RMSE.95CI.Catch.",plotf, sep=""), type=plotf)
  }  # end if-statement for rmse.cat
  
  
  
  #-- plot of parameters with priors
  rmse.val <- as.vector(unlist(asap$RMSE))
  rmse.num <- as.vector(unlist(asap$RMSE.n))
  rmse.names <- names(asap$RMSE)
  #rmse.prior1 <- which(substr(rmse.names,1,8)=="rmse.cat" | substr(rmse.names,1,8)=="rmse.dis")
  rmse.prior1 <- rmse.names[c(-rmse.cat1, -rmse.ind1)]
  rmse.prior <- rmse.prior1[which(rmse.val[rmse.prior1]>0)]
  
  if (length(rmse.prior)>0) {
    par(mfrow=c(2,1) )#need to specify 2/3 layout
    plot(seq(0,100), rep(1, 101), lty=1, type='l', ylim=c(0,2), xlab="Number of Residuals",
         ylab="RMSE")
    lines(ci.vals[,1], ci.vals[,2], lty=2, lwd=2)
    lines(ci.vals[,1], ci.vals[,3], lty=2, lwd=2)
    
    points(rmse.num[rmse.prior], rmse.val[rmse.prior], pch=seq(1,length(rmse.prior)), 
           col=liz.palette[seq(1,length(rmse.prior))], cex=1.5 )
    
    plot(seq(1,20), seq(1,20), type='n', axes=F, xlab="", ylab="")
    text( x= rep(1,length(rmse.prior)), y=seq(1, length(rmse.prior)), labels=rmse.names[rmse.prior],
          pos=4)
    points(x=rep(0.5, length(rmse.prior)), y=seq(1, length(rmse.prior)), 
           pch=seq(1,length(rmse.prior)), col=liz.palette[seq(1,length(rmse.prior))] )
    title(main="Root Mean Square Error for Priors", outer=T, cex=0.85)
    if (save.plots) savePlot(paste(od, "RMSE.95CI.Prior.",plotf, sep=""), type=plotf)
  }
  return()
} #end function


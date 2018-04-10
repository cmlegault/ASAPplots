#--------------  RETROSPECTIVE FUNCTIONS -----------------------
# set of functions to compute Mohn's rho and make retrospective plots

get.retro <- function(asap.name,asap){
  # get the number of peels from the rts file
  asap.name.short <- substr(asap.name, 1, (nchar(asap.name)-4) )  # removing "_000"
  asap.rts <- paste0(asap.name.short,".rts")  
  npeels <- scan(asap.rts,n=1) # includes base run as one of the peels 

  retro <- list()
  retro$nyears <- asap$parms$nyears
  retro$nages <- asap$parms$nages
  retro$favg <- matrix(NA, nrow=asap$parms$nyears, ncol=npeels)
  retro$ssb <- matrix(NA, nrow=asap$parms$nyears, ncol=npeels)
  retro$jan1b <- matrix(NA, nrow=asap$parms$nyears, ncol=npeels)
  retro$explb <- matrix(NA, nrow=asap$parms$nyears, ncol=npeels)
  retro$stockn <- matrix(NA, nrow=asap$parms$nyears, ncol=npeels)
  retro$stock_age <- list()
  for (j in 1:asap$parms$nages){
    retro$stock_age[[j]] <- matrix(NA, nrow=asap$parms$nyears, ncol=npeels)
  }
  
  for (i in 0:(npeels-1)){
    if (i < 10) i3 <- paste("00",i, sep="")
    else if(i < 100) i3 <- paste("0",i, sep="")
    r1 <- paste(asap.name.short,'_',i3,'.rdat', sep="")
    rr <- dget(r1)
    retro$favg[1:rr$parms$nyears,(npeels-i)] <- rr$F.report
    retro$ssb[1:rr$parms$nyears,(npeels-i)] <- rr$SSB
    retro$jan1b[1:rr$parms$nyears,(npeels-i)] <- rr$tot.jan1.B  
    retro$explb[1:rr$parms$nyears,(npeels-i)] <- rr$exploitable.B        
    retro$stockn[1:rr$parms$nyears,(npeels-i)] <- apply(rr$N.age,1,sum)
    for (j in 1:asap$parms$nages){
      retro$stock_age[[j]][1:rr$parms$nyears,(npeels-i)] <- rr$N.age[,j]
    }
  }
  return(retro)
}

#-------------------------------------
plot.retro <- function(retro.mat,years,nages,y.lab,y.range1=NA,y.range2=NA){
  nyears <- length(years)
  ncols <- length(retro.mat[1,])
  npeels <- ncols-1
  # standard retro plot
  if(is.na(max(y.range1))) y.range1 <- range(retro.mat, na.rm=T)
  plot(years,retro.mat[,ncols],lwd=2,col="blue",type='l',xlab="Year",ylab=y.lab,ylim=y.range1)
  for (i in 1:npeels){
    lines(years,retro.mat[,(ncols-i)],type='l')
    points(years[nyears-i],retro.mat[nyears-i,ncols-i],pch=16,col="red") 
  }
  
  # relative retro plot
  rel.mat <- retro.mat
  for (i in 1:nyears){
    rel.mat[i,] <- (retro.mat[i,] - retro.mat[i,ncols]) / retro.mat[i,ncols]
  }
  if(is.na(max(y.range2))) y.range2 <- range(rel.mat, na.rm=T)
  plot(years,rel.mat[,ncols],lwd=2,col="blue",type='l',xlab="Year",ylab="Relative Retro",ylim=y.range2)
  rho.vals<-rep(NA,(npeels+1))
  mm <- 0  
  for (i in 1:npeels){
    lines(years,rel.mat[,(ncols-i)],type='l')
    points(years[nyears-i],rel.mat[nyears-i,ncols-i],pch=16,col="red")
    mm <- mm+rel.mat[nyears-i,ncols-i] 
    rho.vals[i] <- rel.mat[nyears-i,ncols-i]
  }
  mohn.rho <- mm/npeels
  rho.vals[(npeels+1)] <- mohn.rho
  text(x=years[1], y=0.9*y.range2[2], label=expression(paste(rho,"= ",sep="")), pos=4)
  text(x=(years[1]+3), y=0.9*y.range2[2], label=round(mohn.rho,3), pos=4)
  return(rho.vals)
}

#------------------------------------------------------------------------------------
#' Plot retrospectives
#' 
#' Plots both standard and relative retrospectives and computes Mohn's rho values. Uses functions get.retro and plot.retro.
#' @param asap.name Base name of original dat file (without the .dat extension)
#' @param asap name of the variable that read in the asap.rdat file
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @export

PlotRetroWrapper <- function(asap.name,asap,save.plots,od,plotf){
  years <- seq(asap$parms$styr,asap$parms$endyr)
  my <- get.retro(asap.name,asap)
  par(mfrow=c(3,2),mar=c(4,4,4,2) )
  # F, SSB, R
  f.rho<-plot.retro(my$favg,years,asap$parms$nages,"Average F",y.range1=c(0,max(my$favg, na.rm=T)))
  ssb.rho<-plot.retro(my$ssb,years,asap$parms$nages,"SSB",y.range1=c(0,max(my$ssb, na.rm=T)))
  recr.rho<-plot.retro(my$stock_age[[1]],years,asap$parms$nages,"Recruitment",y.range1=c(0,max(my$stock_age[[1]], na.rm=T)))
  title(main="F, SSB, R", outer=T, line=-1)
  if(save.plots) savePlot(paste(od,"retro_F_SSB_R.",plotf, sep=""), type=plotf)
  
  # Jan-1 B, Exploitable B, Total Stock N
  jan1b.rho<-plot.retro(my$jan1b,years,asap$parms$nages,"Jan-1 B",y.range1=c(0,max(my$jan1b, na.rm=T)))
  explb.rho<-plot.retro(my$explb,years,asap$parms$nages,"Exploitable B",y.range1=c(0,max(my$explb, na.rm=T)))
  stockn.rho<-plot.retro(my$stockn,years,asap$parms$nages,"Total Stock N",y.range1=c(0,max(my$stockn, na.rm=T)))
  title(main="Jan-1 B, Exploitable B, Total Stock N", outer=T, line=-1)
  if(save.plots) savePlot(paste(od,"retro_Jan1B_ExplB_StockN.",plotf, sep=""), type=plotf)  
  
  # Population numbers at age
  age.rho <- matrix(NA,1,asap$parms$nages)
  for (j in 1:asap$parms$nages){
    temp.rho<-plot.retro(my$stock_age[[j]],years,asap$parms$nages,paste("N at Age ",j, sep=""),y.range1=c(0,max(my$stock_age[[j]], na.rm=T)))
    if (j==1) age.rho<- (temp.rho )
    if (j>1) age.rho<-rbind(age.rho, t(temp.rho) )    
    title(main="Stock Numbers at Age", outer=T, line=-1)
    if (j%%3==0){
      if(save.plots) savePlot(paste(od,"retro_ages_",j-2,"_",j,".",plotf, sep=""), type=plotf)
    }
  }  # end loop over ages
  age.rho<-as.data.frame(t(age.rho))
  colnames(age.rho) <- c(paste(rep("Age",asap$parms$nages),seq(1,asap$parms$nages), sep="."))
  if (asap$parms$nages%%3==1){
    if(save.plots) savePlot(paste(od,"retro_ages_",j,".",plotf, sep=""), type=plotf)
  }
  if (asap$parms$nages%%3==2){
    if(save.plots) savePlot(paste(od,"retro_ages_",j-1,"_",j,".",plotf, sep=""), type=plotf)
  }
  
  par(mfrow=c(1,1))
  Retro.rho <-cbind(f.rho, ssb.rho, recr.rho, jan1b.rho, explb.rho, stockn.rho, age.rho)
  rownames(Retro.rho) <- c(paste("peel", seq(1,(length(f.rho)-1)), sep="."),"Mohn.rho"  )
  
  write.csv(Retro.rho, file=paste(od, "Retro.rho.values_",asap.name,".csv",sep=""),   row.names=T)
  return()
}


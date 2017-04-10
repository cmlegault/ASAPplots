#' SARC style plot of recruitment and SSB
#' 
#' Recruitment lagged to match SSB (recr years 2:nyears; SSB years 1:(nyears-1)).
#' @param asap name of the variable that read in the asap.rdat file
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @export

PlotSARCrecSSB <- function(asap,save.plots,od,plotf){
  
  par( mar = c(5,5,1,5), oma = c(0,0,0,1), family='serif')   
  
  years = seq(asap$parms$styr, asap$parms$endyr)                       
  nyears<-asap$parms$nyears
  ssb <- asap$SSB
  recr <- asap$N.age[,1]
  ssb.plot<- ssb[1:(nyears-1)]
  recr.plot<- recr[2:nyears]
  yr.text=substr(years,3,4)
  
  scale.ssb=1
  scale.recruits=1
  
  scale.r= max(ssb.plot/scale.ssb)/max(recr.plot/scale.recruits)
  barplot( recr.plot/scale.recruits*scale.r , beside=T,axisnames=F , width=1, 
           space=rep(0,nyears-1), offset=rep(-0.5,nyears-1),  axes=F, xpd=F,
           xlab = '', ylab ='', ylim = c(0,1.1*max(recr.plot/scale.recruits*scale.r)), xlim=c(0.5,nyears-1.5), col="lightcyan2")
  xr <-pretty(recr.plot/scale.recruits) 
  axis(2, at = xr*scale.r, lab = xr )
  axis(side=1, las=2, at=seq(0.5,asap$parms$nyears-1.5, by=2), 
       labels=as.character(seq(years[1],years[nyears-1], by=2)), cex=0.75, las=2)
  
  lines(seq(0.5,nyears-1.5, by=1), ssb.plot/scale.ssb, lwd=2, col = 'navyblue')
  x <- pretty(ssb.plot/scale.ssb)
  axis(4, at = c(0,x), lab = c(0,x), col='navyblue', col.axis="navyblue")
  box()
  
  
  mtext(side = 1, 'Year', line = 3)
  mtext(side = 4, paste('SSB (year)',sep=""), line = 3, col='navyblue')
  mtext(side = 2, paste('Age-1 Recruits (year-class)', sep=""), line = 3)
  
  if (save.plots==T) savePlot(paste(od, 'SARC.SSB.vs.R.barplots.', plotf, sep=''), type=plotf)
  par(family="")
  return()
}  # end function

#' Plot index input
#' 
#' Rescale indices to mean 1 and stdev 1 then plot on regular and lognormal scale.
#' @param asap name of the variable that read in the asap.rdat file
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param liz.palette color definitions
#' @export

PlotIndexInput <- function(asap,save.plots,od,plotf,liz.palette){
  
  par(mfrow=c(2,1))
  years <- seq(asap$parms$styr,asap$parms$endyr)
  nyears <- length(years)
  indvals <- matrix(NA, nrow=nyears, ncol=asap$parms$nindices)
  for (i in 1:asap$parms$nindices){
    indvals[asap$index.year.counter[[i]],i] <- asap$index.obs[[i]]
    
  }
  # rescale to mean 1 and stdev 1
  rescaled <- indvals
  my.mean <- apply(indvals,2,mean, na.rm=T)
  my.std <- apply(indvals,2,sd, na.rm=T)
  for (i in 1:asap$parms$nindices){
    rescaled[,i] <- (indvals[,i] - my.mean[i]) / my.std[i]
  }  
  my.range <- range(rescaled, na.rm=T)
  plot(years,rescaled[,1],xlab="Year",ylab="Rescaled Indices",ylim=my.range,col=liz.palette[1],type='n')
  for (i in 1:asap$parms$nindices){
    lines(years,rescaled[,i],col=liz.palette[i])
  }
  # now repeat on log scale
  log.indvals <- log(indvals)
  log.rescaled <- log.indvals
  my.log.mean <- apply(log.indvals,2,mean, na.rm=T)
  my.log.std <- apply(log.indvals,2,sd, na.rm=T)
  for (i in 1:asap$parms$nindices){
    log.rescaled[,i] <- (log.indvals[,i] - my.log.mean[i]) / my.log.std[i]
  }
  my.log.range <- range(log.rescaled, na.rm=T)
  plot(years,log.rescaled[,1],xlab="Year",ylab="Rescaled Log(Indices)",ylim=my.log.range,col=liz.palette[1],type='n')
  for (i in 1:asap$parms$nindices){
    lines(years,log.rescaled[,i],col=liz.palette[i])
  }
  if (save.plots) savePlot(paste0(od, "index.input.", plotf), type=plotf)  
  par(mfrow=c(1,1))
  return()
}

#' Plot catch by fleet
#' 
#' Barplot of total catch in weight with colors showing fleets.
#' @param asap name of the variable that read in the asap.rdat file
#' @param fleet.names names of fleets 
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param liz.palette color definitions
#' @export

PlotCatchByFleet <- function(asap,fleet.names,save.plots,od,plotf,liz.palette){
  
  nfleets <- asap$parms$nfleets
  barplot(asap$catch.obs,xlab="Year",ylab="Catch",ylim=c(0,1.1*max(apply(asap$catch.obs,2,sum))),col=liz.palette[1:nfleets],space=0)
  if (nfleets > 1) legend('top',legend=fleet.names,horiz=T,pch=15,col=liz.palette[1:nfleets])
  if (save.plots==T) savePlot(paste(od, 'catch.by.fleet.', plotf, sep=''), type=plotf)  
  
  # do proportions only if nfleets > 1
  if (nfleets > 1){
    catch.prop <- asap$catch.obs
    for (i in 1:length(catch.prop[1,])){
      catch.prop[,i] <- catch.prop[,i]/sum(catch.prop[,i])
    }
    barplot(catch.prop,xlab="Year",ylab="Proportion of Catch",ylim=c(0,1.1),col=liz.palette[1:nfleets],space=0)
    legend('top',legend=fleet.names,horiz=T,pch=15,col=liz.palette[1:nfleets])
  }
  if (save.plots==T) savePlot(paste(od, 'catch.proportions.by.fleet.', plotf, sep=''), type=plotf) 
  return()
}

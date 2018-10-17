#' Plot age composition by fleet
#' 
#' Compare observed and predicted age composition by year.
#' @param asap name of the variable that read in the asap.rdat file
#' @param fleet.names names of fleets 
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param liz.palette color definitions
#' @param is.catch.flag true means only catch plotted, false also plots discards (defaults to TRUE)
#' @export

PlotCatchAgeComp <- function(asap,fleet.names,save.plots,od,plotf,liz.palette,is.catch.flag=TRUE){
  par(mar=c(4,4,2,2), oma=c(1,1,1,1), mfcol=c(5,3)  )  
  cc=0
  for (i in 1:asap$parms$nfleets) {
    acomp.obs <- as.data.frame(asap$catch.comp.mats[4*(i-1)+1] )
    acomp.pred <- as.data.frame(asap$catch.comp.mats[4*(i-1)+2] )
    catch.yrs = which(asap$fleet.catch.Neff.init[i,]>0)
    my.title <- "Catch"
    my.save <- "Catch.Age.Comp.F"
    if (!is.catch.flag){
      acomp.obs <- as.data.frame(asap$catch.comp.mats[4*(i-1)+3] )
      acomp.pred <- as.data.frame(asap$catch.comp.mats[4*(i-1)+4] )
      catch.yrs = which(asap$fleet.discard.Neff.init[i,]>0)
      my.title <- "Discards"
      my.save <- "Discard.Age.Comp.F"
    }
    if (length(catch.yrs)>0){
      cc=cc+1
      plot(1:10,1:10,type='n',axes='F',xlab="",ylab="")
      box()
      text(5,7,paste("Fleet ",i))
      text(5,5,fleet.names[i],cex=0.85)
      arrows(5,3,5,1,length=0.1)
      title (my.title, outer=T, line=0 )
      
      for (j in 1:length(catch.yrs)) {
        cc=cc+1
        plot(seq(asap$fleet.sel.start.age[i],asap$fleet.sel.end.age[i]), acomp.obs[catch.yrs[j],] ,
             type='p', col=liz.palette[i],
             pch=1, xlab="Age", ylab="Proportion at Age", ylim=c(0, 1.1 ) )
        lines(seq(asap$fleet.sel.start.age[i],asap$fleet.sel.end.age[i]), acomp.pred[catch.yrs[j],] , 
              col=liz.palette[i],  lwd=2)
        title(paste0("Year = ", as.numeric(names(catch.yrs[j]))), outer=F )
        if (cc%%15==0)  {
          title (my.title, outer=T, line=0 )
          if (save.plots) savePlot(paste0(od, my.save, i, ".p", (cc/15),"." , plotf), type=plotf)
        } #end cc test
      }  #end loop on nyears
    } # end catch.yrs test
  }  #end loop on nfleets
  if (cc>1) {
    title (my.title, outer=T, line=0 )
    if (save.plots) savePlot(paste0(od, my.save, i, ".p", ceiling(cc/15),"." , plotf), type=plotf)
  } 
  par(mfcol=c(1,1)) 
  return()
}

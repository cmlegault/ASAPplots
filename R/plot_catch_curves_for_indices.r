#' Plot catch curves for indices
#' 
#' Creates catch curves to estimate Z from index catch data (converted from West Coast style indices).
#' @param asap name of the variable that read in the asap.rdat file
#' @param a1 list file produced by grab.aux.files function
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param first.age youngest age to use in catch curve, -999 finds peak age (defaults to -999)
#' @export

PlotCatchCurvesForIndices <- function(asap,a1,save.plots,od,plotf,first.age=-999){
  # first check to see if any West Coast style indices
  if(sum(asap$control.parms$index.age.comp.flag) == 0){
    return(list())
  }

  # create catch curve plots for each west coast style index
  usr <- par("usr"); on.exit(par(usr))
  
  catch.curve.ind <- list()
  my.col <- rep(c("blue","red","green","orange","gray50"),50)
  
  # convert the west coast style indices to catch at age matrices
  index.mats <- ConvertSurveyToAtAge(asap)
  
  # loop through all the indices
  for (ind in 1:asap$parms$nindices){
    if (asap$control.parms$index.age.comp.flag[ind] == 1){  # used age composition for the index
      title1 <- paste("Index ",ind, sep="")
      
      min.age <- asap$control.parms$index.sel.start.age[ind]
      max.age <- asap$control.parms$index.sel.end.age[ind]
      ages <- seq(min.age, max.age)
      nages.i <- max.age - min.age + 1
      cohort <- seq(asap$parms$styr-nages.i-min.age, asap$parms$endyr+nages.i-min.age)
      
      # replace zeros with NA and take logs
      iob <- rep0log(index.mats$ob[[ind]])
      ipr <- rep0log(index.mats$pr[[ind]])
      
      # make cohorts
      iob.coh <- makecohorts(iob)
      ipr.coh <- makecohorts(ipr)
      
      # drop plus group
      if (asap$control.parms$index.sel.end.age[ind] == asap$parms$nages){
        iob.coh[,length(iob.coh[1,])] <- NA
        ipr.coh[,length(iob.coh[1,])] <- NA
      }
      
      first.age.label <- 1
      if (first.age==1) title1 <- paste(title1," First Age = 1", sep="") 
      
      # determine which ages to use for each cohort (default)
      if (first.age == -999){
        iob.coh <- find_peak_age(iob.coh)
        ipr.coh <- find_peak_age(ipr.coh)
        first.age.label <- "find_peak"
        title1 <- paste(title1," (Peak Age)", sep="")       
        
      }
      
      # or drop youngest ages based on user control
      if (first.age > min.age) {
        iob.coh[,1:(first.age-min.age)] <- NA
        ipr.coh[,1:(first.age-min.age)] <- NA
        title1 <- paste(title1," First Age = ",first.age, sep="")
        first.age.label <- first.age        
      }
      
      # compute Z by cohort
      z.ob <- calc_Z_cohort(iob.coh)
      z.pr <- calc_Z_cohort(ipr.coh)
      
      # make the plots
      par(mfrow=c(2,1))
      plot(cohort,cohort,type='n',ylim=range(c(iob.coh,ipr.coh),na.rm=T),xlab="",ylab="Log(Index)",main=paste(title1," Observed", sep=""))
      for (i in 1:length(iob.coh[,1])){
        lines(seq(cohort[i],cohort[i]+nages.i-1),iob.coh[i,],type='p',lty=1,pch=seq(1,nages.i),col="gray50")
        lines(seq(cohort[i],cohort[i]+nages.i-1),iob.coh[i,],type='l',lty=1,col=my.col[i])
      }
      
      Hmisc::errbar(cohort,z.ob[,1],z.ob[,3],z.ob[,2],xlab="Year Class",ylab="Z",ylim=range(c(z.ob,z.pr),na.rm=T))
      
      if (save.plots) savePlot(paste(od,"catch_curve_",title1,"_Observed_first_age_",first.age.label,".",plotf, sep=""), type=plotf)
      
      plot(cohort,cohort,type='n',ylim=range(c(iob.coh,ipr.coh),na.rm=T),xlab="",ylab="Log(Index)",main=paste(title1," Predicted", sep=""))
      for (i in 1:length(iob.coh[,1])){
        lines(seq(cohort[i],cohort[i]+nages.i-1),ipr.coh[i,],type='p',lty=1,pch=seq(1,nages.i),col="gray50")
        lines(seq(cohort[i],cohort[i]+nages.i-1),ipr.coh[i,],type='l',lty=1,col=my.col[i])
      }
      
      Hmisc::errbar(cohort,z.pr[,1],z.pr[,3],z.pr[,2],xlab="Year Class",ylab="Z",ylim=range(c(z.ob,z.pr),na.rm=T))
      
      if (save.plots) savePlot(paste(od,"catch_curve_",title1,"_Predicted_first_age_",first.age.label,".",plotf, sep=""), type=plotf)
      
      # write out .csv files for Z, one file for each fleet
      asap.name <- a1$asap.name
      
      colnames(z.ob) <-c("Z.obs","low.80%", "high.80%")
      write.csv(z.ob, file=paste(od,"Z.Ob.Index.",ind,"_",asap.name,".csv", sep=""),
                row.names=cohort)
      
      colnames(z.pr) <-c("Z.pred","low.80%", "high.80%")
      write.csv(z.pr, file=paste(od,"Z.Pr.Index.",ind,"_",asap.name,".csv", sep=""),
                row.names=cohort)
      
    }
    
  }   # end loop over nindices
  return(catch.curve.ind)
}

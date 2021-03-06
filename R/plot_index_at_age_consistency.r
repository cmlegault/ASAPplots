#' Plot index at age consistency
#' 
#' Lattice style plot of index catch at age X vs age Y with each point a cohort for each fleet. Computed for both input and predicted index catch at age.
#' @param asap name of the variable that read in the asap.rdat file
#' @param index.names names of indices 
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @return list with correlation matrices for input and predicted catch at age matrices for each fleet
#' @export

PlotIndexAtAgeConsistency <- function(asap,index.names,save.plots,od,plotf){
  # first check to see if any West Coast style indices
  if(sum(asap$control.parms$index.age.comp.flag) == 0){
    return(list())
  }
  
  # now loop through indices, check to make sure actually estimating proportions at age
  # also need to use only the age range selected in the proportions

  index.corr <- list()
  
  # convert the west coast style indices to catch at age matrices
  index.mats <- ConvertSurveyToAtAge(asap)  
  
  # loop through all the indices
  for (ind in 1:asap$parms$nindices){
    if (asap$control.parms$index.age.comp.flag[ind] == 1){  # used age composition for the index
      title1 <- paste0("Index ",ind," (",index.names[ind],")")
      
      # replace zeros with NA and take logs
      iob <- rep0log(index.mats$ob[[ind]])
      ipr <- rep0log(index.mats$pr[[ind]])
      
      # make cohorts
      iob.coh <- makecohorts(iob)
      ipr.coh <- makecohorts(ipr)
      
      # get age range for index
      age.min <- asap$control.parms$index.sel.start.age[ind]
      age.max <- asap$control.parms$index.sel.end.age[ind]
      
      # make the plots
      iob.cor <- PlotCoh(iob.coh,save.plots,od,plotf,mytitle=paste0(title1," Observed"),mylabels=paste0("age-",seq(age.min,age.max)))
      ipr.cor <- PlotCoh(ipr.coh,save.plots,od,plotf,mytitle=paste0(title1," Predicted"),mylabels=paste0("age-",seq(age.min,age.max)))
      index.corr[[ind]] <- list(iob.cor,ipr.cor)
    }
  }
  return(index.corr)
}

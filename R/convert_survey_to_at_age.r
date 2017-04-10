#' Converts surveys to age matrices
#' 
#' Converts West Coast style surveys (total abundance and age comps) into catch at age matrices for age consistency plots.
#' @param asap name of the variable that read in the asap.rdat file
#' @return list with catch at age matrices by index
#' @export

ConvertSurveyToAtAge <- function(asap){
  # takes West Coast style surveys and converts them to catch at age matrices
  index.mats <- list()
  weight.mat.counter <- 0
  years <- asap$parms$styr:asap$parms$endyr
  for (ind in 1:asap$parms$nindices){
    if (asap$control.parms$index.age.comp.flag[ind] == 1){  # used age composition for the index
      # get the aggregate index observed and predicted time series
      agg.ob <- asap$index.obs[[ind]]
      agg.pr <- asap$index.pred[[ind]]
      
      # get proportions for correct years and ages only
      age.min <- asap$control.parms$index.sel.start.age[ind]
      age.max <- asap$control.parms$index.sel.end.age[ind]
      props.ob <- asap$index.comp.mats[[(ind*2-1)]][asap$index.year.counter[[ind]],age.min:age.max]
      props.pr <- asap$index.comp.mats[[(ind*2)]][asap$index.year.counter[[ind]],age.min:age.max]
      
      # figure out units for aggregate and proportions
      agg.units <- asap$control.parms$index.units.aggregate[ind]
      prp.units <- asap$control.parms$index.units.proportions[ind]
      
      # get weight (matrix if necessary)
      if (agg.units==1 || prp.units==1){  # either in weight
        weight.mat.counter <- weight.mat.counter+1
        use.me <- weight.mat.counter + (asap$parms$nfleets * 2) + 4
        waa <- asap$WAA.mats[[use.me]][asap$index.year.counter[[ind]],age.min:age.max] 
      }
      
      # create index.obs and pred based on which of the four possible combinations of units is used for this index
      if (agg.units==1 && prp.units==1){  # both in weight
        index.ob <- agg.ob * props.ob / waa
        index.pr <- agg.pr * props.pr / waa
      }
      if (agg.units==1 && prp.units==2){  # agg in weight, props in numbers
        index.ob <- wtprop2caa(agg.ob,waa,props.ob)  # use catch function
        index.pr <- wtprop2caa(agg.pr,waa,props.pr)
      }
      if (agg.units==2 && prp.units==1){  # agg in numbers, props in weight
        # need to search for correct agg total in weight to result in observed agg total in number
        # for now just use simple approximation that agg.wt = sum(waa*prop) *ctot and then solve using both in weight approach
        agg.wt.ob <- apply((waa * props.ob),1,sum) * agg.ob
        agg.wt.pr <- apply((waa * props.pr),1,sum) * agg.pr
        index.ob <- agg.wt.ob * props.ob / waa
        index.pr <- agg.wt.pr * props.pr / waa
      }
      if (agg.units==2 && prp.units==2){  # both in numbers
        index.ob <- agg.ob * props.ob
        index.pr <- agg.pr * props.pr
      }
      
      # put matrices into full year matrix (ages only for selected ages though)
      # need to do this to account for missing years of data interspersed in time series
      index.ob.full <- matrix(NA, nrow=asap$parms$nyears, ncol=(age.max-age.min+1))
      index.pr.full <- matrix(NA, nrow=asap$parms$nyears, ncol=(age.max-age.min+1))
      for (i in 1:asap$index.nobs[ind]){
        index.ob.full[asap$index.year.counter[[ind]][i],] <- as.vector(index.ob[i,])
        index.pr.full[asap$index.year.counter[[ind]][i],] <- as.vector(index.pr[i,])
      }
      
      # save the results for this index
      index.mats$ob[[ind]] <- index.ob.full
      index.mats$pr[[ind]] <- index.pr.full 
      # NEW 9/23/2016 added year and age labels to matrices for use with Sinclair Z functions
      rownames(index.mats$ob[[ind]]) <- years
      rownames(index.mats$pr[[ind]]) <- years
      colnames(index.mats$ob[[ind]]) <- age.min:age.max
      colnames(index.mats$pr[[ind]]) <- age.min:age.max
    }
    if (asap$control.parms$index.age.comp.flag[ind] != 1){  # cannot use this index
      index.mats$ob[[ind]] <- NA
      index.mats$pr[[ind]] <- NA
    }
  }
  return(index.mats)  
}

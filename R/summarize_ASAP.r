#' Summarize ASAP
#' 
#' Point estimates and Hessian-based 95 percent CI for SSB, R, and Freport.
#' @param asap name of the variable that read in the asap.rdat file
#' @param a1 list file produced by grab.aux.files function
#' @param od output directory for plots and csv files 
#' @export

SummarizeASAP <- function(asap,a1,od){
  
  if(any(!is.na(a1$asap.std))){
    asap.name <- a1$asap.name
    years <- seq(asap$parms$styr, asap$parms$endyr)
    ssb.std <- a1$asap.std[which(a1$asap.std$name=="SSB") ,4]
    recr.std <- a1$asap.std[which(a1$asap.std$name=="recruits") ,4]
    Frep.std <- a1$asap.std[which(a1$asap.std$name=="Freport") ,4]
    ssb_up <-  asap$SSB + 1.96*ssb.std
    ssb_lo <-  asap$SSB - 1.96*ssb.std
    recr_up <-  asap$N.age[,1] + 1.96*recr.std
    recr_lo <-  asap$N.age[,1] - 1.96*recr.std
    Frep_up <-  asap$F.report + 1.96*Frep.std
    Frep_lo <-  asap$F.report - 1.96*Frep.std
    mwg.out <- cbind("Year"=years, "SSB"=asap$SSB, "SSB_95_lo"=ssb_lo, "SSB_95_hi"=ssb_up,
                     "Recr"=asap$N.age[,1], "Recr_95_lo"=recr_lo, "Recr_95_hi"=recr_up,
                     "Freport"=asap$F.report, "Freport_95_lo"=Frep_lo, "Freport_95_hi"=Frep_up  )
    
    write.csv(mwg.out, paste0(od,"ASAP_summary_",asap.name,".csv"), row.names=F)
  }
  return()
}

#' Plot front page 
#' 
#' Summary information about ASAP run and version of ASAPplots used.
#' @param wd directory where ASAP run is located
#' @param asap.name Base name of original dat file (without the .dat extension)
#' @param asap name of the variable that read in the asap.rdat file
#' @param a1 list file produced by grab.aux.files function
#' @param save.plots saves indivdual plots (defaults to TRUE)
#' @param od output directory for plots and csv files 
#' @param plotf format for individual plots (defaults to 'png')
#' @export

PlotFrontPage <- function(wd,asap.name,asap,a1,save.plots,od,plotf){
  
  plot(1:10,1:10,type='n',axes=F,xlab="",ylab="")
  
  text(5,9,paste0("File = ",asap.name,".dat"))
  
  text(5,7,paste0("ASAP3 run on ",asap$info$date))
  
  text(5,5,paste0("dir = ",wd))
  
  text(5,3,paste0("ASAPplots version = ",packageVersion("ASAPplots")))
  
  if (a1$npar == -999) {
    text(5,1,"Warning, run did not converge, .std file not present", col="red")
  } else {
    text(5,1,paste0("npar = ",a1$npar,", maximum gradient = ",a1$max.grad))
    if (as.numeric(a1$max.grad) > 0.001){
      text(5,2,"Warning, maximum gradient > 0.001", col="red")
    } 
  }
  
  if (save.plots) savePlot(paste0(od, "Front.Page.",plotf), type=plotf)
  
  return()
}

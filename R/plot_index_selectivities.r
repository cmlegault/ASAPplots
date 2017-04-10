#' Plot index selectivities 
#' 
#' Line plots of selectivities only for indices with multiple ages selected.
#' @param asap name of the variable that read in the asap.rdat file
#' @param index.names names of indices 
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param liz.palette color definitions
#' @export

# Index selectivities 
PlotIndexSelectivities <- function(asap,index.names,save.plots,od,plotf,liz.palette){
  par(mfrow=c(1,1) )
  cc=0
  for (i in 1:asap$parms$nindices) {
    a1<-  asap$control.parms$index.sel.start.age[i]
    a2<-  asap$control.parms$index.sel.end.age[i]
    if (a2>a1){
      cc=cc+1
      
      ind.sel <- rep(NA,asap$parms$nages)
      ind.sel[a1:a2] <- asap$index.sel[i,a1:a2]
      if (cc==1){
        plot(1:asap$parms$nages, ind.sel, type='l', col=liz.palette[cc], ylim=c(0,1.1),
             xlab="Age", ylab="Selectivity at age" , xlim=c(1, (asap$parms$nages+3) ), lwd=2 )
        #leg.txt <- paste("Index_",i,sep="")
        leg.txt <- index.names
      }    
      if (cc>1){
        lines(1:asap$parms$nages, ind.sel, col=liz.palette[cc], lwd=2  )
        #leg.txt <- c(leg.txt,paste("Index_",i,sep=""))
      }    
    }  #end test for a2>a1
  } #end i loop
  
  ##New
  age.diff <- asap$control.parms$index.sel.end.age - asap$control.parms$index.sel.start.age
  plot.index <- which(age.diff != 0)
  #  if (cc>1) legend("topright", col=liz.palette[1:cc], legend=leg.txt, lwd=2)
  ## New (to fix cases where a1=a2)
  if (cc>1) legend("topright", col=liz.palette[1:cc], legend=index.names[plot.index], lwd=2)
  title("Indices")
  if (save.plots) savePlot(paste(od, "Index.Sel.",plotf, sep=""), type=plotf) 
  return()
}
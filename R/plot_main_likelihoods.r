#' Plot main likelihoods by component
#' 
#' Not a very useful plot, need to replace this with something more informative and less misleading.
#' @param asap.name Base name of original dat file (without the .dat extension)
#' @param asap name of the variable that read in the asap.rdat file
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param liz.palette color definitions
#' @param max.grad maximum gradient at solution
#' @param npar number of parameters
#' @export

PlotMainLikelihoods <- function(asap.name,asap,save.plots,od,plotf,liz.palette,max.grad,npar){
  par(mfrow=c(1,1) )

  like2 <- unlist(asap$like[2:length(asap$like)])
  like2.names <- names(asap$like[2:length(asap$like)])
  n.like <- length(like2)
  my.range <- c(min(like2), 1.2*max(like2) )
  par(mar=c(5,10,1,1), oma=c(1,0,0,0)) 
  barplot(horiz=T, like2, beside=F, col=liz.palette[1:n.like],
          xlab="Likelihood Contribution",  axisnames=F,  axes=F,  space=0,
          xlim=my.range  )
  axis(side=1, at=pretty(seq(my.range[1],my.range[2]), n=10), labels=pretty(seq(my.range[1],my.range[2]), n=10), cex=.75 )
  axis(side=2, at=seq(0.5,(n.like-0.5)), labels= like2.names, las=2)
  text(x= like2, y=seq(0.5,(n.like-0.5)), labels=round(like2,0), cex=0.8, pos=4)
  text(x= my.range[1], y=(n.like+0.25),  cex=0.95, pos=4, col='#110033',
       labels=paste('Maximum gradient = ',round(as.numeric(max.grad),6),sep="") )  
  box()
  title(paste("Components of Obj. Function (", round(as.numeric(asap$like[1]),0), "), npar=", npar, sep=""), cex=0.9 )
  title( sub=paste("Model: ", asap.name, "     ", asap$info$date,sep=""))
  
  if (save.plots) savePlot(paste(od, "Likelihood.Comp.",plotf, sep=""), type=plotf)
  
  ###NEW  
  # write out .csv with likelihood components
  tmp.n.catch <- 0
  tmp.n.discard <- 0
  tmp.n.index <- 0
  tmp.fleet.catch <- numeric(0)
  tmp.fleet.discard <- numeric(0)
  tmp.index <- numeric(0)
  if (asap$like.additional$nfleets>1) {
    tmp.fleet.catch <- asap$like.additional[which(substr(names(asap$like.additional),1,8)=="lk.catch")]
    tmp.fleet.discard <- asap$like.additional[which(substr(names(asap$like.additional),1,10)=="lk.discard")]
    tmp.n.catch <- length(tmp.fleet.catch)+length(tmp.fleet.discard)
  } 
  if (asap$like.additional$nindices>1) {
    tmp.index <- asap$like.additional[which(substr(names(asap$like.additional),1,12)=="lk.index.fit")]
    tmp.n.index <- length(tmp.index)
  } 
  like.table <- as.data.frame(matrix(NA, nrow=(1+length(asap$like)+tmp.n.catch+tmp.n.index), ncol=2) )
  line.catch <- which(names(asap$like)=="lk.catch.total")
  line.discard <- which(names(asap$like)=="lk.discard.total")
  line.index <- which(names(asap$like)=="lk.index.fit.total")
  like.table[,1] <- c("N_parameters", substr(names(asap$like[1:line.catch]),4,20), 
                      substr(names(tmp.fleet.catch), 4,20) , substr(names(asap$like[(line.catch+1):line.discard]),4, 20),
                      substr(names(tmp.fleet.discard), 4,20), substr(names(asap$like[(line.discard+1):line.index]),4,20),
                      substr(names(tmp.index),4,20), substr(names(asap$like[(line.index+1):length(asap$like)]),4,20 )  )
  #like.table[1,2] <-  npar
  #like.table[2:(line.catch+1),2] <- as.numeric(asap$like[1:line.catch])
  
  like.table[,2] <- as.numeric(c(npar, asap$like[1:line.catch], tmp.fleet.discard,
                                 asap$like[(line.catch+1):line.discard], tmp.fleet.discard,
                                 asap$like[(line.discard+1):line.index], tmp.index,
                                 asap$like[(line.index+1):length(asap$like)]  )   )
  
  colnames(like.table) <- c(asap.name, "Value")
  write.csv(like.table, file=paste(od, "Likelihood.Table.", asap.name, ".csv", sep=""), 
            row.names=F)
  
  return() 
}

#' Plot high correlations among parameters
#' 
#' Identify parameters with high correlation (as defined by user).
#' @param a1 list file produced by grab.aux.files function
#' @param correlation.limit will return parameter combos where |correlation| > value
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @export

PlotHighCorr <- function(a1,correlation.limit,save.plots,od,plotf){

  par(mfrow=c(1,1) )
  npar <- a1$npar
  if (npar != -999){
    count.high.corr <- rep(0, npar)
    asap.cor.mat <- a1$asap.cor.mat[1:npar, 1:npar]
    text.col <- ifelse(abs(asap.cor.mat)>correlation.limit, "red", "white")
    tcor<-0
    for (i in 1:npar) {
      for (j in 1:npar) {
        if (text.col[i,j]=="red" && is.na(text.col[i,j])==F) tcor<- tcor+1
        if (text.col[i,j]=="red" && is.na(text.col[i,j])==F) count.high.corr[i]<- count.high.corr[i]+1
      } # end j-loop
    } # end i-loop
    
    plot(seq(1,npar), count.high.corr[1:npar], type='l', xlab="Parameter", lwd=1, col='black',
         ylab="Number of high correlations", ylim=c(0,1.1*max(count.high.corr, 1) )  )
    
    high.rows <- matrix(NA, 1, 2)
    cor.vals <- 0
    for (irow in 1:npar) {
      temp.row <- as.vector(which( abs(asap.cor.mat[irow,])>correlation.limit))
      
      if(length(temp.row)>0) {
        cor.pairs <- cbind(rep(irow, length(temp.row)), temp.row)
        high.rows<- rbind(high.rows, cor.pairs)
        cor.vals <-c(cor.vals, asap.cor.mat[irow, temp.row] )
      }
    } #end loop over irow
    
    high.rows<- high.rows[-1,]
    cor.vals <- cor.vals[-1]
    
    if (length(cor.vals) >0 ) {
      named.cor.pairs <- as.data.frame(matrix(NA, length(cor.vals),3))
      colnames(named.cor.pairs)=c("Par1", "Par2", "Correlation")
      if (length(cor.vals)==1) {
        named.cor.pairs[,1] <-a1$asap.cor.names[high.rows[1]]
        named.cor.pairs[,2] <-a1$asap.cor.names[high.rows[2]]
      }
      if (length(cor.vals)>1) {
        named.cor.pairs[,1] <-a1$asap.cor.names[high.rows[,1]]
        named.cor.pairs[,2] <-a1$asap.cor.names[high.rows[,2]]
      }
      named.cor.pairs[,3] <-  cor.vals
      
      write.csv(named.cor.pairs, file=paste(od, "High.Corr.Pars.csv",sep=""),   row.names=F)
    }  # end check for length(cor.vals)
    
    if (save.plots) savePlot(paste(od, "Corr.Limit.Count.by.par.",plotf, sep=""), type=plotf)
    
  } # end if npar check
  
  return()
}


#' Lattice style plot of cohorts by age
#' 
#' Each point shows age x vs age y of a cohort. Correlation values also shown. Assumes matrix has ages as columns and that cohorts are in rows.
#' @param matcoh matrix of values with ages as columns and cohorts as rows
#' @param save.plots save individual plots
#' @param od output directory for plots and csv files 
#' @param plotf type of plot to save
#' @param mytitle title for plot (defaults to blank)
#' @param mylabels optional labels to write in boxes on diagonal (defaults to showing "age-X")
#' @return correlation matrix
#' @export

PlotCoh <- function(matcoh,save.plots,od,plotf,mytitle="",mylabels=NA){
  nc <- length(matcoh[1,])
  my.cor <- cor(matcoh,use="pairwise.complete.obs")
  my.cor.round <- round(my.cor,2)
  usr <- par("usr"); on.exit(par(usr))
  par(mfcol=c(nc,nc))
  par(oma=c(0,0,3,1),mar=c(1,1,0,0))
  for (i in 1:nc){
    for (j in nc:1){
      if (i == j){
        plot(1:10,1:10,type='n',axes=F)
        if (is.na(mylabels[1])) text(5,5,paste0("age-",i),cex=1.4)
        if (is.na(mylabels[1])==F) text(5,5,mylabels[i],cex=1.4)
      }
      if (i < j){
        if (!is.na(my.cor[i,j])){
          plot(matcoh[,i],matcoh[,j],axes=F) # make sure have some data to plot
          xx <- matcoh[,i]
          yy <- matcoh[,j]
          my.fit <- lm(yy~xx)
          if (!is.na(my.fit$coefficients[2])) abline(my.fit,col="red")
          xrng <- data.frame(xx = seq(min(xx,na.rm=T),max(xx,na.rm=T),length.out=100))
          ##New  (fix for NaN)
          ss<- summary(my.fit)
          if (is.finite(ss$sigma) ) {
            zz <- predict(my.fit,xrng,interval="confidence")
            lines(xrng[,1],zz[,2],col="blue")
            lines(xrng[,1],zz[,3],col="blue")
          }  #end fix for NaN
          
          box()
        }
        if (is.na(my.cor[i,j])){  # if not data, just make empty box
          plot(1:10,1:10,type='n',axes=F)
          box()
        }
      }
      if (i > j){
        plot(1:10,1:10,type='n',axes=F)
        txt <- format(my.cor.round[i,j], nsmall=2)
        text(5,5,txt)
        box()
      }
    }
  }
  title(mytitle, outer=T)
  if (save.plots) savePlot(paste0(od,"cohort_age_matrix_",mytitle,".",plotf), type=plotf)
  return(my.cor)
}

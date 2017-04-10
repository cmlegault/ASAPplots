#' Calculates mean and standard error of frequency data
#' 
#' Helper function for Francis ESS calculations.
#' @param bins the values in the class marks (ages in this case)
#' @param N the total number (ESS in this case)
#' @param frdist the frequency distribution (should sum to one over the bins)
#' @return vector of mean and standard error
#' @export
 
CalcFreqMeanSE <- function(bins,N,frdist){
  Y <- bins
  f <- N*frdist
  n <- sum(f)
  sumfY <- sum(f*Y)
  Ybar <- sumfY/n
  sumfY2 <- sum(f*Y*Y)
  CT <- sumfY^2/n
  sumfy2 <- sumfY2-CT
  s2 <- sumfy2/(n-1)
  sigma <- sqrt(s2)
  stderror <- sigma/sqrt(n)  # note this is standard deviation of the mean
  meanstderror <- c(Ybar,stderror)
  return(meanstderror)
}

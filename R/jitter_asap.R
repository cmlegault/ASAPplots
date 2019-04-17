#' JitterASAP
#' Changes .pin file to use random restarts from either full range of parameters or SS3.30 approach of values near estimated ones.
#' @param in.pin pin object of original parameter estimates
#' @param.list list of parameters as either "fixed" or "estimated", only "estimated" are jittered
#' @param jitterfac factor used in SS3.30 approach (default=0.1)
#' @export

JitterASAP <- function(in.pin, param.list, jitterfac=0.1){
  
  out.pin <- in.pin
  
  zmin <- qnorm(0.001)
  zmax <- qnorm(0.999)
  
  for (i in 1:length(param.list$type)){
    
    if (param.list$type[i] == "full"){
      nrows <- length(out.pin$dat[[i]])
      for (irow in 1:nrows){
        nvals <- length(out.pin$dat[[i]][[irow]])
        for (ival in 1:nvals){
          Pmin <- param.list$lowerbound[[i]][[irow]][ival]
          Pmax <- param.list$upperbound[[i]][[irow]][ival]
          newval <- runif(1, min = Pmin, max = Pmax)
          out.pin$dat[[i]][[irow]][ival] <- newval
        }
      }
    }
    
    if (param.list$type[i] == "jitter"){
      nrows <- length(out.pin$dat[[i]])
      for (irow in 1:nrows){
        nvals <- length(out.pin$dat[[i]][[irow]])
        for (ival in 1:nvals){
          Pval <- out.pin$dat[[i]][[irow]][ival]
          Pmin <- param.list$lowerbound[[i]][[irow]][ival]
          Pmax <- param.list$upperbound[[i]][[irow]][ival]
          Pmean <- (Pmin + Pmax) / 2
          Psigma <- (Pmax - Pmean) / zmax
          zval <- (Pval - Pmean) / Psigma
          kval <- pnorm(zval)
          temp <- runif(1)
          kjitter <- kval + (jitterfac * ((2 * temp) - 1))
          if (kjitter < 0.0001){
            newval <- Pmin + 0.1 * (Pval - Pmin)
          } else if (kjitter > 0.9999){
            newval <- Pmax - 0.1 * (Pmax - Pval)
          } else {
            zjitter <- qnorm(kjitter)
            newval <- Pmean + (Psigma * zjitter)
          }
          # check for jittered value outside bounds
          if (newval < Pmin) newval <- Pmin
          if (newval > Pmax) newval <- Pmax
          out.pin$dat[[i]][[irow]][ival] <- newval
        }
      }
    }
    
  } # end of param.list loop
  
  return(out.pin)
}

# simple demo of how jitter works
# uncomment and run the following for different values of Pval, Pmin, and Pmax
# Pval <- 0.3  # estimated parameter value
# Pmin <- 0    # lower bound for parameter
# Pmax <- 1    # upper bound for parameter
# 
# jitterfac <- 0.1
# zmin <- qnorm(0.001)
# zmax <- qnorm(0.999)
# Pmean <- (Pmin + Pmax) / 2
# Psigma <- (Pmax - Pmean) / zmax
# zval <- (Pval - Pmean) / Psigma
# 
# nr <- 101
# randval <- seq(0, 1, length.out = nr)
# res <- rep(NA, nr)
# for (i in 1:nr){
#   kval <- pnorm(zval)
#   temp <- randval[i] # runif(1)
#   kjitter <- kval + (jitterfac * ((2 * temp) - 1))
#   if (kjitter < 0.0001){
#     newval <- Pmin + 0.1 * (Pval - Pmin)
#   } else if (kjitter > 0.9999){
#     newval <- Pmax - 0.1 * (Pmax - Pval)
#   } else {
#     zjitter <- qnorm(kjitter)
#     newval <- Pmean + (Psigma * zjitter)
#   }
#   # check for jittered value outside bounds
#   if (newval < Pmin) newval <- Pmin
#   if (newval > Pmax) newval <- Pmax
#   res[i] <- newval
# }
# plot(randval, res, ylim = c(Pmin, Pmax))
# abline(h=Pval, col="red")
# abline(v=0.5, col="blue")
# 

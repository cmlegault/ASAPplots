#' ReadASAP3PinFile
#' 
#' Read ASAP 3 pin (=par) file into R.
#' @param pinf full path and name of pin file (including .pin or .par suffix) 
#' @export

ReadASAP3PinFile <- function(pinf){
  
  char.lines <- readLines(pinf)
  com.ind <- which(substring(char.lines,1,1) == "#")
  dat.start <- com.ind[c(which(diff(com.ind)>1), length(com.ind))]
  comments <- char.lines[dat.start]
  
  # drop leading space in data lines
  for (i in 1:length(char.lines)){
    if (substring(char.lines[i], 1, 1) == " "){
      char.lines[i] <- substring(char.lines[i], 2, nchar(char.lines[i]))
    }
  }
  
  dat <- list()
  np <- length(comments)
  ind <- 0
  
  # fleet selectivity parameters
  nselparams <- which(comments == "# log_Fmult_year1:") - 1
  for (i in 1:nselparams){
    ind <- ind + 1
    dat[[ind]] <- list()
    dat[[ind]][[1]] <- scan(pinf, what = double(), skip = dat.start[ind], n = 1)
  }
  
  # log Fmult year 1 (one value for each fleet)
  ind <- ind + 1
  dat[[ind]] <- list()
  nfleets <- length(strsplit(char.lines[dat.start[ind] + 1], " ")[[1]])
  dat[[ind]][[1]] <- scan(pinf, what = double(), skip = dat.start[ind], n = nfleets)
  
  # log Fmult devs (one row for each fleet, each row nyears-1 long)
  ind <- ind + 1
  dat[[ind]] <- list()
  ndevs <- length(strsplit(char.lines[dat.start[ind] + 1], " ")[[1]])
  for (ifleet in 1:nfleets){
    dat[[ind]][[ifleet]] <- scan(pinf, what = double(), skip = dat.start[ind] + ifleet - 1, n = ndevs)
  }
  
  # log recruit devs (one value for each year)
  ind <- ind + 1
  dat[[ind]] <- list()
  nyears <- length(strsplit(char.lines[dat.start[ind] + 1], " ")[[1]])
  dat[[ind]][[1]] <- scan(pinf, what = double(), skip = dat.start[ind], n = nyears)
  
  # log N year 1 devs (one value for each age except age 1 (# values = nages - 1))
  ind <- ind + 1
  dat[[ind]] <- list()
  nagesm1 <- length(strsplit(char.lines[dat.start[ind] + 1], " ")[[1]]) 
  dat[[ind]][[1]] <- scan(pinf, what = double(), skip = dat.start[ind], n = nagesm1)
  
  # log q year 1 (one value for each index)
  ind <- ind + 1
  dat[[ind]] <- list()
  ninds <- length(strsplit(char.lines[dat.start[ind] + 1], " ")[[1]])
  dat[[ind]][[1]] <- scan(pinf, what = double(), skip = dat.start[ind], n = ninds)
  
  # log q devs (one row for each index, each row depends on number of obs in that index, so varies)
  ind <- ind + 1
  dat[[ind]] <- list()
  for (ii in 1:ninds){
    nobs <- length(strsplit(char.lines[dat.start[ind] + ii], " ")[[1]])
    dat[[ind]][[ii]] <- scan(pinf, what = double(), skip = dat.start[ind] + ii - 1, n = nobs)
  }
  
  # index selectivity parameters
  nindexselparams <- which(comments == "# log_SR_scaler:") - ind - 1
  for (i in 1:nindexselparams){
    ind <- ind + 1
    dat[[ind]] <- list()
    dat[[ind]][[1]] <- scan(pinf, what = double(), skip = dat.start[ind], n = 1)
  }
  
  # log stock recruit scaler
  ind <- ind + 1
  dat[[ind]] <- list()
  dat[[ind]][[1]] <- scan(pinf, what = double(), skip = dat.start[ind], n = 1)
  
  # stock recruit steepness
  ind <- ind + 1
  dat[[ind]] <- list()
  dat[[ind]][[1]] <- scan(pinf, what = double(), skip = dat.start[ind], n = 1)
  
  return(list(dat=dat, comments=comments))  
}

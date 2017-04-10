# helper functions used by catch curves and age consistency plots

#-------------------------------------------------------------------------------
# function to compute catch at age (matrix) 
# from proportions at age (matrix), weight at age (matrix) and total weight (vector)
wtprop2caa <- function(totwt,waa,props){
  caa <- props
  for (i in 1:length(totwt)){
    if (sum(props[i,]) == 0) caa[i,] = 0
    if (sum(props[i,]) > 0) caa[i,] <- props[i,] * (totwt[i] / sum(props[i,] * waa[i,]))
  }
  return(caa)
}

#-------------------------------------------------------------------------------
# function replace zeros and take log
rep0log <- function(mat){
  mat1 <- mat
  for (i in 1:length(mat1[,1])){
    mat1[i,mat1[i,]==0] <- NA
    mat1[i,] <- log(mat1[i,])
  }
  return(mat1)
}

#-------------------------------------------------------------------------------
# function make cohorts
makecohorts <- function(mat){
  NAmat <- matrix(NA, nrow=length(mat[1,]), ncol=length(mat[1,]))
  matcoh1 <- rbind(NAmat,mat,NAmat)
  nr <- length(matcoh1[,1])
  nc <- length(mat[1,])
  matcoh <- matrix(NA, nrow=nr, ncol=nc)
  for (i in 1:nc){
    matcoh[1:(nr-i),i] <- matcoh1[i:(nr-1),i]
  }
  return(matcoh)
}

#-------------------------------------------------------------------------------
find_peak_age <- function(cohmat){
  # determines peak age within each cohort and replaces younger ages with NA
  ages <- seq(1,length(cohmat[1,]))
  #  temp.sum <- apply(cohmat,1,function(x) sum(x, na.rm=T)) # this is the offending line, replaced with the one below
  temp.sum <- apply(cohmat,1,function(x) sum(exp(x), na.rm=T))  # note have to use exp because took logs in earlier function rep0log
  for (i in 1:length(cohmat[,1])){
    if (temp.sum[i] > 0){  # necessary to avoid cohorts that are all NA
      age.peak <- max(ages[cohmat[i,]==max(cohmat[i,],na.rm=T)], na.rm=T)  # find the peak age
      if (age.peak > 1) cohmat[i,1:(age.peak-1)] <- NA  # replace ages < peak.age with NA
    }
  }
  return(cohmat)
}

#-------------------------------------------------------------------------------
calc_Z_cohort <- function(cohmat){
  # calculate Z along cohort using linear regression and return point estimate and 80% confidence interval
  ages <- seq(1,length(cohmat[1,])) 
  z <- matrix(NA, nrow=length(cohmat[,1]), ncol=3)
  for (i in 1:length(cohmat[,1])){
    if (length(cohmat[i,!is.na(cohmat[i,])]) >= 2){  # make sure there are at least 2 data points for point estimate
      z.fit <- lm(cohmat[i,]~ages)  # linear regression of cohort abundance vs age
      z[i,1] <- -1 * z.fit$coefficients[2]  # note change in sign for Z
      if (length(cohmat[i,!is.na(cohmat[i,])]) >= 3){  # need at least 3 data points for CI
        z[i,2:3] <- -1 * rev(confint(z.fit, "ages", level=0.80))  # note change in sign and order for Z CI
      }  
    }
  }
  return(z)
}

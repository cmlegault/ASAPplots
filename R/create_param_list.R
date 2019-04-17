#' CreateParamList
#' 
#' Provides the upper and lower limits for each parameter based on ASAP tpl bounded parameter values.
#' @param asap.pin.obj contains data and comments from an ASAP3 pin file
#' @param asap.dat.obj contains data and comments from an ASAP3 dat file
#' @export

CreateParamList <- function(asap.pin.obj, asap.dat.obj){
  
  dat <- asap.pin.obj$dat
  comments <- asap.pin.obj$comments
  np <- length(comments)
  
  param.list <- list()
  param.list$type <- rep(NA, np) # placeholder, will be filled in later
  param.list$lowerbound <- list()
  param.list$upperbound <- list()

  p <- which(substring(comments, 1, 12) == "# sel_params")
  for (ip in p){
    param.list$lowerbound[[ip]] <- list()
    param.list$upperbound[[ip]] <- list()
    param.list$lowerbound[[ip]][[1]] <- 0
    param.list$upperbound[[ip]][[1]] <- 1
  }
  
  p <- which(substring(comments, 1, 18) == "# index_sel_params")
  for (ip in p){
    param.list$lowerbound[[ip]] <- list()
    param.list$upperbound[[ip]] <- list()
    param.list$lowerbound[[ip]][[1]] <- 0
    param.list$upperbound[[ip]][[1]] <- 1
  }
  
  p <- which(comments == "# log_Fmult_year1:")
  param.list$lowerbound[[p]] <- list()
  param.list$upperbound[[p]] <- list()
  param.list$lowerbound[[p]][[1]] <- rep(-15, length(dat[[p]][[1]]))
  param.list$upperbound[[p]][[1]] <- rep(2, length(dat[[p]][[1]]))
  
  p <- which(comments == "# log_Fmult_devs:")
  param.list$lowerbound[[p]] <- list()
  param.list$upperbound[[p]] <- list()
  for (i in 1:length(dat[[p]])){
    nvals <- length(dat[[p]][[i]])
    param.list$lowerbound[[p]][[i]] <- rep(-15, nvals)
    param.list$upperbound[[p]][[i]] <- rep(15, nvals)
  }
  
  p <- which(comments == "# log_recruit_devs:")
  param.list$lowerbound[[p]] <- list()
  param.list$upperbound[[p]] <- list()
  param.list$lowerbound[[p]][[1]] <- rep(-15, length(dat[[p]][[1]]))
  param.list$upperbound[[p]][[1]] <- rep(15, length(dat[[p]][[1]]))
  
  p <- which(comments == "# log_N_year1_devs:")
  param.list$lowerbound[[p]] <- list()
  param.list$upperbound[[p]] <- list()
  param.list$lowerbound[[p]][[1]] <- rep(-15, length(dat[[p]][[1]]))
  param.list$upperbound[[p]][[1]] <- rep(15, length(dat[[p]][[1]]))
  
  p <- which(comments == "# log_q_year1:")
  param.list$lowerbound[[p]] <- list()
  param.list$upperbound[[p]] <- list()
  param.list$lowerbound[[p]][[1]] <- rep(-30, length(dat[[p]][[1]]))
  param.list$upperbound[[p]][[1]] <- rep(5, length(dat[[p]][[1]]))
  
  p <- which(comments == "# log_q_devs:")
  param.list$lowerbound[[p]] <- list()
  param.list$upperbound[[p]] <- list()
  for (i in 1:length(dat[[p]])){
    nvals <- length(dat[[p]][[i]])
    param.list$lowerbound[[p]][[i]] <- rep(-15, nvals)
    param.list$upperbound[[p]][[i]] <- rep(15, nvals)
  }
  
  p <- which(comments == "# log_SR_scaler:")
  param.list$lowerbound[[p]] <- list()
  param.list$upperbound[[p]] <- list()
  param.list$lowerbound[[p]][[1]] <- -1
  param.list$upperbound[[p]][[1]] <- 200
  
  p <- which(comments == "# SR_steepness:")
  param.list$lowerbound[[p]] <- list()
  param.list$upperbound[[p]] <- list()
  param.list$lowerbound[[p]][[1]] <- 0.20001
  param.list$upperbound[[p]][[1]] <- 1.0
  
  # replace upper bound of logistic and double logistic selectivity parameters with nages
  asap <- asap.dat.obj$dat
  counter <- 0
  sel_block_option <- asap$sel_block_option
  nages <- asap$n_ages
  for (iblock in 1:asap$n_fleet_sel_blocks){
    if (sel_block_option[iblock] == 1){  # by age
      counter <- max(counter) + 1:nages
    }
    if (sel_block_option[iblock] == 2){  # single logistic
      counter <- max(counter) + 1:2
      for (ic in counter){
        param.list$upperbound[[ic]][[1]] <- nages        
      }
    }
    if (sel_block_option[iblock] == 3){  # double logistic
      counter <- max(counter) + 1:4
      for (ic in counter){
        param.list$upperbound[[ic]][[1]] <- nages
      }
    }
  }

  # repeat for index selectivity  
  ind <- seq(1, np)
  counter <- ind[comments == "# index_sel_params[1]:"] - 1
  if (counter > 0){
    index_sel_option <- asap$index_sel_option
    for (ind in 1:asap$n_indices){
      if (asap$use_index[ind] == 1){
        if (index_sel_option[ind] == 1){  # by age
          counter <- max(counter) + 1:nages
        }
        if (index_sel_option[ind] == 2){  # single logistic
          counter <- max(counter) + 1:2
          for (ic in counter){
            param.list$upperbound[[ic]][[1]] <- nages
          }
        }
        if (index_sel_option[ind] == 3){  # double logistic
          counter <- max(counter) + 1:4
          for (ic in counter){
            param.list$upperbound[[ic]][[1]] <- nages
          }
        }
      }
    }
  }

  return(param.list)
}

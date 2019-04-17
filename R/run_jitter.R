#' RunJitter
#' 
#' Run ASAP a number of times from different initial guesses.
#' @param wd directory where ASAP run is located
#' @param asap.name Base name of original dat file (without the .dat extension)
#' @param njitter number of realizations to run with different initial guesses
#' @param ploption "full" selects initial guesses from full range of possible values, "jitter" uses SS3.30 approach to select values close to parameter estimates (defaults to "jitter")
#' @param save.plots saves indivdual plots (defaults to TRUE)
#' @param od location where plot is saved (defaults to jitter subdirectory of wd)
#' @param plotf format for individual plots (defaults to 'png')
#' @param showtitle option to include title of numeric results on objective function plot (defaults to FALSE)
#' @export

RunJitter <- function(wd, asap.name, njitter, ploption="jitter", save.plots=TRUE, od=paste0(wd,"\\jitter\\"), plotf='png', showtitle=FALSE){
  
  # error checks for missing files 
  if (!file.exists(paste0(wd, "\\", asap.name, ".dat"))){
    return(paste0("Error: ", asap.name, ".dat not located in ", wd))
  }
  
  if (!file.exists(paste0(wd, "\\", asap.name, ".rdat"))){
    return(paste0("Error: ", asap.name, ".rdat not located in ", wd))
  }
  
  if (!file.exists(paste0(wd, "\\", asap.name, ".par"))){
    return(paste0("Error: ", asap.name, ".par not located in ", wd))
  }
  
  if (!file.exists(paste0(wd, "\\ASAP3.exe"))){
    return(paste0("Error: ASAP3.exe not located in ", wd))
  }
  
  # directory and file handling
  # need to use setwd approach because ASAP3 creates files in current working directory
  orig.dir <- getwd()
  setwd(wd) 
  fname <- paste0(asap.name, ".dat")
  rname <- paste0(asap.name, ".rdat")
  pname <- paste0(asap.name, ".par")
  
  asap.dat <- ReadASAP3DatFile(fname)
  asap.rdat <- dget(rname)
  asap.pin <- ReadASAP3PinFile(pname)

  # check for jitter subdirectory, create if necessary
  if (!dir.exists("./jitter")){
    shell("mkdir jitter")
  } 
  
  # copy ASAP3.exe into jitter dir
  shell("copy ASAP3.exe jitter/ASAP3.EXE")
  
  # change working directory to jitter dir and clean up files
  setwd("./jitter")
  shell("del jitter*.par", mustWork = NA, intern = TRUE)
  shell("del jitter*.pin", mustWork = NA, intern = TRUE)
  shell("del jitter*.rdat", mustWork = NA, intern = TRUE)
  shell("del jitter_objfxn.png", mustWork = NA, intern = TRUE)

  # write orig file with ignore_guesses flag=1 to file [base]_no_init_guesses.dat
  nname <- paste0(asap.name, "_no_init_guesses.dat")
  no.init.guesses <- asap.dat
  no.init.guesses$dat$ignore_guesses <-  1
  WriteASAP3DatFile(nname, no.init.guesses, "no initial guesses")
  
  # create base param.list using 
  # ploption = "full" for full range of parameters or 
  # ploption = "jitter" for values near parameter estimates
  param.list <- CreateParamList(asap.pin, asap.dat)
  param.list$type <- rep(ploption, length(param.list$type))

  # which parameters are not estimated
  fixed_params <- GetFixedParams(asap.dat)
  
  # check to make sure same number of parameters in par file and returned by GetFixedParams
  if (length(fixed_params[,1]) != length(param.list$type)){
    return("ERROR: different number of parameters in .par and calculated by GetFixedParams function")
  }
  
  # parameters not estimated stay at original values
  param.list$type[fixed_params[,2] == "fixed"] <- "fixed"
  
  # loop through njitter writing pin file with random values and running program
  objfxn <- rep(NA, njitter)
  nan <- NA # to deal with rdat file reporting nan as likelihood value
  for (ijit in 1:njitter){
    jname <- paste0("jitter", ijit, ".pin")
    asap.pin.jit <- JitterASAP(asap.pin, param.list)
    WriteASAP3PinFile(jname, asap.pin.jit)
    shell("del asap3.rdat", intern = TRUE)
    shell("del asap3.std", intern = TRUE)
    shell(paste("ASAP3.exe -ind", nname, "-ainp", jname), intern=TRUE)
    # use presence of .std file to indicate converged run
    if (file.exists("asap3.std")){
      shell(paste("copy asap3.rdat", paste0("jitter", ijit, ".rdat")), intern=TRUE)
      shell(paste("copy asap3.par", paste0("jitter", ijit, ".par")), intern=TRUE)
      asap <- dget("asap3.rdat")
      objfxn[ijit] <- asap$like$lk.total
      print(paste("jitter", ijit, "complete, objective function =", objfxn[ijit]))
    }else{
      print(paste("jitter", ijit, "did not converge"))
    }
  }
  
  # results list
  reslist <- list(objfxn=objfxn, orig_objfxn=asap.rdat$like$lk.total)
  # to determine which realizations had lower objective function value than original:
  # lowerobjfxn <- which(reslist$objfxn < reslist$orig_objfxn)
  
  # plot obj fxn results 
  PlotJitter(reslist, save.plots, od, plotf, showtitle)

  # change back to original directory
  setwd(orig.dir)
  
  return(reslist)
}


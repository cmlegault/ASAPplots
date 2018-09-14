#' Make selectivity decoder
#' 
#' Create csv file decoding selectivity parameters for fleet blocks and indices.
#' @param wd directory where ASAP run is located
#' @param asap name of the variable that read in the asap.rdat file
#' @param a1 list file produced by grab.aux.files function
#' @param index.names names of indices 
#' @param od output directory for plots and csv files 
#' @export

MakeSelectivityDecoder <- function(wd,asap,a1,index.names,od){
  asap.name <- a1$asap.name
  
  empty.df <- data.frame(Source = character(),
                         Name = character(),
                         NameNum = integer(),
                         SelType = character(),
                         Param = character(),
                         ParamNum = integer(),
                         InitGuess = double(),
                         Phase = integer(),
                         Lambda = double(),
                         CV = double())  
  
  fleet.selectivity.decoder.table <- empty.df
  index.selectivity.decoder.table <- empty.df
  
  nages <- asap$parms$nages
  
  # fleet selectivity blocks
  icount <- 1
  
  for (iblock in 1:asap$parms$nselblocks){
    
    # age specific parameters
    if (asap$fleet.sel.option[iblock] == 1){
      nparms <- nages
      start.row <- (iblock - 1) * (nages + 6) + 1
      end.row <- start.row + nparms - 1
      mySelType <- "Age Specific"
      myParam <- paste("Age", 1:nages)
    }

    # single logistic parameters
    if (asap$fleet.sel.option[iblock] == 2){
      nparms <- 2
      start.row <- (iblock - 1) * (nages + 6) + nages + 1
      end.row <- start.row + nparms - 1
      mySelType <- "Single Logistic"
      myParam <- c("A50", "Slope")
    }

    # double logistic parameters
    if (asap$fleet.sel.option[iblock] == 3){
      nparms <- 4
      start.row <- (iblock - 1) * (nages + 6) + nages + 2 + 1
      end.row <- start.row + nparms - 1
      mySelType <- "Double Logistic"
      myParam <- c("A50 ascending", "Slope ascending", "A50 descending", "Slope descending")
    }

    # add this block to table and increase icount
    thisdf <- data.frame(Source = "Fleet",
                         Name = paste("Selblock", iblock),
                         NameNum = iblock,
                         SelType = mySelType,
                         Param = myParam,
                         ParamNum = seq(icount, (icount + nparms - 1)),
                         InitGuess = asap$sel.input.mats$fleet.sel.ini[start.row:end.row, 1],
                         Phase = asap$sel.input.mats$fleet.sel.ini[start.row:end.row, 2],
                         Lambda = asap$sel.input.mats$fleet.sel.ini[start.row:end.row, 3],
                         CV = asap$sel.input.mats$fleet.sel.ini[start.row:end.row, 4])
    
    fleet.selectivity.decoder.table <- rbind(fleet.selectivity.decoder.table, thisdf)
    icount <- icount + nparms
  }
  
  # get fleet selectivity estimates, if available, and join to table
  if (any(!is.na(a1$asap.std))){
    fleet.sel.estimates <- a1$asap.std %>%
      data.frame(.) %>%
      filter(substr(name, 1, 10) == "sel_params") %>%
      mutate(ParamNum = readr::parse_number(name)) %>%
      mutate(Estimate = value, EstimatedStDev = stdev, EstimatedCV = stdev/value) %>%
      select(ParamNum, Estimate, EstimatedStDev, EstimatedCV)
    
    # join the selectivity table and estimates
    selectivity.decoder.table <- left_join(fleet.selectivity.decoder.table, fleet.sel.estimates, 
                                           by = "ParamNum")
  }else{
    selectivity.decoder.table <- fleet.selectivity.decoder.table
  }
  
  #--------------------
  # index selectivities
  icount <- 1
  
  # cannot get index sel with release version of ASAP3 because index.sel.options not saved in rdat
  # need to make a one-off version that has this added to rdat or else read from the dat file
  if (is.null(asap$control.parms$index.sel.option)){
    dat.file.index.sel.options <- GrabDatFileIndexSelOptions(wd,asap.name,asap)
    if (!is.null(dat.file.index.sel.options)){
      asap$control.parms$index.sel.option <- dat.file.index.sel.options
    }
  }
  
  if (!is.null(asap$control.parms$index.sel.option)){
    
    ind <- 0 # counter for indices that are used in estimation
    
    for (avail.ind in 1:asap$parms$nindices){
      
      # check to see if index used
      if (asap$initial.guesses$index.use.flag[avail.ind] == 1){
        
        ind <- ind + 1
        
        # check to make sure index selectivity not linked to a fleet
        if (asap$control.parms$index.sel.choice[ind] <= 0){
          # age specific parameters
          if (asap$control.parms$index.sel.option[ind] == 1){
            nparms <- nages
            start.row <- (avail.ind - 1) * (nages + 6) + 1
            end.row <- start.row + nparms - 1
            mySelType <- "Age Specific"
            myParam <- paste("Age", 1:nages)
          }
          
          # single logistic parameters
          if (asap$control.parms$index.sel.option[ind] == 2){
            nparms <- 2
            start.row <- (avail.ind - 1) * (nages + 6) + nages + 1
            end.row <- start.row + nparms - 1
            mySelType <- "Single Logistic"
            myParam <- c("A50", "Slope")
          }
          
          # double logistic parameters
          if (asap$control.parms$index.sel.option[ind] == 3){
            nparms <- 4
            start.row <- (avail.ind - 1) * (nages + 6) + nages + 2 + 1
            end.row <- start.row + nparms - 1
            mySelType <- "Double Logistic"
            myParam <- c("A50 ascending", "Slope ascending", "A50 descending", "Slope descending")
          }
          
          # add this block to table and increase icount
          thisdf <- data.frame(Source = "Index",
                               Name = index.names[ind],
                               NameNum = ind,
                               SelType = mySelType,
                               Param = myParam,
                               ParamNum = seq(icount, (icount + nparms - 1)),
                               InitGuess = asap$sel.input.mats$index.sel.ini[start.row:end.row, 1],
                               Phase = asap$sel.input.mats$index.sel.ini[start.row:end.row, 2],
                               Lambda = asap$sel.input.mats$index.sel.ini[start.row:end.row, 3],
                               CV = asap$sel.input.mats$index.sel.ini[start.row:end.row, 4])
          
          index.selectivity.decoder.table <- rbind(index.selectivity.decoder.table, thisdf)
          icount <- icount + nparms
        }
        
        # if index selectivity is linked to a fleet, just increase parameter counter
        if (asap$control.parms$index.sel.choice[ind] > 0){
          if (asap$control.parms$index.sel.option[ind] == 1) icount <- icount + nages
          if (asap$control.parms$index.sel.option[ind] == 2) icount <- icount + 2
          if (asap$control.parms$index.sel.option[ind] == 3) icount <- icount + 4
        }  
      }
    }
    
    # get index selectivity estimates, if available, and join to table
    if (any(!is.na(a1$asap.std))){
      index.sel.estimates <- a1$asap.std %>%
        data.frame(.) %>%
        filter(substr(name, 1, 16) == "index_sel_params") %>%
        mutate(ParamNum = readr::parse_number(name)) %>%
        mutate(Estimate = value, EstimatedStDev = stdev, EstimatedCV = stdev/value) %>%
        select(ParamNum, Estimate, EstimatedStDev, EstimatedCV)
      
      # join the selectivity table and estimates
      index.selectivity.decoder.table <- left_join(index.selectivity.decoder.table, index.sel.estimates,
                                                   by = "ParamNum")
    }
    selectivity.decoder.table <- rbind(selectivity.decoder.table, index.selectivity.decoder.table)
    
  }
  
  write.csv(selectivity.decoder.table, 
            file = paste0(od, "Selectivity_Decoder_", asap.name, ".csv"), row.names = FALSE)
  return()
}
  
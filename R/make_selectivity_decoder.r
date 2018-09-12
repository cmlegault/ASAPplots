#' Make selectivity decoder
#' 
#' Create csv file decoding selectivity parameters for fleet blocks and indices.
#' @param asap name of the variable that read in the asap.rdat file
#' @param a1 list file produced by grab.aux.files function
#' @param index.names names of indices 
#' @param od output directory for plots and csv files 
#' @export

PlotFleetSelBlocks <- function(asap,a1,index.names,od){
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
                         CV = double() )
                         #Estimate = double(),        # not sure this will be easy to get, but can try
                         #EstimatedStDev = double(),  
                         #EstimatedCV = double())  
  
  selectivity.decoder.table <- empty.df
  
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
    
    selectivity.decoder.table <- rbind(selectivity.decoder.table, thisdf)
    icount <- icount + nparms
  }
  
  # index selectivities
  icount <- 1
  
  # cannot get index sel with release version of ASAP3 because index.sel.options not saved in rdat
  # need to make a one-off version that has this added to rdat or else read from the dat file
  if (is.null(asap$control.parms$index.sel.option)){
    dat.file.index.sel.options <- GrabDatFileIndexSelOptions(asap.name,asap)
    if (!is.null(dat.file.index.sel.options)){
      asap$control.parms$index.sel.option <- dat.file.index.sel.options
    }
  }
  
  if (!is.null(asap$control.parms$index.sel.option)){
    
    for (ind in 1:asap$parms$nindices){
      
      # first check to make sure index selectivity not linked to a fleet
      if (asap$control.parms$index.sel.choice[ind] <= 0){
        # age specific parameters
        if (asap$control.parms$index.sel.option[ind] == 1){
          nparms <- nages
          start.row <- (ind - 1) * (nages + 6) + 1
          end.row <- start.row + nparms - 1
          mySelType <- "Age Specific"
          myParam <- paste("Age", 1:nages)
        }
        
        # single logistic parameters
        if (asap$control.parms$index.sel.option[ind] == 2){
          nparms <- 2
          start.row <- (ind - 1) * (nages + 6) + nages + 1
          end.row <- start.row + nparms - 1
          mySelType <- "Single Logistic"
          myParam <- c("A50", "Slope")
        }
        
        # double logistic parameters
        if (asap$control.parms$index.sel.option[ind] == 3){
          nparms <- 4
          start.row <- (ind - 1) * (nages + 6) + nages + 2 + 1
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
        
        selectivity.decoder.table <- rbind(selectivity.decoder.table, thisdf)
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
  
  write.csv(selectivity.decoder.table, 
            file = paste0(od, "Selectivity_Decoder_", asap.name, ".csv"), row.names = FALSE)
  return()
}
  
#' PlotJitter
#' 
#' Plot results of jitter analysis, objective function values by realization with line for original value.
#' @param reslist result of RunJitter function containing objective function values by realization and original value
#' @param save.plots saves indivdual plots 
#' @param od directory where plot will be saved
#' @param plotf format for individual plots 
#' @param showtitle option to add title for plot providing number of realizations better than original, equal to original, and not converged
#' @param ymaxlimit replace any objfxn value greater than this value with this value in plot (default = NULL means no replacement of any values)
#' @export

PlotJitter <- function(reslist, save.plots, od, plotf, showtitle, ymaxlimit=NULL){
  
  nna <- length(reslist$objfxn[is.na(reslist$objfxn)])
  neq <- length(which(reslist$objfxn == reslist$orig_objfxn))
  loreals <- which(reslist$objfxn < reslist$orig_objfxn)
  
  if (length(loreals) == 1){
    title1 <- paste0("Realization ", loreals, " is less than original")
  }else if (length(loreals > 1)){
    title1 <- paste0(length(loreals), " realizations are less than original")
  }else{
    title1 <- "No realizations had objective function lower than original"
  }
  
  title2 <- paste0(nna, " realizations did not converge and ", neq, " had same objfxn as orig")
  maintitle <- paste0(title1, "\n", title2)

  showreplaced <- FALSE
  if (!is.null(ymaxlimit)){
    replacevals <- which(reslist$objfxn > ymaxlimit)
    nreplacevals <- length(replacevals)
    if (nreplacevals > 0){
      showreplaced <- TRUE
      reslist$objfxn[replacevals] <- ymaxlimit
    }
  }
  
  windows(record = TRUE)
  
  plot(1:length(reslist$objfxn), reslist$objfxn, xlab = "Realization", ylab = "Objective Function")
    abline(h = reslist$orig_objfxn, col="red")
    if (showtitle == TRUE) title(main = maintitle)
    if (showreplaced == TRUE) points(replacevals, rep(ymaxlimit, nreplacevals), pch=17)
  if (save.plots==TRUE) savePlot(paste0(od, "jitter_objfxn.", plotf), type=plotf)

  return()
}

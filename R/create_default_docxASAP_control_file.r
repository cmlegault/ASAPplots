#' createDefaultdocxASAPcontrolfile
#' 
#' Create docxASAP.csv with default settings.
#' @param wd directory where ASAP run is located
#' @export

createDefaultdocxASAPcontrolfile <- function(wd){
  
  # first check to see if docxASAP.csv already exists, don't overwrite if it does
  if (file.exists(paste0(wd, "\\docxASAP.csv")) == TRUE){
    print("docxASAP.csv already present in directory")
    print("rename or delete current docxASAP.csv before creating new default file")
    return()
  }
  
  # FigureText
  x <- rep(NA, 84)
  x[1] <- "Overview of ASAP run being summarized, model run"
  x[2] <- "Parameters with high correlations from model run"
  x[3] <- "Parameters with coefficients of variation (CV) less than 0.05 (asterisk) or greater than 0.55 (x) from model run"
  x[4] <- "Root mean square error (RMSE) for survey indices from model run"
  x[5] <- "Root mean square error (RMSE) for catch by fleet from model run"
  x[6] <- "Fit diagnostics for the catch by fleet from model run"
  x[7] <- "Fit diagnostics for the discards by fleet from model run"
  x[8] <- "Catch age composition observed and predicted by year and fleet from model run"
  x[9] <- "Fleet-specific age composition Pearson residuals for catch from model run"
  x[10] <- "Discard age composition observed and predicted by year and fleet from model run"
  x[11] <- "Fleet-specific age composition Pearson residuals for discards from model run"
  x[12] <- "Input and estimated effective sample size by fleet for catch from model run"
  x[13] <- "Francis mean age plots for catch from model run"
  x[14] <- "Francis qq plots for catch from model run"
  x[15] <- "Input and estimated effective sample size by fleet for discards from model run"
  x[16] <- "Francis mean age plots for discards from model run"
  x[17] <- "Francis qq plots for discards from model run"
  x[18] <- "Fit diagnostics for the indices from model run"
  x[19] <- "Index-specific age composition Pearson residuals from model run"
  x[20] <- "Input and estimated effective sample size by index from model run"
  x[21] <- "Francis mean age plots for indices from model run"
  x[22] <- "Francis qq plots for indices from model run"
  x[23] <- "Selectivity at age by block and fleet from model run"
  x[24] <- "Fishing mortality rate multiplier by fleet and year from model run"
  x[25] <- "Index selectivity at age from model run"
  x[26] <- "Catch curves by cohort and fleet from model run"
  x[27] <- "Catch curves by cohort and index from model run"
  x[28] <- "Total mortality estimates using method of Sinclair by index from model run"
  x[29] <- "Sinclair total mortality regressions by index and moving window of years from model run"
  x[30] <- "Sinclair total mortality residual distributions at age by index frommodel run"
  x[31] <- "Catch at age consistency plots for catch by fleet from model run"
  x[32] <- "Catch at age consistency plots by index frommodel run"
  x[33] <- "Index catchability with approximate 95% confidence intervals from model run"
  x[34] <- "Spawning stock biomass and fishing mortality rates by year from model run"
  x[35] <- "Comparison of biomass estimates estimated at the start of the year from model run"
  x[36] <- "Spawning stock biomass at age by year from model run"
  x[37] <- "Proportion of spawning stock biomass at age by year from model run"
  x[38] <- "Population numbers at age by year from model run"
  x[39] <- "Proportion of population numbers at age by year from model run"
  x[40] <- "Recruitment deviations by year from model run"
  x[41] <- "Stock-recruitment plot from model run"
  x[42] <- "Recruits per spawning stock biomass by year from model run"
  x[43] <- "Stock-recruitment plot with predicted line from model run"
  x[44] <- "Spawning stock biomass and recruitment by year from model run"
  x[45] <- "Coefficient of variation (CV) by year for recruitment, spawning stock biomass, and Freport from model run"
  x[46] <- "Retrospective plots of Freport, spawning stock biomass, and recruitment from model run"
  x[47] <- "Retrospective plots of January 1 biomass, exploitable biomass, and total stock numbers from model run"
  x[48] <- "Retrospective plots of population numbers at age from model run"
  x[49] <- "Yield per recruit and spawning potential ratio by fishing mortality rate from model run"
  x[50] <- "Spawning potential ratio target curves from model run"
  x[51] <- "Overall selectivity and maturity at age  in recent years from model run"
  x[52] <- "Expected number of spawnings and spawning potential ratio by fishing mortality rate from model run"
  x[53] <- "Annual fishing mortality rates at a range of spawning potential ratios from model run"
  x[54] <- "Annual yield per recruit at a range of spawning potential ratios from model run"
  x[55] <- "Distribution of annual  fishing mortality rates at a range of spawning potential ratios frommodel run"
  x[56] <- "Distribution of annual yield per recruit at a range of spawning potential ratios from model run"
  x[57] <- "Distribution of annual MSY reference points from model run"
  x[58] <- "Distribution of annual MSY reference points from model run"
  x[59] <- "Maximum sustainable yield (MSY) steepness (h) versus spawning potential ratio (SPR) from model run"
  x[60] <- "Annual maximum sustainable yield (MSY) from model run"
  x[61] <- "Annual spawning potential ratio (SPR) at maximum sustainable yield (MSY) from model run"
  x[62] <- "Annual spawning stock biomass (SSB) at maximum sustainable yield (MSY) from model run"
  x[63] <- "Annual recruitment at maximum sustainable yield (MSY) from model run"
  x[64] <- "Trace for spawning stock biomass in first and last year from model run"
  x[65] <- "Trace for fishing mortality rate in first and last year from model run"
  x[66] <- "Cumulative quantiles for fishing mortality rate and spawning stock biomass from model run"
  x[67] <- "Autocorrelation of spawning stock biomass at different lags from model run"
  x[68] <- "Autocorrelation of fishing mortality rate at different lags from model run"
  x[69] <- "Distribution of spawning stock biomass in first and last year from model run"
  x[70] <- "Distribution of fishing mortality rate in first and last year from model run"
  x[71] <- "Distribution of fishing mortality rate in first and last year from model run"
  x[72] <- "Distribution of January 1 biomass in first and last year from model run"
  x[73] <- "Annual spawning stock biomass with 90% probability intervals from model run"
  x[74] <- "Annual fishing mortality rate with 90% probability intervals from model run"
  x[75] <- "Annual fishing mortality rate with 90% probability intervals from model run"
  x[76] <- "Annual January 1 biomass with 90% probability intervals from model run"
  x[77] <- "Catch by fleet and year from model run"
  x[78] <- "Catch proportions by fleet and year from model run"
  x[79] <- "Catch by age and year by fleet from model run"
  x[80] <- "Discards by age and year by fleet from model run"
  x[81] <- "Rescaled indices by year on normal and lognormal scales from model run"
  x[82] <- "Weight at age by year from model run"
  x[83] <- "Natural mortality rate at age from model run"
  x[84] <- "Maturity at age from model run"
  
  # create the default docxASAP.csv values
  my.ini <- data.frame(Include = c("No","Yes","Yes","Yes","Yes","Yes","Yes","No","Yes","No","Yes","Yes","Yes","No","Yes","Yes","No","Yes","Yes","Yes","Yes","No","Yes","Yes","Yes","No","No","Yes","No","No","Yes","Yes","Yes","Yes","No","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","No","Yes","Yes","Yes","Yes","Yes","Yes","No","No","No","No","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","No","No","Yes","Yes","No","No","Yes","Yes","Yes","Yes","Yes","Yes","No","No"),
                       Order = seq(1, 84),
                       Number = NA,
                       Group = c("Front", rep("Diagnostics", 21), rep("Results", 23), rep("Retro", 3), rep("RefPoints", 15), rep("MCMC", 13), rep("InputData", 8)),
                       ASAPplot = c("Front.Page","Corr.Limit.Count.by.par","CV.check.params","RMSE.95CI.Indices","RMSE.95CI.Catch","Catch.4panel","Discard.4panel","Catch.Age.Comp.F","Pearson.catch.resid.bubble.plots","Discard.Age.Comp.F","Pearson.discard.resid.bubble.plots","Catch.Neff.F","Catch.ESS_Mean_Age_Fleet_","Catch.ESS_QQplot_Fleet_","Discard.Neff.F","Discard.ESS_Mean_Age_Fleet_","Discard.ESS_QQplot_Fleet_","Index.4panel","Pearson.index.resid.bubble.plots","Index.Neff","Francis_Mean_Age_Orig_Ind_","Francis_QQ_Orig_Ind_","Catch.Sel.Blocks.Fleet","Fmult.by.fleet","Index.Sel","catch_curve_Catch for Fleet","catch_curve_Index","Sinclair_Z_estimates_index_","Sinclair_Z_regress_index_","Sinclair_Z_resids_index_","cohort_age_matrix_Catch for Fleet","cohort_age_matrix_Index","Q.Index.Err.Bars","SSB.F.Timeseries.Line","Biomass.Comparisons","SSB.AA.barplot.stacked","SSB.AA.proportion.barplot.stacked","NAA.barplot.stacked","NAA.proportion.barplot.stacked","Recruit.Yr.Devs","S_R.Yr","RpSSB_time_series","S_R.Pred.Line","SARC.SSB.vs.R.barplots","CV.estimates","retro_F_SSB_R","retro_Jan1B_ExplB_StockN","retro_ages_","YPR.SPR.Curves","SPR.Target.Curves","Selectivity.vs.Maturity","Exp.Spawn.SPR.Curves","Annual.FSPR","Annual.YPR","Annual.F_SPR.4panel.hist","Annual.YPR_SPR.4panel.hist","MSY.4panel.hist1","MSY.4panel.hist2","MSY.h.vs.SPR","Annual.MSY","Annual.SPR.MSY","Annual.SSB.MSY","Annual.Recr.MSY","Trace.SSB.first.last.yr","Trace.Freport.first.last.yr","Cumu.F_and_SSB.first.last.yr","lag.autocorrelation.SSB","lag.autocorrelation.Freport","Distribution.SSB.first.last.yr","Distribution.Freport.first.last.yr","Distribution.Fmult.first.last.yr","Distribution.Jan1.B.first.last.yr","SSB.90PI","Freport.90PI","Full.F.90PI","Jan1.B.90PI","catch.by.fleet","catch.proportions.by.fleet","obs.catch.bubble.plots","obs.discard.bubble.plots","index.input","waa.matrix","M","maturity"),
                       FigureText = x)
  
  write.csv(my.ini, file = paste0(wd, "\\docxASAP.csv"), row.names = FALSE)
  return()
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Sinclair_Z_functions.r
\name{get_Sinclair_Z}
\alias{get_Sinclair_Z}
\title{Get Sinclair Z}
\usage{
get_Sinclair_Z(asap, a1, index.names, save.plots, od, plotf)
}
\arguments{
\item{asap}{name of the variable that read in the asap.rdat file}

\item{a1}{list file produced by grab.aux.files function}

\item{index.names}{names of indices}

\item{save.plots}{save individual plots}

\item{od}{output directory for plots and csv files}

\item{plotf}{type of plot to save}
}
\description{
Set of functions to prepare index matrices and compute and plot Sinclair Z. Estimate Z from catch curves using 4 year moving window with cohort-specific coefficients. Based on Sinclair 2001 ICES J Mar Sci 58: 1-10. Note hard wired to use only ages that are at least 90 percent selected.
}

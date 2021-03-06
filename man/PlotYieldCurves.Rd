% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_yield_curves.r
\name{PlotYieldCurves}
\alias{PlotYieldCurves}
\title{Plot yield curves}
\usage{
PlotYieldCurves(asap, a1, nyrs.ave, save.plots, od, plotf)
}
\arguments{
\item{asap}{name of the variable that read in the asap.rdat file}

\item{a1}{list file produced by grab.aux.files function}

\item{nyrs.ave}{number of years to average for calculating Reference Points}

\item{save.plots}{save individual plots}

\item{od}{output directory for plots and csv files}

\item{plotf}{type of plot to save}
}
\description{
Plot yield and SSB per recruit as a function of F and create associated csv file.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grab_names.r
\name{GrabNames}
\alias{GrabNames}
\title{Grab Names}
\usage{
GrabNames(wd, asap.name, asap)
}
\arguments{
\item{wd}{directory where ASAP run is located}

\item{asap.name}{Base name of original dat file (without the .dat extension)}

\item{asap}{name of the variable that read in the asap.rdat file}
}
\value{
list of fleet names and index names
}
\description{
This function gets fleet and index names from the original ASAP.DAT file.
}

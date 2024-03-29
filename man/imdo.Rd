% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imdo.R
\name{imdo}
\alias{imdo}
\alias{imdoPlot}
\title{Iterative Missing Data Optimization (IMDO)}
\usage{
imdo(x, groups = NULL, plot = TRUE)

imdoPlot(opt.smry, equal.axes = FALSE)
}
\arguments{
\item{x}{data.frame or matrix to optimize.}

\item{groups}{vector of groups as long as number of rows in \code{x}.}

\item{plot}{generate a plot of the optimization results.}

\item{opt.smry}{data.frame of optimization summary results from run of 
\code{imdo} in (\code{$opt.smry} element).}

\item{equal.axes}{show imdo plot with both axes on same scale?}
}
\description{
Identify optimal combination of variables to minimize 
  number of samples with missing data.
}
\author{
Eric Archer \email{eric.archer@noaa.gov}
}

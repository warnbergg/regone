% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CreateDffitsPlot.R
\name{CreateDffitsPlot}
\alias{CreateDffitsPlot}
\title{CreateDffitsPlot}
\usage{
CreateDffitsPlot(fit, critical.value = NULL, save.plot = TRUE)
}
\arguments{
\item{fit}{lm object. Linear Model fit to the data. No default.}

\item{critical.value}{Numeric vector of length 1. Cutoff/critical value for DFFITS plot. Defaults to NULL, in which case no critical values are plotted.}

\item{save.plot}{Logical vector of length 1. If TRUE the DFFITS plot are saved to disk. Defaults to TRUE.}
}
\description{
Computes the DFFITS statstic for the fitted model, plots them, and optionally saves the to disk.
}
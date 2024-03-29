% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CreateAddedVariablePlots.R
\name{CreateAddedVariablePlots}
\alias{CreateAddedVariablePlots}
\title{AddedVariablePlots}
\usage{
CreateAddedVariablePlots(nms, fit, dir = "./", save.plot = TRUE)
}
\arguments{
\item{nms}{Character vector. Regressors to be plotted against residuals. Each regressor is individually plotted against the externally studentized residuals. No default.}

\item{fit}{lm object. LM model that has been fit to the data. No default.}

\item{dir}{Character vector of lenght 1. Directory in which to store the plot. Ignored if save.plot is FALSE. Defaults to "./"}

\item{save.plot}{Logical vector of length 1. If TRUE the added variable plots are saved to disk. Defaults to TRUE.}
}
\description{
Plot added variable plots. Much credit to: https://stackoverflow.com/questions/59150905/is-there-a-ggplot2-analogue-to-the-avplots-function-in-r
}

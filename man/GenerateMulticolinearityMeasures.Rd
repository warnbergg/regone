% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GenerateMulticolinearityMeasures.R
\name{GenerateMulticolinearityMeasures}
\alias{GenerateMulticolinearityMeasures}
\title{GenerateMulticolinearityMeasures}
\usage{
GenerateMulticolinearityMeasures(data, dv, fit, dir = "./", save.plots = TRUE)
}
\arguments{
\item{data}{data.frame Data used to fit the model. Used for pair-wise correlation analysis and eigenvalue system analysis. No default.}

\item{dv}{Character vector of length 1. Dependent variable. No default.}

\item{fit}{lm object. Fitted model. No default}

\item{dir}{Character vector of lenght 1. Directory in which to store the plot. Ignored if save.plot is FALSE. Defaults to "."}

\item{save.plots}{Logical vector of length 1. If TRUE the VIF plot and correlation heatmap are saved to disk. Defaults to TRUE.}
}
\description{
Produces variance inflation factors, eigenvalue system analysis for the fitted model.
}

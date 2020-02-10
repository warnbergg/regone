% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BootstrapEstimates.R
\name{BootstrapEstimates}
\alias{BootstrapEstimates}
\title{BoostrapEstimates}
\usage{
BootstrapEstimates(data, ...)
}
\arguments{
\item{data}{data.frame. Data to fit the lm model to. No default.}

\item{...}{Additional arguments for the boot function Description. Default/No default.}
}
\description{
Uses the boot package to bootstrap the regression coefficients for our fitted model.
}
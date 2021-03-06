% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tissot.R
\name{plot}
\alias{plot}
\alias{plot.tissot}
\title{Plot indicatrix}
\usage{
\method{plot}{tissot}(x, ..., crs = "auto", areachange = FALSE)
}
\arguments{
\item{x}{tissot object}

\item{...}{ignored}

\item{crs}{target coordinate reference system: object of class 'crs', or input string for st_crs,
default is automatic}

\item{areachange}{Boolean to specify if you want to plot the area change or not in percentage, Default is FALSE}
}
\description{
This function plots the sf geometry and Tissot indicatrix circles that were generated.
ggplot2 library required.
}

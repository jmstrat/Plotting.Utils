% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/axis.break.R
\name{axis.break.box}
\alias{axis.break.box}
\title{Put a box around a plot, leaving gaps for axis breaks on the x and / or y axes}
\usage{
axis.break.box(axis = 1, breakpos, brw = 0.02)
}
\arguments{
\item{axis}{Either a number (1-4) or a vector of 2 numbers c(1,2) choosing the axes to break}

\item{breakpos}{If only one axis is provided, then a single number corresponding to the point at which to break the axis, otherwise a vector of two numbers c(x,y)}

\item{brw}{If only one axis is provided, then a single number corresponding to the width of the break as a fraction of the total width, otherwise a vector of two numbers c(x,y)}
}
\description{
Based on \code{\link[plotrix]{axis.break}}. The x axis can be specified as either 1 or 3, the y axis as 2 or 4
}

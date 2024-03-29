% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Pretty.R
\name{pretty_axes}
\alias{pretty_axes}
\title{Draws a bounding box and pretty axes}
\usage{
pretty_axes(
  xlim = c(0, 1),
  ylim = c(0, 1),
  y2lim = NA,
  axes = c(1, 2),
  drawBox = TRUE,
  frac = FALSE,
  div = 1,
  flexible = TRUE,
  xlab = NULL,
  ylab = NULL,
  y2lab = NULL,
  line = -0.6,
  labline = NULL,
  lowerTickLimit = c(NA, NA, NA),
  lowerLabelLimit = c(NA, NA, NA),
  upperTickLimit = c(NA, NA, NA),
  upperLabelLimit = c(NA, NA, NA),
  forcedInterval = NA,
  forcePrint = FALSE,
  ticklabels = c(T, T, T),
  cex = par("cex"),
  ticksOut = c(T, T, T),
  tcl = 0.4,
  centreTitlesToLabels = c(F, F, F),
  ...
)
}
\arguments{
\item{xlim, ylim, y2lim}{Numeric vectors of length 2, giving the x, y (and y2) coordinate ranges.}

\item{axes}{Which axes to draw c(1,2,3,4) -- 1 & 3 == x; 2=y; 4=y2}

\item{frac}{Allow fractional intervals?}

\item{div}{Factor by which the minor tick interval is smaller than the major tick interval c(x,y,y2)}

\item{flexible}{Allow min and max to be adjusted for prettier results?}

\item{xlab, ylab, y2lab}{Titles for the x,y,y2 axes}

\item{labline}{Which margin line to draw the labels on c(x,x2,y,y2)}

\item{lowerTickLimit, upperTickLimit}{Limits beyond which not to draw ticks}

\item{lowerLabelLimit, upperLabelLimit}{Limits beyond which not to draw labels}

\item{forcedInterval}{Override the calculated tick interval}

\item{forcePrint}{Attempt to print all labels even if they are closely spaced}

\item{ticklabels}{Print labels?}

\item{ticksOut}{Ticks point outwards?}

\item{centreTitlesToLabels}{Centre the axis title relative to the labels rather than the axis?}

\item{...}{Additional parameters are passed to the underlying function calls}
}
\description{
Draws a bounding box and axes with tick positions generated by \code{\link{pretty_ticks}}
}

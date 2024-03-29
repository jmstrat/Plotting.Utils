% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_plot_label.R
\name{add_plot_label}
\alias{add_plot_label}
\title{Add a label to a plot}
\usage{
add_plot_label(
  label,
  xoffset = 0.05,
  yoffset = 0.05,
  align = "left",
  inside = TRUE,
  ...
)
}
\arguments{
\item{label}{The label text}

\item{xoffset}{Offset in inches from the left limit of the plot}

\item{yoffset}{Offset in inches from the top limit of the plot}

\item{align}{Align to the left or right of the plot}

\item{inside}{Label should be inside the box? : c(x,y) where x,y are T/F (or just T/F)}

\item{...}{Additional parameters passed to \code{\link{text}}}
}
\description{
Add a label to a plot
}

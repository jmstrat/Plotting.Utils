#' Creates a blank plot with the specified limits
#'
#' @param xlim,ylim Numeric vectors of length 2, giving the x and y coordinates ranges.
#' @export
new_plot <- function(xlim,ylim) {
  plot.new()
  plot.window(xlim=xlim,ylim=ylim)
}


#' Makes a new plot with pretty axes
#'
#' Makes a new plot with \code{\link{new_plot}} and a bounding box and axesusing \code{\link{pretty_axes}}
#' @param x_axis,y_axis Side on which to draw the x,y axes
#' @param xlab,ylab Titles for the x,y axes
#' @param xlim,ylim Numeric vectors of length 2, giving the x and y coordinates ranges.
#' @inheritParams pretty_axes
#' @export
pretty_plot <- function(xlim,ylim, x_axis=1, y_axis=2, frac=FALSE,div=1,ensureZero=TRUE, xlab=NA, ylab=NA,...) {
  new_plot(xlim,ylim)
  if(xlim[[2]]<xlim[[1]]) xlim=rev(xlim)
  if(ylim[[2]]<ylim[[1]]) ylim=rev(ylim)
  pretty_axes(c(xlim[[1]],ylim[[1]]),c(xlim[[2]],ylim[[2]]),x_axis,y_axis,frac,div,ensureZero,...)

  ##Add axis titles
  x_line=1.4
  y_line=1.6
  if(is.na(x_axis)) {x_line=0.2; x_axis=1}
  if(is.na(y_axis)) {y_line=0.2; y_axis=2}
  if(!is.na(xlab)) mtext(side = x_axis, xlab, line = x_line,...)
  if(!is.na(ylab)) mtext(side = y_axis, ylab, line = y_line,...)
}

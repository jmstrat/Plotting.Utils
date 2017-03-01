#' Creates a blank plot with the specified limits
#'
#' @param xlim,ylim Numeric vectors of length 2, giving the x and y coordinates ranges.
#' @export
new_plot <- function(xlim,ylim) {
  plot.new()
  plot.window(xlim=xlim,ylim=ylim)
}

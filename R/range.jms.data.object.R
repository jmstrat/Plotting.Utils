#' Calculate Range
#'
#' This function calculates the range of a data objects
#' @param x The data object
#' @return
#' The range
#' @examples
#' range(data)
#' @export
range.jms.data.object <- function(x,...) {
  if(!is.jms.data.object(x)) stop("x must be a jms.data.object")
  range.default(x[,ycol(x)],na.rm=TRUE)
}

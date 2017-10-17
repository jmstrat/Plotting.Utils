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
  y_cols=attr(x,'y_column')
  if(is.null(y_cols)) {
    warning('Data type unknown, assuming last column for y axis')
    y_cols=ncol(x)
  }
  range.default(x[,y_cols],na.rm=TRUE)
}

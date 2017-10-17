#' Get or set the x or y column(s) for a data object
#'
#' @param x The data object
#' @return The column(s)
#' @rdname xycols.jms
#' @export
xcol <- function(x) UseMethod("xcol")
#' @export
xcol.default <- function(x) {
  stop("Unable to get x data for this class")
}
#' @export
xcol.jms.data.object <- function(x) {
  xcol=attr(x,'x_column')
  if(is.null(xcol)) {
    if(ncol(x)<=1) stop('Cannot get x column')
    warning('Data type unknown, assuming 1st column for x axis')
    xcol=1
  }
  xcol
}
#' @rdname xycols.jms
#' @export
`xcol<-` <- function(x,value) UseMethod("xcol<-")
#' @export
`xcol<-.default` <- function(x,value) {
  stop("Unable to assign x column for this class")
}
#' @export
`xcol<-.jms.data.object` <- function(x,value) {
  attr(x,'x_column')<-value
  x
}


#' @rdname xycols.jms
#' @export
ycol <- function(x) UseMethod("ycol")
#' @export
ycol.default <- function(x) {
  stop("Unable to get y data for this class")
}
#' @export
ycol.jms.data.object <- function(x) {
  ycol=attr(x,'y_column')
  if(is.null(ycol)) {
    if(ncol(x)<=1) stop('Cannot get y column')
    warning('Data type unknown, assuming 2nd column for y axis')
    ycol=2
  }
  ycol
}
#' @rdname xycols.jms
#' @export
`ycol<-` <- function(x,value) UseMethod("ycol<-")
#' @export
`ycol<-.default` <- function(x,value) {
  stop("Unable to assign y column for this class")
}
#' @export
`ycol<-.jms.data.object` <- function(x,value) {
  attr(x,'y_column')<-value
  x
}

#' Get or set the x or y column labels for a data object
#'
#' @param x The data object
#' @return The column labels
#' @rdname xylabels.jms
#' @export
xlab <- function(x) UseMethod("xlab")
#' @export
xlab.default <- function(x) {
  stop("Unable to get x data for this class")
}
#' @export
xlab.jms.data.object <- function(x) {
  xlab=attr(x,'x_type')
  if(is.null(xlab)) xlab='Unknown'
  xlab
}
#' @rdname xylabels.jms
#' @export
`xlab<-` <- function(x,value) UseMethod("xlab<-")
#' @export
`xlab<-.default` <- function(x,value) {
  stop("Unable to assign x column for this class")
}
#' @export
`xlab<-.jms.data.object` <- function(x,value) {
  attr(x,'x_type')<-value
  x
}

#' @rdname xylabels.jms
#' @export
ylab <- function(x) UseMethod("ylab")
#' @export
ylab.default <- function(x) {
  stop("Unable to get y data for this class")
}
#' @export
ylab.jms.data.object <- function(x) {
  ylab=attr(x,'y_type')
  if(is.null(ylab)) ylab='Unknown'
  ylab
}
#' @rdname xylabels.jms
#' @export
`ylab<-` <- function(x,value) UseMethod("ylab<-")
#' @export
`ylab<-.default` <- function(x,value) {
  stop("Unable to assign y column for this class")
}
#' @export
`ylab<-.jms.data.object` <- function(x,value) {
  attr(x,'y_type')<-value
  x
}

xlab_<-function(...)xlab(...)
ylab_<-function(...)ylab(...)

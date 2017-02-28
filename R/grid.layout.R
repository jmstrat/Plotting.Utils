#' Layout plots in grid form
#'
#' @param n number of plots
#' @param show Show the resultant layout using \code{\link{layout.show}}?
#' @export
grid.layout <- function(n, show=FALSE) {
  if(n==1) {
    layout(1)
  } else if(n==2) {
    layout(matrix(c(1,2),byrow=T))
  } else if(n==3) {
    layout(matrix(c(1,1,2,2,0,3,3,0),2,4,byrow=T))
  } else if(n==4) {
    layout(matrix(c(1,2,3,4),2,2,byrow=T))
  } else if(n==5) {
    layout(matrix(c(1,1,2,2,3,3,0,4,4,5,5,0),2,6,byrow=T))
  } else if(n==6) {
    layout(matrix(c(1,2,3,4,5,6),2,3,byrow=T))
  } else if(n==7) {
    layout(matrix(c(1,2,3,4,5,6,0,7,0),3,3,byrow=T))
  } else if(n==8) {
    layout(matrix(c(1,1,2,2,3,3,4,4,5,5,6,6,0,7,7,8,8,0),3,6,byrow=T))
  } else if(n==9) {
    layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow=T))
  } else {
    stop('Unsupported number of plots passed to grid.layout')
  }
  if(show) layout.show(n)
}

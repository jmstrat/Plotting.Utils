#' Add a label to a plot
#'
#' @param xoffset Offset in inches from the left limit of the plot
#' @param yoffset Offset in inches from the top limit of the plot
#' @param label The label text
#' @param ... Additional parameters passed to \code{\link{text}}
#' @export
add_plot_label <- function(xoffset=0,yoffset=0,label,...) {
  xin=grconvertX(xoffset, from = "inches", to = "ndc")
  yin=grconvertY(yoffset, from = "inches", to = "ndc")

  fig=par('fig')

  xpos=grconvertX(fig[[1]]+xin, from = "ndc", to = "user")
  ypos=grconvertY(fig[[4]]-yin, from = "ndc", to = "user")

  text(xpos,ypos,label,pos=4,xpd=NA,offset=0)
}

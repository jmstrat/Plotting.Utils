#' Add a label to a plot
#'
#' @param xoffset Offset in inches from the left limit of the plot
#' @param yoffset Offset in inches from the top limit of the plot
#' @param align Align to the left or right of the plot
#' @param inside Label should be inside the box? : c(x,y) where x,y are T/F (or just T/F)
#' @param label The label text
#' @param ... Additional parameters passed to \code{\link{text}}
#' @export
add_plot_label <- function(label, xoffset=0.05,yoffset=0.05,align='left',inside=TRUE,...) {
  xin=grconvertX(xoffset, from = "inches", to = "ndc")
  yin=grconvertY(yoffset, from = "inches", to = "ndc")

  if(length(inside)==1) inside=c(inside,inside)

  if(inside[[1]]) xcord='npc' else xcord='nfc'
  if(inside[[2]]) ycord='npc' else ycord='nfc'

  plt_ndc=c(grconvertX(0,from=xcord,to='ndc'),
            grconvertX(1,from=xcord,to='ndc'),
            grconvertY(0,from=ycord,to='ndc'),
            grconvertY(1,from=ycord,to='ndc'))

  ypos=grconvertY(plt_ndc[[4]]-yin, from = "ndc", to = "user")
  if(align=='left') {
    xpos=grconvertX(plt_ndc[[1]]+xin, from = "ndc", to = "user")
    text(xpos,ypos,label,xpd=NA,adj=c(0,1),...)
  } else if(align=='right') {
    xpos=grconvertX(plt_ndc[[2]]-xin, from = "ndc", to = "user")
    text(xpos,ypos,label,xpd=NA,adj=c(1,1),...)
  } else {
    stop('Align must be one of left / right')
  }
}

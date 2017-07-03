#' Add a horizontal curly brace to a plot
#'
#' @param start x coordinate of the start of the brace
#' @param end x coordinate of the end of the brace
#' @param width Width of the brace in inches
#' @param height Height of the brace in inches
#' @param ... Additional parameters passed to \code{\link{lines}}
#' @export
CurlyBrace <- function(start, end, width,height,...) {
  width=xinch(width)
  height=yinch(height)
  # Transform from axis to screen coordinates
  th = atan2(end[[2]]-start[[2]], end[[1]]-start[[1]])
  c1 = start + width*c(cos(th), sin(th))
  c2 = 0.5*(start+end) + 2*c(-width*sin(th), height*cos(th)) - c(width*cos(th), height*sin(th))
  c3 = 0.5*(start+end) + 2*c(-width*sin(th), height*cos(th)) + c(width*cos(th), height*sin(th))
  c4 = end - width*c(cos(th), sin(th))

  # Assemble brace coordinates
  q = seq(0+th, pi/2+th, length=50)
  t = q[length(q):1]
  part1x = width*cos(t+pi/2) + c1[[1]]
  part1y = height*sin(t+pi/2) + c1[[2]]
  part2x = width*cos(q-pi/2) + c2[[1]]
  part2y = height*sin(q-pi/2) + c2[[2]]
  part3x = width*cos(q+pi) + c3[[1]]
  part3y = height*sin(q+pi) + c3[[2]]
  part4x = width*cos(t) + c4[[1]]
  part4y = height*sin(t) + c4[[2]]
  x = c(part1x, part2x, part3x, part4x)
  y = c(part1y, part2y, part3y, part4y)

  lines(x, y,...)

}

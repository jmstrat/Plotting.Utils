#' Colour Data
#'
#' This function is a copy of \code{\link[plotrix]{color.scale}} but without the override of par('xpd').
#'
#' @export
color.legend.jms <- function (xl, yb, xr, yt, legend, rect.col, cex = 1, align = "lt", gradient = "x", ...)
{
  oldcex <- par("cex")
  par(cex = cex)
  gradient.rect(xl, yb, xr, yt, col = rect.col, nslices = length(rect.col), gradient = gradient)
  if (gradient == "x") {
    xsqueeze <- (xr - xl)/(2 * length(rect.col))
    textx <- seq(xl + xsqueeze, xr - xsqueeze, length.out = length(legend))
    if (match(align, "rb", 0)) {
      texty <- yb - 0.2 * strheight("O")
      textadj <- c(0.5, 1)
    }
    else {
      texty <- yt + 0.2 * strheight("O")
      textadj <- c(0.5, 0)
    }
  }
  else {
    ysqueeze <- (yt - yb)/(2 * length(rect.col))
    texty <- seq(yb + ysqueeze, yt - ysqueeze, length.out = length(legend))
    if (match(align, "rb", 0)) {
      textx <- xr + 0.2 * strwidth("O")
      textadj <- c(0, 0.5)
    }
    else {
      textx <- xl - 0.2 * strwidth("O")
      textadj <- c(1, 0.5)
    }
  }
  text(textx, texty, labels = legend, adj = textadj, ...)
  par(cex = oldcex)
}

#From plotrix
gradient.rect <- function (xleft, ybottom, xright, ytop, reds, greens, blues,
          col = NULL, nslices = 50, gradient = "x", border = par("fg"))
{
  if (is.null(col))
    col <- color.scale.jms(1:nslices,reds, greens, blues)
  else nslices <- length(col)
  nrect <- max(unlist(lapply(list(xleft, ybottom, xright, ytop),
                             length)))
  if (nrect > 1) {
    if (length(xleft) < nrect)
      xleft <- rep(xleft, length.out = nrect)
    if (length(ybottom) < nrect)
      ybottom <- rep(ybottom, length.out = nrect)
    if (length(xright) < nrect)
      xright <- rep(xright, length.out = nrect)
    if (length(ytop) < nrect)
      ytop <- rep(ytop, length.out = nrect)
    for (i in 1:nrect) gradient.rect(xleft[i], ybottom[i],
                                     xright[i], ytop[i], reds, greens, blues, col, nslices,
                                     gradient, border = border)
  }
  else {
    if (gradient == "x") {
      xinc <- (xright - xleft)/nslices
      xlefts <- seq(xleft, xright - xinc, length = nslices)
      xrights <- xlefts + xinc
      rect(xlefts, ybottom, xrights, ytop, col = col, lty = 0)
      rect(xlefts[1], ybottom, xrights[nslices], ytop,
           border = border)
    }
    else {
      yinc <- (ytop - ybottom)/nslices
      ybottoms <- seq(ybottom, ytop - yinc, length = nslices)
      ytops <- ybottoms + yinc
      rect(xleft, ybottoms, xright, ytops, col = col, lty = 0)
      rect(xleft, ybottoms[1], xright, ytops[nslices],
           border = border)
    }
  }
  invisible(col)
}

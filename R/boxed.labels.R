#' Place labels in boxes
#'
#' This function is a workaround for an issue with \code{\link[plotrix]{boxed.labels}}, where adj doesn't seem to function properly.
#'
#' @details
#' The label(s) are displayed on a rectangular background. This may be useful for visibility and is the reason that "transparent" background is not available. With the default textcol=NA, the function tries to work out whether white or black text will be more easily read based on the background color and displays the text accordingly. If the user specifies text colors in the additional arguments, these colors will override the automatic white/black above - see the last example. \cr
# 'Only right angle rotations are allowed in boxed.labels. Important change: xpad and ypad are now the full proportion of the box to text, not half. The user can now call cylindrect or gradient.rect for the background rectangle.
#'
#' @return
#' nil
#'
#' @param x,y x and y position of the centers of the labels. x can be an \code{\link{xy.coords}} list.
#' @param bg The fill color of the rectangles on which the labels are displayed (see Details).
#' @param labels Text strings
#' @param border Whether to draw borders around the rectangles.
#' @param xpad,ypad The proportion of the rectangles to the extent of the text within.
#' @param cex Character expansion. See \code{\link{text}}.
#' @param adj left/right adjustment. If this is set outside the function, the box will not be aligned properly.
#' @param xlog Whether the X axis is a log axis
#' @param ylog Whether the Y axis is a log axis
#' @param ... additional arguments passed to \code{\link{text}}.
#' @export
boxed.labels <- function (x, y = NA, labels, bg = ifelse(match(par("bg"), "transparent",
                                                               0), "white", par("bg")), border = TRUE, xpad = 1.2, ypad = 1.2,
                          srt = 0, cex = 1, adj = 0.5, xlog = FALSE, ylog = FALSE,
                          ...)
{
  oldpars <- par(c("cex", "xpd"))
  par(cex = cex, xpd = TRUE)
  if (is.na(y) && is.list(x)) {
    y <- unlist(x[[2]])
    x <- unlist(x[[1]])
  }
  box.adj <- adj + (xpad - 1) * cex * (0.5 - adj)
  if (srt == 90 || srt == 270) {
    bheights <- strwidth(labels)
    theights <- bheights * (1 - box.adj)
    bheights <- bheights * box.adj
    lwidths <- rwidths <- strheight(labels) * 0.5
  }
  else {
    lwidths <- strwidth(labels)
    rwidths <- lwidths * (1 - box.adj)
    lwidths <- lwidths * box.adj
    bheights <- theights <- strheight(labels) * 0.5
  }
  args <- list(x = x, y = y, labels = labels, srt = srt, adj = adj,
               col = ifelse(colSums(col2rgb(bg) * c(1, 1.4, 0.6)) <
                              350, "white", "black"))
  args <- modifyList(args, list(...))
  if (xlog) {
    xpad <- xpad * 2
    xr <- exp(log(x) - lwidths * xpad)
    xl <- exp(log(x) + lwidths * xpad)
  }
  else {
    xr <- x + rwidths * xpad #JMS - lwidths --> + rwidths
    xl <- x - lwidths * xpad #JMS + lwidths --> - lwidths
  }
  if (ylog) {
    ypad <- ypad * 2
    yb <- exp(log(y) - bheights * ypad)
    yt <- exp(log(y) + theights * ypad)
  }
  else {
    yb <- y - bheights * ypad
    yt <- y + theights * ypad
  }

  rect(xl, yb, xr, yt, col = bg, border = border) # JMS switched xl and xr
  do.call(text, args)
  par(cex = oldpars)
}

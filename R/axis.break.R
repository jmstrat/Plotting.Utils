#' Put a box around a plot, leaving gaps for axis breaks on the x and / or y axes
#'
#' The x axis can be specified as either 1 or 3, the y axis as 2 or 4
#' @param axis Either a number (1-4) or a vector of 2 numbers c(1,2) choosing the axes to break
#' @param breakpos If only one axis is provided, then a single number corresponding to the point at which to break the axis, otherwise a vector of two numbers c(x,y)
#' @param brw If only one axis is provided, then a single number corresponding to the width of the break as a fraction of the total width, otherwise a vector of two numbers c(x,y)
#' @export
axis.break.box <- function (axis = 1, breakpos, brw = 0.02)
{
  figxy <- par("usr")
  xaxl <- par("xlog")
  yaxl <- par("ylog")

  xa=axis %in% c(1,3)
  if(any(xa)) {
    breakposx <- breakpos[xa]
    brwx=brw
    if(length(brw)==2) brwx=brw[xa]
    xw <- (figxy[2] - figxy[1]) * brwx
    if(xaxl)
      breakposx <- log10(breakposx)
  }
  ya=axis %in% c(2,4)
  if(any(ya)) {
    breakposy <- breakpos[ya]
    brwy=brw
    if(length(brw)==2) brwy=brw[ya]
    yw <- (figxy[4] - figxy[3]) * brwy
    if(yaxl)
      breakposy <- log10(breakposy)
  }

  old.xpd <- par("xpd")
  par(xpd = TRUE)

  if (xaxl) {
    figxy[1] <- 10^figxy[1]
    figxy[2] <- 10^figxy[2]
  }
  if (yaxl) {
    figxy[3] <- 10^figxy[3]
    figxy[4] <- 10^figxy[4]
  }
  if (any(xa)) {
    #Fill with white
    rect(breakposx-xw/2, figxy[3], breakposx + xw/2, figxy[4],
         col = 'white', border = 'white')
    if(any(ya)) {
      rect(figxy[1], breakposy-yw/2, figxy[2], breakposy + yw/2,
           col = 'white', border = 'white')
      xbegin <- c(figxy[1],figxy[1],breakposx + xw/2,breakposx + xw/2)
      ybegin <- c(figxy[3], breakposy + yw/2,breakposy + yw/2,figxy[3])
      xend <- c(breakposx-xw/2,breakposx-xw/2, figxy[2], figxy[2])
      yend <- c(breakposy - yw/2, figxy[4], figxy[4],breakposy - yw/2)
      if (xaxl) {
        xbegin <- 10^xbegin
        xend <- 10^xend
      }
      if (yaxl) {
        ybegin <- 10^xbegin
        yend <- 10^xend
      }
    } else {
      xbegin <- c(figxy[1],breakposx + xw/2)
      ybegin <- c(figxy[3], figxy[3])
      xend <- c(breakposx-xw/2, figxy[2])
      yend <- c(figxy[4], figxy[4])
      if (xaxl) {
        xbegin <- 10^xbegin
        xend <- 10^xend
      }
    }
  }
  else {  #Just y axis
    rect(figxy[1], breakposy-yw/2, figxy[2], breakposy + yw/2,
         col = 'white', border = 'white')
    xbegin <- c(figxy[1], figxy[1])
    ybegin <- c(figxy[3], breakposy + yw/2)
    xend <- c(figxy[2], figxy[2])
    yend <- c(breakposy-yw/2, figxy[4])
    if (yaxl) {
      ybegin <- 10^xbegin
      yend <- 10^xend
    }
  }
  par(xpd = TRUE)
  rect(xbegin, ybegin, xend, yend, border = 'black', lty = 1)
  par(xpd = FALSE)
}

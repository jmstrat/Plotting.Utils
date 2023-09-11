#' Put a box around a plot, leaving gaps for axis breaks on the x and / or y axes
#'
#' Based on \code{\link[plotrix]{axis.break}}. The x axis can be specified as either 1 or 3, the y axis as 2 or 4
#' @param axis Either a number (1-4) or a vector of 2 numbers c(1,2) choosing the axes to break
#' @param breakpos If only one axis is provided, then a single number corresponding to the point at which to break the axis, otherwise a vector of two numbers c(x,y)
#' @param brw If only one axis is provided, then a single number corresponding to the width of the break as a fraction of the total width, otherwise a vector of two numbers c(x,y)
#' @export
axis.break.box <- function(axis=1, breakpos, brw=0.02) {
  figxy <- par("usr")
  xaxl <- par("xlog")
  yaxl <- par("ylog")

  xa <- axis %in% c(1, 3)
  if (any(xa)) {
    breakposx <- breakpos[xa]
    brwx <- brw
    if (length(brw) == 2) brwx <- brw[xa]
    xw <- (figxy[2] - figxy[1]) * brwx
    if (xaxl) {
      breakposx <- log10(breakposx)
    }
  }
  ya <- axis %in% c(2, 4)
  if (any(ya)) {
    breakposy <- breakpos[ya]
    brwy <- brw
    if (length(brw) == 2) brwy <- brw[ya]
    yw <- (figxy[4] - figxy[3]) * brwy
    if (yaxl) {
      breakposy <- log10(breakposy)
    }
  }

  old.xpd <- par("xpd")
  par(xpd=TRUE)

  if (xaxl) {
    figxy[1] <- 10^figxy[1]
    figxy[2] <- 10^figxy[2]
  }
  if (yaxl) {
    figxy[3] <- 10^figxy[3]
    figxy[4] <- 10^figxy[4]
  }
  if (any(xa)) {
    # Fill with white
    rect(breakposx - xw / 2, figxy[3], breakposx + xw / 2, figxy[4],
      col="white", border="white"
    )
    if (any(ya)) {
      rect(figxy[1], breakposy - yw / 2, figxy[2], breakposy + yw / 2,
        col="white", border="white"
      )
      xbegin <- c(figxy[1], figxy[1], breakposx + xw / 2, breakposx + xw / 2)
      ybegin <- c(figxy[3], breakposy + yw / 2, breakposy + yw / 2, figxy[3])
      xend <- c(breakposx - xw / 2, breakposx - xw / 2, figxy[2], figxy[2])
      yend <- c(breakposy - yw / 2, figxy[4], figxy[4], breakposy - yw / 2)
      if (xaxl) {
        xbegin <- 10^xbegin
        xend <- 10^xend
      }
      if (yaxl) {
        ybegin <- 10^xbegin
        yend <- 10^xend
      }
    } else {
      xbegin <- c(figxy[1], breakposx + xw / 2)
      ybegin <- c(figxy[3], figxy[3])
      xend <- c(breakposx - xw / 2, figxy[2])
      yend <- c(figxy[4], figxy[4])
      if (xaxl) {
        xbegin <- 10^xbegin
        xend <- 10^xend
      }
    }
  }
  else { # Just y axis
    rect(figxy[1], breakposy - yw / 2, figxy[2], breakposy + yw / 2,
      col="white", border="white"
    )
    xbegin <- c(figxy[1], figxy[1])
    ybegin <- c(figxy[3], breakposy + yw / 2)
    xend <- c(figxy[2], figxy[2])
    yend <- c(breakposy - yw / 2, figxy[4])
    if (yaxl) {
      ybegin <- 10^xbegin
      yend <- 10^xend
    }
  }
  par(xpd=TRUE)
  rect(xbegin, ybegin, xend, yend, border="black", lty=1)
  par(xpd=FALSE)
}

#' Put a box around a plot, leaving gaps for axis breaks on the x and / or y axes
#'
#' Based on \code{\link[plotrix]{axis.break}}.
#' @param axis Either a number (1-4) or a vector of numbers c(1,2,...) choosing the axes to break
#' @param breakpos If only one axis is provided, then a single number corresponding to the point at which to break the axis, otherwise a vector of numbers c(...)
#' @param brw If only one axis is provided, then a single number corresponding to the width of the break as a fraction of the total width, otherwise a vector of two numbers c(...)
#' @param brwx As brw, but specifically in the x direction (do not specify both brw and brwx/y)
#' @param brwy As brw, but specifically in the y direction (do not specify both brw and brwx/y)
#' @export
#' @seealso \code{\link{axis.break.box}}
axis.break.slash <- function(axis=1, breakpos, brw=0.02, brwx=NA, brwy=NA, xpd=TRUE) {
  if (all(is.na(c(brwx, brwy)))) brwx <- brwy <- brw
  if (length(axis) > 1 || length(breakpos) > 1) {
    args <- expand_args(axis=axis, breakpos=breakpos, brwx=brwx, brwy=brwy)
    for (i in 1:length(args$axis)) {
      axis.break.slash(axis=args$axis[[i]], breakpos=args$breakpos[[i]], brwx=args$brwx[[i]], brwy=args$brwy[[i]])
    }
    return(invisible())
  }
  figxy <- par("usr")
  xaxl <- par("xlog")
  yaxl <- par("ylog")
  xw <- (figxy[2] - figxy[1]) * brwx
  yw <- (figxy[4] - figxy[3]) * brwy

  if (xaxl && (axis == 1 || axis == 3)) {
    breakpos <- log10(breakpos)
  }
  if (yaxl && (axis == 2 || axis == 4)) {
    breakpos <- log10(breakpos)
  }
  switch(axis,
    br <- c(
      breakpos - xw / 2,
      figxy[3] - yw / 2,
      breakpos + xw / 2,
      figxy[3] + yw / 2
    ),
    br <- c(
      figxy[1] - xw / 2,
      breakpos - yw / 2,
      figxy[1] + xw / 2,
      breakpos + yw / 2
    ),
    br <- c(
      breakpos - xw / 2,
      figxy[4] - yw / 2,
      breakpos + xw / 2,
      figxy[4] + yw / 2
    ),
    br <- c(
      figxy[2] - xw / 2,
      breakpos - yw / 2,
      figxy[2] + xw / 2,
      breakpos + yw / 2
    ),
    stop("Improper axis specification.")
  )
  old.xpd <- par("xpd")
  par(xpd=xpd)
  if (xaxl) {
    br[c(1, 3)] <- 10^br[c(1, 3)]
  }
  if (yaxl) {
    br[c(2, 4)] <- 10^br[c(2, 4)]
  }

  # Remove bit of axis where break mark will go
  rect(br[1], br[2], br[3], br[4], col="white", border="white")
  if (axis == 1 || axis == 3) {
    xbegin <- c(breakpos - xw, breakpos)
    xend <- c(breakpos, breakpos + xw)
    ybegin <- c(br[2], br[2])
    yend <- c(br[4], br[4])
    if (xaxl) {
      xbegin <- 10^xbegin
      xend <- 10^xend
    }
  }
  else {
    xbegin <- c(br[1], br[1])
    xend <- c(br[3], br[3])
    ybegin <- c(breakpos - yw, breakpos)
    yend <- c(breakpos, breakpos + yw)
    if (yaxl) {
      ybegin <- 10^ybegin
      yend <- 10^yend
    }
  }
  segments(xbegin, ybegin, xend, yend, col="black", lty=1)
  par(xpd=old.xpd)
}

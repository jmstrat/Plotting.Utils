#' Colour Data
#'
#' This function is a workaround for an issue with \code{\link[plotrix]{color.scale}}.
#' Calculating the number of colours must be performed after the check of xrange.
#'
#' @details color.scale calculates a sequence of colors by a linear transformation of the numeric values supplied into the ranges for the three color parameters.If only one number is supplied for a color range, that color remains constant for all values of x. If more than two values are supplied, the x values will be split into equal ranges (one less than the number of colors) and the transformation carried out on each range. Values for a color range must be between 0 and 1 for the RGB or HSV specifications, and between 0 and 360 (cs1) and 0 to 100 (cs2 and cs3) for the HCL specifications.
#'
#' IMPORTANT: If x has fewer values than the number of values in the color parameters, it will usually return incorrect colors. This is usually only a problem when using color.legend with a small number of rectangles in the legend as color.legend calls color.scale to calculate the color rectangles.
#'
#' If extremes is not NA, the ranges will be calculated from its values using col2rgb, even if ranges are also supplied. extremes allows the user to just pass the extreme color values in any format that col2rgb will accept. Note that this forces the color specification to RGB.
#'
#' If the user wants to specify a range of values with xrange, it must at least include the range of x values. This can be useful when there is a notional range like 0-100% that the values do not cover, or when several series of values with different ranges are to be assigned the same color scale.
#'
#' The user may not want the color scheme to be continuous across some critical point, often zero. In this case, color scale can be called separately for the values below and above zero. I may get around to adding an argument to do this in one shot. Until then, see the second example for color2D.matplot and also the diverge.hcl and diverge.hsv functions in the colorspace package.
#'
#' @param x A numeric vector, matrix or data frame
#' @param cs1,cs2,cs3	Colour parameters for scaling x
#' @param alpha	Value for transparency in colours.
#' @param extremes	The colours for the extreme values of x (RGB only).
#' @param na.color	The colour to use for NA values of x.
#' @param xrange	An explicit range to use in the transformation.
#' @param color.spec The color specification to use in the transformation. Anything other than "rgb", "hsv" or "hcl" will almost certainly fail.
#' @export
color.scale.jms <- function (x, cs1 = c(0, 1), cs2 = c(0, 1), cs3 = c(0, 1), alpha = 1, extremes = NA, na.color = NA, xrange = NULL, color.spec = "rgb") {
  if(length(x[!is.na(x)])==0) return(rep_len(na.color,length(x)))
  naxs <- is.na(x)
  if (!is.na(extremes[1])) {
    colmat <- col2rgb(extremes)
    cs1 <- colmat[1, ]/255
    cs2 <- colmat[2, ]/255
    cs3 <- colmat[3, ]/255
    color_spec <- "rgb"
  }
  maxcs1 <- ifelse(color.spec == "hcl", 360, 1)
  maxcs2 <- ifelse(color.spec == "hcl", 100, 1)
  maxcs3 <- ifelse(color.spec == "hcl", 100, 1)
  #ncolors <- length(x)
  if (is.null(xrange)) {
    xrange <- range(x, na.rm = TRUE)
    drop.extremes <- FALSE
  } else {
    if (xrange[1] > min(x, na.rm = TRUE) || xrange[2] < max(x,na.rm = TRUE))
      stop("An explicit range for x must include the range of x values.")
    x <- c(xrange, x)
    drop.extremes = TRUE
  }
  ncolors <- length(x) ##MOVED THIS TO HERE
  ncs1 <- length(cs1)
  if (ncs1 > 1) {
    cs1s <- rep(cs1[ncs1], ncolors)
    xstart <- xrange[1]
    xinc <- diff(xrange)/(ncs1 - 1)
    for (seg in 1:(ncs1 - 1)) {
      segindex <- which((x >= xstart) & (x <= (xstart +xinc)))
      cs1s[segindex] <- rescale.jms(x[segindex], cs1[c(seg,seg + 1)])
      xstart <- xstart + xinc
    }
    if (min(cs1s, na.rm = TRUE) < 0 || max(cs1s, na.rm = TRUE) > maxcs1)
      cs1s <- rescale.jms(cs1s, c(0, maxcs1))
  } else cs1s <- rep(cs1, ncolors)
  ncs2 <- length(cs2)
  if (ncs2 > 1) {
    cs2s <- rep(cs2[ncs2], ncolors)
    xstart <- xrange[1]
    xinc <- diff(xrange)/(ncs2 - 1)
    for (seg in 1:(ncs2 - 1)) {
      segindex <- which((x >= xstart) & (x <= (xstart + xinc)))
      cs2s[segindex] <- rescale.jms(x[segindex], cs2[c(seg, seg + 1)])
      xstart <- xstart + xinc
    }
    if (min(cs2s, na.rm = TRUE) < 0 || max(cs2s, na.rm = TRUE) > maxcs2)
      cs2s <- rescale.jms(cs2s, c(0, maxcs2))
  } else cs2s <- rep(cs2, ncolors)
  ncs3 <- length(cs3)
  if (ncs3 > 1) {
    cs3s <- rep(cs3[ncs3], ncolors)
    xstart <- xrange[1]
    xinc <- diff(xrange)/(ncs3 - 1)
    for (seg in 1:(ncs3 - 1)) {
      segindex <- which((x >= xstart) & (x <= (xstart + xinc)))
      cs3s[segindex] <- rescale.jms(x[segindex], cs3[c(seg,seg + 1)])
      xstart <- xstart + xinc
    }
    if (min(cs3s, na.rm = TRUE) < 0 || max(cs3s, na.rm = TRUE) > maxcs3)
      cs3s <- rescale.jms(cs3s, c(0, maxcs3))
  } else cs3s <- rep(cs3, ncolors)
  if (drop.extremes) {
    cs1s <- cs1s[-(1:2)]
    cs2s <- cs2s[-(1:2)]
    cs3s <- cs3s[-(1:2)]
  }
  xdim <- dim(x)
  colors <- do.call(color.spec, list(cs1s, cs2s, cs3s, alpha = alpha))
  if (!is.null(xdim))
    colors <- matrix(colors, nrow = xdim[1])
  if (length(naxs))
    colors[naxs] <- na.color
  return(colors)
}

#' Rescale Data
#'
#' This function is a workaround for an issue with \code{\link[plotrix]{rescale}}.
#' Where all values for x are the same, we should still make sure they fall within the required range. We thus return the lower limit for all values.
#' @export
rescale.jms <- function (x, newrange)
{
  if (missing(x) | missing(newrange)) {
    usage.string <- paste("Usage: rescale(x,newrange)\n", "\twhere x is a numeric object and newrange is the new min and max\n", sep = "", collapse = "")
    stop(usage.string)
  }
  if (is.numeric(x) && is.numeric(newrange)) {
    xna <- is.na(x)
    if (all(xna))
      return(x)
    if (any(xna))
      xrange <- range(x[!xna])
    else xrange <- range(x)
    if (xrange[1] == xrange[2])
      return(rep_len(newrange[[1]],length(x)))
    mfac <- (newrange[2] - newrange[1])/(xrange[2] - xrange[1])
    return(newrange[1] + (x - xrange[1]) * mfac)
  } else {
    warning("Only numeric objects can be rescaled")
    return(x)
  }
}

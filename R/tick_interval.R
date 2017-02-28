#' Calculates a tick inteval to make pretty plots
#' @param range The axis range
#' @param frac Allow fractional intervals?
#' @export
tick_interval <- function(range,frac=FALSE) {
  if(length(range) != 1) stop("'range' must be of length 1")
  nice=c(1,2,5,10)
  ti=10^floor(log10(range)) * nice[[which(range <= 10^floor(log10(range)) * nice)[[1]]]]/10
  if(!frac) {
    ti=ceiling(ti)
  }
  ti
}

#' Calculates tick locations to make pretty plots
#' @param min The minimum axis limit
#' @param max The maximum axis limit
#' @param div Divide the calculated interval by this number (to generate minor ticks)
#' @param frac Allow fractional intervals?
#' @export
pretty_ticks <- function(min,max,frac=FALSE, div=1) {
  tickInterval<-tick_interval(max-min)/div
  ticksat=seq(signif(min,1)-tickInterval*10,signif(max,1)+tickInterval*10,tickInterval)
}

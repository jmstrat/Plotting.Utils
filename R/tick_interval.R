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
#' @param ensureZero If 0 is within the range, ensure it has a tick mark
#' @export
pretty_ticks <- function(min,max,frac=FALSE, div=1, ensureZero=TRUE) {
  tickInterval<-tick_interval(max-min)/div
  tickStart=signif(min,1)
  tickEnd=signif(max,1)
  #Range is only to 1 sf, so extend it to make sure we cover everything
  ticksat=seq(tickStart-tickInterval*10,tickEnd+tickInterval*10,tickInterval)
  if(ensureZero && 0>=min && 0<=max) {
    #If zero is in the range, we ensure that it has a tick
    if(!0 %in% ticksat) {
      #Zero currently isn't a tick position, find the closest point to zero
      nearest=which.min(abs(ticksat))
      #Get it's value (affects sign)
      nearest=ticksat[[nearest[[1]]]]
      #Subtract this from the ticks
      ticksat=ticksat-nearest
    }
  }
  ticksat
}

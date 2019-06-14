draw_axis <- function(min, max,
                      axisSide,
                      frac, div, flexible,
                      line=line, las=NULL,
                      scale=c(1, 0),
                      # invert=F,
                      lab=NULL, labline=NULL, labcex=cex, centreTitlesToLabels=F,
                      lowerTickLimit=NA, lowerLabelLimit=NA,
                      upperTickLimit=NA, upperLabelLimit=NA,
                      forcedInterval=NA,
                      forcePrint=FALSE,
                      ticklabels=T, ticksOut=T, ...) {
  # Add some axes
  ticksat <- pretty_ticks(min, max, frac, div=1, flexible, forcedInterval)
  if (is.null(ticksat)) {
    # min == max
    return()
  }
  Minorticksat <- pretty_ticks(min, max, frac, div, flexible, forcedInterval)
  Minorticksat <- Minorticksat[!(Minorticksat %in% ticksat)]

  if (!is.na(upperTickLimit)) {
    ticksat <- ticksat[ticksat <= upperTickLimit]
    Minorticksat <- Minorticksat[Minorticksat <= upperTickLimit]
  }
  if (!is.na(lowerTickLimit)) {
    ticksat <- ticksat[ticksat >= lowerTickLimit]
    Minorticksat <- Minorticksat[Minorticksat >= lowerTickLimit]
  }

  tclMult <- if (ticksOut) -1 else 1

  # Add minor ticks
  axis(side=axisSide, tcl=tclMult * .2, at=Minorticksat * scale[[1]] + scale[[2]], labels=NA, ...)
  # Add major ticks
  axis(side=axisSide, tcl=tclMult * .4, at=ticksat * scale[[1]] + scale[[2]], labels=NA, ...)

  if (!is.na(upperLabelLimit)) {
    ticksat <- ticksat[ticksat <= upperLabelLimit]
    Minorticksat <- Minorticksat[Minorticksat <= upperLabelLimit]
  }
  if (!is.na(lowerLabelLimit)) {
    ticksat <- ticksat[ticksat >= lowerLabelLimit]
    Minorticksat <- Minorticksat[Minorticksat >= lowerLabelLimit]
  }
  if (!ticklabels) {
    return()
  }
  # Add labels
  if (forcePrint) {
    ta1 <- ticksat[c(T, F)]
    ta2 <- ticksat[c(F, T)]
    axis(side=axisSide, lwd=0, tcl=-0.5, line=line, at=ta1 * scale[[1]] + scale[[2]], labels=ta1, las=las, ...)
    axis(side=axisSide, lwd=0, tcl=-0.5, line=line, at=ta2 * scale[[1]] + scale[[2]], labels=ta2, las=las, ...)
  } else {
    axis(side=axisSide, lwd=0, tcl=-0.5, line=line, at=ticksat * scale[[1]] + scale[[2]], labels=ticksat, las=las, ...)
  }

  if (!is.null(lab)) {
    if (centreTitlesToLabels) {
      displayedTicks <- Minorticksat
      displayedTicks <- displayedTicks[displayedTicks > min & displayedTicks < max] * scale[[1]] + scale[[2]]
      if (length(displayedTicks) > 0) {
        centre <- (max(displayedTicks) - min(displayedTicks)) / 2 + min(displayedTicks)
        draw_axis_label(side=axisSide, at=centre, lab, line=labline, cex=labcex, ...)
      } else {
        draw_axis_label(side=axisSide, lab, line=labline, cex=labcex, ...)
      }
    } else {
      draw_axis_label(side=axisSide, lab, line=labline, cex=labcex, ...)
    }
  }
}


draw_axis_label <- function(side, label, line, cex, at=NA, ...) {
  mtext(side=side, as.expression(label), line=line, cex=cex, at=at, ...)
}

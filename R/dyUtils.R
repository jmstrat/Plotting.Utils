#' Add a box to a \code{\link[dygraphs]{dygraph}}
#'
#' @export
dyBox <- function(dygraph, lwd=1) {
  dygraphs::dyCallbacks(
    dygraph,
    underlayCallback=htmlwidgets::JS(
      paste0("function(ctx, area, dygraph) {
                 ctx.lineWidth=", lwd * 2, ";
                 ctx.strokeStyle = 'black';
                 ctx.strokeRect(area.x, area.y, area.w, area.h);
              }")
    )
  )
}

#' Set the x limits for a \code{\link[dygraphs]{dygraph}}
#'
#' @export
dyxlim <- function(dygraph, xlim) {
  dygraph$x$attrs$dateWindow <- xlim
  dygraph
}

#' Default axis for a \code{\link[dygraphs]{dygraph}}
#' @inheritParams dygraphs::dyAxis
#' @param ticks Show ticks?
#' @param digits Number of decimal places in legend (only relevant if ticks==TRUE)
#'
#' @export
dyAxis.jms <- function(graph, name, label, valueRange, ticks=TRUE, digits=2) {
  graph <- if (ticks) {
    dygraphs::dyAxis(graph, name,
      label=label, valueRange=valueRange, drawGrid=FALSE, axisLineWidth=1, independentTicks=TRUE,
      valueFormatter=paste0("function(d) {return parseFloat(Math.round(d * 100) / 100).toFixed(", digits, ")}")
    )
  } else {
    dygraphs::dyAxis(graph, name,
      label=label, valueRange=valueRange, drawGrid=FALSE, axisLineWidth=1,
      axisLabelWidth=20, ticker="function(x) {return[]}",
      valueFormatter='function(x) {return("")}'
    )
  }

  if (name == "x") graph <- dyxlim(graph, valueRange)
  graph
}

#' Sets the visibility of a trace
#' @export
dyVisibility <- function(graph, trace=1, visibility=TRUE) {
  vis <- graph$x$attrs$visibility
  if (!length(vis)) vis <- rep_len(TRUE, length(graph$x$data))
  vis[trace] <- visibility
  graph$x$attrs$visibility <- vis
  graph
}

#' Create a blank plot
#' @export
dyBlankPlot <- function() {
  blank_plot <- dygraphs::dygraph(data.frame(x=0.5, y=NA))
  blank_plot <- dygraphs::dyOptions(blank_plot,
    drawXAxis=F, drawYAxis=F, drawGrid=F, pointSize=0,
    plotter="function(p) {return}", strokeWidth=0
  )
  blank_plot <- dygraphs::dyCallbacks(
    blank_plot,
    drawPointCallback="function(p) {return}",
    annotationMouseOverHandler="function(p) {return}"
  )
  blank_plot <- Plotting.Utils::dyxlim(blank_plot, c(0, 1))
  blank_plot <- dygraphs::dyAxis(blank_plot, "y", valueRange=c(0, 1))
  blank_plot <- dygraphs::dyLegend(blank_plot, show="never")
  blank_plot
}

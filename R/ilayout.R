#' Specifying Complex Plot Arrangements for Interactive Plots
#'
#' \code{ilayout} divides the device up into as many rows and columns
#' as there are in matrix \code{mat}, with the column-widths and the
#' row-heights specified in the respective arguments.
#' @details Figure \emph{i} is allocated a region composed from a subset of
#' these rows and columns, based on the rows and columns in which \emph{i}
#' occurs in \code{mat}. \cr
#' \code{ilayout.addPlot} adds a plot to the current layout \cr
#' \code{ilayout.show}(n) plots (part of) the current layout,
#' namely the outlines of the next n figures. \cr
#' \code{ilayout.all} Sets up both the default plot device and the interactive
#' plot device.
#' @inherit graphics::layout details
#' @inheritParams graphics::layout
#' @export
#' @rdname ilayout
ilayout <- function(mat, widths=rep.int(1, ncol(mat)), heights=rep.int(1, nrow(mat))) {
  storage.mode(mat) <- "integer"
  mat <- as.matrix(mat)

  num.figures <- as.integer(max(mat))
  for (i in 1L:num.figures) {
    if (match(i, mat, nomatch=0L) == 0L) {
      stop(gettextf("ilayout matrix must contain at least one reference\nto each of the values {1 ... %d}\n", num.figures), domain=NA)
    }
  }
  gridcss <- c()
  for (i in 1:num.figures) {
    locations <- which(mat == i, arr.ind=TRUE)
    gridcss[[i]] <- sprintf(
      ".plot%s {grid-column: %s ; grid-row: %s ; grid-column-end: %s; grid-row-end: %s;}",
      i,
      locations[1, 2],
      locations[1, 1],
      locations[1, 2] + length(unique(locations[, 2])),
      locations[1, 1] + length(unique(locations[, 1]))
    )
  }
  wrappercss <- sprintf(
    ".plotwrapper {display: grid; height: 98%%; width:100%%; grid-template-columns: %s; grid-template-rows: %s}",
    paste(widths, "fr", sep="", collapse=" "), paste(heights, "fr", sep="", collapse=" ")
  )
  ilayout.options$taglist <- htmltools::browsable(shiny::tagList(shiny::tags$head(shiny::tags$style(
    paste("html, body {height: 100%;}", wrappercss, paste(gridcss, collapse="
"), sep="
")
  )), shiny::div(class="plotwrapper")))
  ilayout.options$nextPlot <- 1
  ilayout.options$num.figures <- num.figures
  invisible(num.figures)
}

#' Add a plot to an interactive layout
#'
#' @param graph The \code{\link[dygraphs]{dygraph}} to add.
#' @export
#' @rdname ilayout
ilayout.addPlot <- function(graph) {
  if (ilayout.options$nextPlot > ilayout.options$num.figures) {
    ilayout.options$nextPlot <- 1
    ilayout.options$taglist[[2]]$children <- list()
  }
  graph$height <- "98%"
  graph$width <- "100%"
  newdiv <- shiny::div(class=paste0("plot", ilayout.options$nextPlot), style="width: 100% ; height: 100%", graph)
  ilayout.options$taglist[[2]]$children[[ilayout.options$nextPlot]] <- newdiv
  ilayout.options$nextPlot <- ilayout.options$nextPlot + 1
  ilayout.options$taglist
}

#' @param trace Trace # of plot
#' @param plot \code{ilayout} plot number
#' @export
#' @rdname ilayout
ilayout.hideTraceFromPlot <- function(trace, plot=1, visibility=FALSE) {
  ilayout.options$taglist[[2]]$children[[plot]]$children[[1]] <- dyVisibility(ilayout.options$taglist[[2]]$children[[plot]]$children[[1]], trace, visibility)
  ilayout.options$taglist
}

#' @export
#' @rdname ilayout
ilayout.show <- function(n) {
  blank_plot <- dyBlankPlot()
  blank_plot <- dyBox(blank_plot)
  blank_plot$x$css <- "
  .plotnumber {
  overflow: visible !important;
  width: initial !important;
  border: none !important;
  font-size: 200% !important;
  height: 100% !important;
  width: 100% !important;
  position: relative !important;
  display: flex;
  align-items: center !important;
  background-color:rgba(0, 0, 0, 0) !important;
  }"
  for (i in 1:n) {
    combined_plot <- ilayout.addPlot(dygraphs::dyAnnotation(blank_plot, 0.5, i, cssClass="plotnumber"))
  }
  combined_plot
}

#' @export
#' @rdname ilayout
layout.all <- function(...) {
  layout(...)
  ilayout(...)
}

# Prepare layout environment
ilayout.options <- new.env()
# Set default layout
ilayout(1)

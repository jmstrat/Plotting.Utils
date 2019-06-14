#' Layout plots in grid form
#'
#' Sets up a grid layout for plots. Currently up to 9 plots are supported.
#' @param n number of plots
#' @param show Show the resultant layout using \code{\link{layout.show}}?
#' @param device Which device to setup - one of 'base', 'interactive', or 'both'
#' @export
#' @rdname grid.layout
grid.layout <- function(n, show=FALSE, device="both") {
  if (device == "both") {
    layoutFunction <- layout.all
    showFunction <- layout.show
  } else if (device == "interactive") {
    layoutFunction <- ilayout
    showFunction <- ilayout.show
  } else {
    if (!device == "base") warning("Unknown device, assuming base")
    layoutFunction <- graphics::layout
    showFunction <- graphics::layout.show
  }
  if (n == 1) {
    layoutFunction(1)
  } else if (n == 2) {
    layoutFunction(matrix(c(1, 2), byrow=T))
  } else if (n == 3) {
    layoutFunction(
      matrix(
        c(
          1, 1, 2, 2,
          0, 3, 3, 0
        ),
        2, 4,
        byrow=T
      )
    )
  } else if (n == 4) {
    layoutFunction(
      matrix(
        c(
          1, 2,
          3, 4
        ),
        2, 2,
        byrow=T
      )
    )
  } else if (n == 5) {
    layoutFunction(
      matrix(
        c(
          1, 1, 2, 2, 3, 3,
          0, 4, 4, 5, 5, 0
        ),
        2, 6,
        byrow=T
      )
    )
  } else if (n == 6) {
    layoutFunction(
      matrix(
        c(
          1, 2, 3,
          4, 5, 6
        ),
        2, 3,
        byrow=T
      )
    )
  } else if (n == 7) {
    layoutFunction(
      matrix(
        c(
          1, 2, 3,
          4, 5, 6,
          0, 7, 0
        ),
        3, 3,
        byrow=T
      )
    )
  } else if (n == 8) {
    layoutFunction(
      matrix(
        c(
          1, 1, 2, 2, 3, 3,
          4, 4, 5, 5, 6, 6,
          0, 7, 7, 8, 8, 0
        ),
        3, 6,
        byrow=T
      )
    )
  } else if (n == 9) {
    layoutFunction(
      matrix(
        c(
          1, 2, 3,
          4, 5, 6,
          7, 8, 9
        ),
        3, 3,
        byrow=T
      )
    )
  } else {
    stop("Unsupported number of plots passed to grid.layout")
  }
  if (show) showFunction(n)
}

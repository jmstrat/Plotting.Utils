#' Read a data file
#'
#' This function reads one data file
#' @param path The path to the file / directory
#' @param func The function to read the file
#' @return A jms.data.object containing the data
#' @examples
#' load.file('/path/to/file.ext', load_function)
#' @keywords internal
load.file <- function(path,func,...) {
  if(!file.exists(path)) stop(path,' not found')
  func(path,...)
}

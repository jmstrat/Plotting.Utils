#' Export a data object
#'
#' @param x The data object
#' @param path The path to a csv file in which to save the data
#' @rdname export
#' @export
export <- function(x,path) UseMethod("export")
#' @export
#' @export
export.default <- function(x,path) {
  stop("Unable to export data for this class")
}
#' @rdname export
#' @export
export.jms.data.object <- function(x,path) {
  atts=attributes(x)
  attNames=names(atts)
  attNames=attNames[!attNames %in% c("names","row.names","class",'y_type','x_type','x_column','y_column')]
  f <- file(path, "w")
  for(att in attNames) {
    writeLines(paste(att, atts[[att]],sep=','),f)
  }
  write.table(x,f,sep=",",row.names =FALSE)
  close(f)
}

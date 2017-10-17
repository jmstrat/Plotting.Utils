#' Read every file in a directory
#'
#' This function reads every .ext file in a directory
#' @param dir The path to the directory
#' @param func The loader function
#' @param ext The file extension to look for
#' @param pattern The pattern to look for
#' @param sort If TRUE then use a "natural" numerical sort (see \code{\link[Plotting.Utils]{list.files.sorted}}), else use the default alphabetical sort
#' @details One of ext or pattern must be specified
#' @return A list containing the data
#' @examples
#' load_dir_as_list('/path/to/directory',<function>,ext='ext')
#' @keywords internal
#' @seealso \code{\link{load_directory}} \code{\link{list.files.sorted}}
load.directory <- function(dir,func, ext=NULL,pattern=NULL, sort=FALSE) {
  if(is.null(pattern)) {
    if(is.null(ext)) stop('One of ext or pattern must be specified')
    pattern=paste0('.*\\.',ext,'$')
  }
  sorter = if(sort) list.files.sorted else list.files
  file_list <- sorter(path=dir,full.names=TRUE,pattern=pattern)
  data=lapply(file_list,function(x) func(x,...))
}

#' list.files with sorting
#'
#' @param ... Parameters passed to \code{\link{list.files}}
#' @section Sorting:
#' Natural sorting will take a file name of the sort <string><number><string>.<extension> and sort based on the number in a natural manner that does not depend on the number of digits in that number.
#' i.e 10 would come after 9 in a natural sort, but after 1 using the default sorting method
#' @export
list.files.sorted <- function(...) {
  files=list.files(...)
  files[order(as.numeric(sub("^[^0-9]*([0-9]*).*", "\\1", basename(files))))]
}

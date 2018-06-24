#' Find the nearest rows in a dataframe
#'
#' @param data The data frame
#' @param ... the data to match e.g. column=c(1,2,3,4)
#' @param as.list Return a list even if there is only one column to search
#' @return list of columns, for each column name: a data.frame of the nearest rows
#'         if as.list is FALSE and there is only one column the data.frame is returned
#'         without an encapsulating list
#' @export
#' @examples
#' nearest(rock, area=c(6775, 4895, 9819, 4990))
#'
#' nearest(rock, area=c(6775, 4895, 9819, 4990), shape=0.31)
nearest <- function(data, ..., as.list = F) {
  values = list(...)
  output = list()
  for(k in names(values)) {
    raw = as.data.frame(data[k])
    indexes=sapply(as.list(values[[k]]), function(x) which(abs(raw-x)==min(abs(raw-x))))
    output[[k]] = data[indexes,]
  }
  if(length(values) == 1) {
    output[[1]]
  } else {
    output
  }
}


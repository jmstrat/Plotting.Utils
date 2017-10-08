#' Extend jms.data.object for a custom class
#'
#' Creates the following functions:
#' \itemize{
#' \item <name>.data.object
#' \item as.<name>.data.object
#' \item is.<name>.data.object
#' \item [.<name>.data.object
#' \item read.table.<name>
#' }
#'
#' @param name The name of the new data type
#' @param xlab The x-axis label for the new data type
#' @param ylab The y-axis label for the new data type
#' @param envir The environment within which to define the new functions
#' @export
create_data_type <- function(name,xlab,ylab,envir=parent.frame()) {
  dataObjName=paste0(name,'.data.object')
  asDataObjFun <- function(x) {
    x=as.jms.data.object(x)
    class(x) <- c(dataObjName,class(x))
    attr(x,'y_type')<-ylab
    attr(x,'x_type')<-xlab
    x
  }
  assign(paste0('as.',dataObjName),asDataObjFun,envir=envir)

  isDataObjFun <- function(x) {
    return(inherits(x,dataObjName))
  }
  assign(paste0('is.',dataObjName),isDataObjFun,envir=envir)

  dataObjFun <- function(...) {
    return(asDataObjFun(jms.data.object(...)))
  }
  assign(dataObjName,dataObjFun,envir=envir)

  readTableFun <- function(...) {
    return(asDataObjFun(read.table.jms(...)))
  }
  assign(paste0('read.table.',name),readTableFun,envir=envir)

  subsetFun <- function(x,...) {
    r <- NextMethod("[")
    if(inherits(r,'jms.data.object')) return(asDataObjFun(r))
    return(r)
  }
  assign(paste0('[.',dataObjName),subsetFun,envir=envir)
}

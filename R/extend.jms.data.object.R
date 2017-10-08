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
#' @details
#' Intended usage is within a package's .onLoad function.
#' If any function already exists within the environment at load time,
#' the function <function>.super will be created instead. This allows
#' the functions created here to be extended (simply call the super
#' method within the new method)
#'
#' @param name The name of the new data type
#' @param xlab The x-axis label for the new data type
#' @param ylab The y-axis label for the new data type
#' @param inherits Name of any additional data types from which this should inherit
#' @param envir The environment within which to define the new functions
#' @export
create_data_type <- function(name,xlab,ylab,inherits=c(),envir=parent.frame()) {
  dataObjName=paste0(name,'.data.object')
  inheritsNames=if(length(inherits)) paste0(inherits,'.data.object') else NULL
  asDataObjFun <- function(x) {
    x=as.jms.data.object(x)
    class(x) <- c(dataObjName,inheritsNames,class(x))
    attr(x,'y_type')<-ylab
    attr(x,'x_type')<-xlab
    x
  }
  asName=paste0('as.',dataObjName)
  if(exists(asName,envir=envir)) asName=paste0(asName,'.super')
  assign(asName,asDataObjFun,envir=envir)

  isDataObjFun <- function(x) {
    return(inherits(x,dataObjName))
  }
  isName=paste0('is.',dataObjName)
  if(exists(isName,envir=envir)) isName=paste0(isName,'.super')
  assign(isName,isDataObjFun,envir=envir)

  dataObjFun <- function(...) {
    return(asDataObjFun(jms.data.object(...)))
  }
  if(exists(dataObjName,envir=envir)) dataObjName=paste0(dataObjName,'.super')
  assign(dataObjName,dataObjFun,envir=envir)

  readTableFun <- function(...) {
    return(asDataObjFun(read.table.jms(...)))
  }
  readName=paste0('read.table.',name)
  if(exists(readName,envir=envir)) readName=paste0(readName,'.super')
  assign(readName,readTableFun,envir=envir)

  subsetFun <- function(x,...) {
    r <- NextMethod("[")
    if(inherits(r,'jms.data.object')) return(asDataObjFun(r))
    return(r)
  }
  subsetName=paste0('[.',dataObjName)
  if(exists(subsetName,envir=envir)) subsetName=paste0(subsetName,'.super')
  assign(subsetName,subsetFun,envir=envir)
}

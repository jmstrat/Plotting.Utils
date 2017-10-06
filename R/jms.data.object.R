#' Retain attributes upon subsetting
#' @export
`[.jms.data.object` <- function (x,i=T,j=T, ...) {
  r <- NextMethod("[")
  special_attrs=c('class', 'comment', 'dim', 'dimnames', 'names', 'row.names', 'tsp')
  oldAtts=attributes(x)
  oldAtts=oldAtts[!names(oldAtts)%in%special_attrs]
  nOldAtts=names(oldAtts)
  attributes(r)[nOldAtts]<-oldAtts
  if('x_column'%in%nOldAtts&&!oldAtts[['x_column']]%in%j) {
    attributes(r)[['x_column']]=NA
  }
  if('y_column'%in%nOldAtts) {
    if(is.numeric(j))
      new_y=which(j%in%oldAtts[['y_column']])
    else if(is.logical(j))
      new_y=which((1:ncol(x))[j]%in%oldAtts$y_column)
    else
      new_y=numeric()
    attributes(r)[['y_column']]=new_y
    if(is.na(attributes(r)[['x_column']])) new_y=new_y[new_y>oldAtts[['x_column']]]-1
    if(length(new_y)==1&&inherits(r,'data.frame')) class(r) <- c("jms.data.object","data.frame")
  }
  return (r)
}

#' Read a table as a JMS data object
#'
#' @param ... parameters are passed to \code{\link{read.table}}
#' @return A JMS data object containing the data
#' @export
read.table.jms <- function(...) {
  df=read.table(...)
  attr(df, "class") <- c("jms.data.object","data.frame")
  attr(df,'file_type')<-NULL
  attr(df,'data_type')<-NULL
  attr(df,'y_type')<-'Unknown'
  attr(df,'x_type')<-'Unknown'
  attr(df,'x_column')<-1
  attr(df,'y_column')<-2
  df
}

#' Check if an object is a pdf.data.object
#'
#' @param x The object to be tested
#' @return TRUE / FALSE
#' @keywords internal
is.jms.data.object <- function(x) {
  return(inherits(x,"jms.data.object"))
}

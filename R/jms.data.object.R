#' Read a table as a JMS data object
#'
#' @param ... parameters are passed to \code{\link{read.table}}
#' @return A JMS data object containing the data
#' @export
read.table.jms <- function(...) {
  df=read.table(...)
  as.jms.data.object(df)
}

#' Make a new JMS data object
#'
#' @param ... parameters are passed to \code{\link{data.frame}}
#' @return A JMS data object
#' @export
jms.data.object <- function(...) {
  return(as.jms.data.object(data.frame(...)))
}

#' Check if an object is a jms.data.object
#'
#' @param x The object to be tested
#' @return TRUE / FALSE
#' @export
is.jms.data.object <- function(x) {
  return(inherits(x,"jms.data.object"))
}

#' Convert an object into a JMS data object
#'
#' @param x The object to be converted
#' @return The converted object
#' @export
as.jms.data.object <- function(x) UseMethod("as.jms.data.object")

#' @export
as.jms.data.object.default <- function(x) {
  stop("Unable to convert this class")
}

#' @export
as.jms.data.object.data.frame <- function(x) {
  attr(x, "class") <- c("jms.data.object", "data.frame")
  atts=names(attributes(x))
  if(!'file_type'%in%atts) attr(x,'file_type')<-NULL
  if(!'data_type'%in%atts) attr(x,'data_type')<-NULL
  if(!'y_type'%in%atts) attr(x,'y_type')<-'Unknown'
  if(!'x_type'%in%atts) attr(x,'x_type')<-'Unknown'
  if(!'x_column'%in%atts) attr(x,'x_column')<-1
  if(!'y_column'%in%atts) attr(x,'y_column')<-2
  return(x)
}

#' @export
as.jms.data.object.matrix <- function(x) {
  as.jms.data.object(as.data.frame(x))
}

#' @export
print.jms.data.object <- function (x, ..., digits = NULL, quote = FALSE, right = TRUE, row.names = TRUE) {
  n <- length(row.names(x))
  if (length(x) == 0L) {
    classes=class(x)
    classes_=c()
    for(i in classes) {
      if(i=='jms.data.object') break
      classes_=append(classes_,i)
    }
    extendedby = if(length(classes_)) gsub("\\.", " ", paste0('(extended by ',paste0(classes_, collapse=', '),') ')) else ''
    cat(sprintf(ngettext(n, "JMS data object %swith 0 columns and %d row",
                         "JMS data object %swith 0 columns and %d rows"), extendedby,n), "\n",
        sep = "")
  }
  else if (n == 0L) {
    print.default(names(x), quote = FALSE)
    cat(gettext("<0 rows> (or 0-length row.names)\n"))
  }
  else {
    m <- as.matrix(format.data.frame(x, digits = digits,
                                     na.encode = FALSE))
    if (!isTRUE(row.names))
      dimnames(m)[[1L]] <- if (identical(row.names, FALSE))
        rep.int("", n)
    else row.names
    print(m, ..., quote = quote, right = right)
  }
  invisible(x)
}

#' Retain attributes upon subsetting
#' @export
`[.jms.data.object` <- function (x,i=T,j=T, ...) {
  r <- NextMethod("[")
  # Restore attributes:
  # Don't restore these
  special_attrs=c('class', 'comment', 'dim', 'dimnames', 'names', 'row.names', 'tsp')
  oldAtts=attributes(x)
  oldAtts=oldAtts[!names(oldAtts)%in%special_attrs]
  nOldAtts=names(oldAtts)
  attributes(r)[nOldAtts]<-oldAtts
  # Update class attributes:
  # Sometimes j is not set but exists() ?!?!?!
  j<-tryCatch({get('j')},error=function(e) TRUE)
  # Check we still have the x column after subsetting and whether its number has changed
  if('x_column'%in%nOldAtts) {
    if(is.numeric(j))
      attributes(r)[['x_column']]=which(sort(j)%in%oldAtts[['x_column']])
    else if(is.logical(j))
      attributes(r)[['x_column']]=which((1:ncol(x))[j]%in%oldAtts$x_column)
    else
      attributes(r)[['x_column']]=integer()
  }
  # Check which y columns we still have and get their new numbers
  if('y_column'%in%nOldAtts) {
    if(is.numeric(j))
      attributes(r)[['y_column']]=which(sort(j)%in%oldAtts[['y_column']])
    else if(is.logical(j))
      attributes(r)[['y_column']]=which((1:ncol(x))[j]%in%oldAtts$y_column)
    else
      attributes(r)[['y_column']]=integer()
    # Restore the class if the dataset is still 2D
    if(inherits(r,'data.frame')) class(r) <- c("jms.data.object","data.frame")
  }
  return (r)
}

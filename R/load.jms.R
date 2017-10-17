#' Read data files
#'
#' This function reads one or more data files in a directory
#' @param path The path to the file / directory
#' @param func The function to read the file
#' @param ... Additional parameters are passed to \code{\link[Plotting.Utils]{load.directory}} or \code{\link[Plotting.Utils]{load.file}}
#' @return A jms.data.object containing the data
#' @examples
#' load.jms('/path/to/directory', load_function, ext='ext')
#' load.jms('/path/to/file.ext', load_function)
#' load.jms(c('/path/to/file.ext','/path/to/file2.ext'))
#' @export
load.jms <- function(path,func,...) {
  dat=c()
  for(p in path) {
    if(dir.exists(p)) dat=c(dat,load.directory(p,func,...))
    else if(file.exists(p)) dat=c(dat,list(load.file(p,func,...)))
    else stop(p, ' not found')
  }
  combine.data.objects(as.list(dat))
}

#' Combine a list of data objects
#'
#' This function combines jms.data.objects
#' @return A jms.data.object containing the data
#' @examples
#' combine.data.objects(objects)
#' @export
combine.data.objects <- function(objects) {
  if(is.jms.data.object(objects)) return(objects)
  if(!inherits(objects,'list')) stop('objects is not a list')
  if(length(objects)==1) return(objects[[1]])
  #Now we have to read each object as a subsequent column for a data frame
  columns=c()
  len_f=length(objects)
  x_1=NA
  for(f in 1:len_f) {
    x_column=xcol(objects[[f]])
    x=objects[[f]][,x_column]
    if(f==1) x_1=x
    if(!all(x==x_1)) stop("X axis is not the same across all files -- aborting")
    y_column=ycol(objects[[f]])
    columns[[f]]=objects[[f]][,y_column]
  }
  df=jms.data.object(x_1,columns)
  #Add default attributes
  xlab(df)<-xlab(objects[[1]])
  ylab(df)<-ylab(objects[[1]])
  xcol(df)<-1
  ycol(df)<-c(2:ncol(df))
  attr(df,'file_type')<-attr(objects[[1]],'file_type')
  attr(df,'data_type')<-attr(objects[[1]],'data_type')
  #Rename columns
  names(df) <- c(xlab(df),paste0(ylab(df),'_',c(1:len_f)))
  class(df)<-class(objects[[1]])
  #return data
  return(df)
}

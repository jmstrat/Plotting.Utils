#' Calculate Difference
#'
#' This function calculates the difference between two data objects
#' @param data1 The 1st data object
#' @param data2 The 2nd data object
#' @return
#' A data object representing the difference
#' @examples
#' calculate_difference(data1,data2)
#' @export
calculate_difference <- function(data1,data2) {
  if(!(is.jms.data.object(data1) && is.jms.data.object(data2))) stop("Data must be a jms.data.object")
  x_col=attr(data1,'x_column')
  y_cols=attr(data1,'y_column')
  if(is.null(x_col)) {
    warning('Data type unknown, assuming 1st column for x axis')
    x_col=1
  }
  if(is.null(y_cols)) {
    warning('Data type unknown, assuming last column for y axis')
    y_cols=ncol(data)
  }
  x_col2=attr(data2,'x_column')
  y_cols2=attr(data2,'y_column')
  if(is.null(x_col2)) {
    warning('Data type unknown, assuming 1st column for x axis')
    x_col2=1
  }
  if(is.null(y_cols2)) {
    warning('Data type unknown, assuming last column for y axis')
    y_cols2=ncol(data)
  }

  if(length(x_col)!=1||length(x_col2)!=1) stop('Cannot process this data type (unknown x)')
  if(length(y_cols)!=length(y_cols2)) {
    warning("Data has differing number of y columns, only using 1st")
    y_cols=y_cols[[1]]
    y_cols2=y_cols2[[1]]
  }

  calc_diff <- function(x1,y1,x2,y2) {
    dat2_approxfun=approxfun(x2,y2)
    xdiff=x1[x1<=max(x2)&x1>=min(x2)]
    ydiff=y1[x1<=max(x2)&x1>=min(x2)]
    y1=rep_len(NA,length(y1))
    y1[x1<=max(x2)&x1>=min(x2)]<-ydiff-dat2_approxfun(xdiff)
    y1
  }
  diff=data1
  diff[,2:ncol(diff)]<-NULL
  attr(diff,'x_column')<-1
  if(length(y_cols)>1) {
    for(i in 1:length(y_cols)) {
      diff[paste0("diff.",i)]<-calc_diff(data1[,x_col],data1[,y_cols[[i]]],data2[,x_col],data2[,y_cols2[[i]]])
    }
    attr(diff,'y_column')<-2:(length(y_cols)+1)
  } else if(length(y_cols)==1) {
    diff["diff"]<-calc_diff(data1[,x_col],data1[,y_cols],data2[,x_col],data2[,y_cols2])
    attr(diff,'y_column')<-2
  } else stop("Unknown y data")
  diff
}

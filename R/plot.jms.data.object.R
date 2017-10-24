#' Plot a data object
#'
#' This function plots a data object
#' @param x The object to plot
#' @examples
#' plot(data)
#' @export
plot.jms.data.object <- function(x,offset=1/sqrt(ncol(x)-1),xlim=NULL,ylim=NULL,xlab=xlab_(x),ylab=ylab_(x),xaxt=par('xaxt'),yaxt=par('yaxt'),.extend_y=c(0,0),...) {
  x_col=xcol(x)
  y_cols=ycol(x)
  x_data=x[,x_col]
  y_df=x[,y_cols]

  if(any(is.null(xlim))) xlim=range(x_data[is.finite(x_data)])
  y_max=max(y_df)
  y_range=range(y_df,offset=offset)

  if(any(is.null(ylim))) ylim=extendrange(r=(y_range+.extend_y),0.05)
  y_axis=if(yaxt=='n') NA else 2
  x_axis=if(xaxt=='n') NA else 1

  args=list(...)
  plot_args=args[names(args) %in% names(c(formals(axis),formals(pretty_axes),formals(pretty_plot)))]
  plot_args=plot_args[!names(plot_args) %in% c('col','lwd')]
  lines_args=args[names(args) %in% names(c(
    formals(getAnywhere(findMethod(lines,x))$objs[[1]]),
    formals(plot.xy))
    )]

  plot_args=append(list(xlim=xlim,ylim=ylim,x_axis=x_axis,y_axis=y_axis,xlab=xlab,ylab=ylab),plot_args)
  lines_args=append(list(x=x),lines_args)
  do.call(pretty_plot,plot_args)
  do.call(lines,lines_args)
}

#' Draw lines for a data object
#'
#' This function plots a data object
#' @param x The object to plot
#' @examples
#' lines(data)
#' @export
lines.jms.data.object <- function(x,offset=1/sqrt(ncol(x)-1),col=par('col'),...) {
  x_col=xcol(x)
  y_cols=ycol(x)

  x_data=x[,x_col]
  y_df=x[,y_cols]

  if(!is.data.frame(y_df)) {
    x=data.frame(x=x_data,y=y_df)
    offset<-NULL
    return(NextMethod())
  }

  y_max=max(y_df,na.rm=T)
  col_all=expand_args(1:ncol(y_df),col)[[2]]
  offset_=offset
  offset=NULL
  for(i in 1:ncol(y_df)) {
    y=y_df[,i]+offset_*(i-1)*y_max
    x=data.frame(x=x_data,y=y)
    col=col_all[[i]]
    NextMethod()
  }
}

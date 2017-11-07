#' Make an interactive plot for a data object
#'
#' @rdname iPlot
#' @export
iPlot <- function(...) UseMethod("iPlot")
#' @rdname iPlot
#' @export
iPlot.default <- function(...) {
  stop("Unable to make an interactive plot for this class")
}
#' @inheritParams graphics::plot.window
#' @inheritParams graphics::plot.default
#' @inheritParams graphics::par
#' @inheritParams graphics::plot.xy
#' @rdname iPlot
#' @export
iPlot.jms.data.object <- function(...,offset=1/sqrt(length(ycol(data))-1),xlim=NULL,ylim=NULL,axes=c(1,2),xlab=xlab_(data),ylab=ylab_(data),col=par('col'),lwd=1,pch=NA,labels=NULL) {
  data<-combine.data.objects(unname(list(...)),interpolate=TRUE) #Need to interpolate to avoid gaps...
  dots <- substitute(list(...))[-1]
  argNames=c(sapply(dots, deparse))
  data<-data[,c(xcol(data),ycol(data))]
  if(length(argNames)==length(ycol(data))) names(data)[ycol(data)]<-argNames

  if(length(ycol(data))>1) data=data+offset*seq(0,length(ycol(data))-1,1)*range(data)[[2]]
  if(any(is.null(xlim))) xlim=range(data[,xcol(data)][is.finite(data[,xcol(data)])])
  if(any(is.null(ylim))) ylim=extendrange(r=range(data),0.04)

  graph<-dygraphs::dygraph(data)
  col_all=expand_args(2:(ncol(data)),col)[[2]]
  lwd_all=expand_args(2:(ncol(data)),lwd)[[2]]
  pch_all=expand_args(2:(ncol(data)),pch)[[2]]
  for(i in 1:(ncol(data)-1)) {
    col=col_all[[i]]
    drawPoints <- if(!is.na(pch_all[[i]])) TRUE else NULL
    pointSize <- if(!is.na(pch_all[[i]])) 1 else 0
    strokeWidth <- lwd_all[[i]]
    label<- if(!is.null(labels)) labels[[i]] else NULL
    graph<-dygraphs::dySeries(graph,label=label,color=rgb(t(col2rgb(col)/255)),axis='y',drawPoints=drawPoints,pointSize=pointSize,strokeWidth=strokeWidth)
  }
  graph<-dyAxis.jms(graph,'x',label=xlab,valueRange=xlim,ticks=1%in%axes)
  graph<-dyAxis.jms(graph,'y',label=ylab,valueRange=ylim,ticks=2%in%axes)
  graph <- dyBox(graph)
  direction <- if(any(c(2,4)%in%axes)) "both" else "vertical"
  graph <- dygraphs::dyCrosshair(graph,direction = direction)
  graph <- dygraphs::dyLegend(graph,show = "always", hideOnMouseOut = TRUE)

  #Fix for mysterious warning...
  set.seed(1)
  #Return the graph (will plot at top level)
  graph
}

plot.per.state <- function(x, state.names, measure, dig=1,
                           col.pal=colorRampPalette(c("blue","white","red"))(3),
                           bks=NULL,percent=FALSE,paren=TRUE,text.names=TRUE,
                           text.cex=.7,num=NULL, ...) {
  mar <- par()$mar
  cp.len <- length(col.pal)
  cols <- rep(0,length(x))

  qq <- quantile(x,seq(0,1,len=cp.len+1))
  if (!is.null(bks)) qq <- bks

  if (length(qq) != cp.len+1) {
    print("bks has to have length have col.pal + 1")
    return
  }

  for (i in 1:cp.len) {
    ind <- x >= qq[i]
    cols[ind] <- col.pal[i]
  } 

  map('state', state.names, mar=rep(0,4), border='grey90', col=cols, fill=TRUE, ...)
}

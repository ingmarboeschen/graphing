#' stacked area chart
#'
#' stacked area chart
#' @param x 2 dimensional frequency table
#' @param col color of areas 
#' @param xpos.text x position of text 
#' @param reorder Logical. If TRUE reorders by colsums of x
#' @export

stacked.area.chart<-function(x,
      col=(dim(x)[2]+1):2,
      xpos.text=dim(x)[1]+.1,
      reorder=T,
      xlab="",
      ylab="",
      cex=.9,
      xlim=c(1,length(rownames(x))),
      ylim=c(0,max(mat))){
# reorder by absolutfrequency
if(reorder==T) x<-x[,order(colSums(x))]
# transpose x
trans<-t(x)
# create cumulated matrix
mat<-rbind(rep(0,dim(x)[1]),trans)
for(i in 1:dim(x)[2]) mat[i+1,]<-mat[i+1,]+mat[i,]
# draw
plot(1,type="n",xlim=xlim,ylim=ylim,axes=F,xlab=xlab,ylab=ylab)
axis(1,min(xlim):max(xlim),rownames(x)[min(xlim):max(xlim)])
axis(2,seq(0,max(ylim),len=5),round(seq(0,max(ylim),len=5),4))
# draw polygons
for(i in 1:dim(x)[2]) polygon(c(1:dim(mat)[2],dim(mat)[2]:1),c(mat[i,],rev(mat[i+1,])),col=col[i])
# draw points
#for(i in 1:dim(x)[2]) points(1:dim(mat)[2],(mat[i+1,]-mat[i,])/2+mat[i,],col=i,type="l",lwd=3)
# add text
ypos<-NULL
for(i in 1:dim(x)[2]) ypos[i]<-mat[i,dim(mat)[2]]+(mat[i+1,dim(mat)[2]]-mat[i,dim(mat)[2]])/2
par(xpd=T)
text(rep(xpos.text,dim(x)[2]),ypos,colnames(x),pos=4,cex=cex)
par(xpd=F)
}

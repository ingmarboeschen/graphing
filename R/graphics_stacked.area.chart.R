#' stacked area chart
#'
#' stacked area chart for 2 dimensional frequency tables
#' @param x 2 dimensional frequency table
#' @param type="area" type of grapf ("area" or "line")
#' @param col color of areas 
#' @param labels=TRUE Logicical. If TRUE labels of areas are drawn on the right side of the plot
#' @param xpos.text x position of text 
#' @param reorder=FALSE Logical. If TRUE reorders by frequency of y
#' @param xlab="" x axis label
#' @param ylab="" y axis label
#' @param cex=.9 character expansation of area labels
#' @param cex.axis=1 character expansation of axis labels
#' @param las=1 las for y axis labels 
#' @param ylim=c(0,max(mat) limits of x axis
#' @export
#' @example
#' x<-sample(2010:2020,1000,T,.2+(1:11)*.5)
#' y<-sample(paste("category",LETTERS[1:10]),1000,T)
#' par(mar=c(4,4,2,5))
#' stacked.area.chart(table(x,y),main="stacked.area.chart()",ylab="h(x)",xlab="year",reorder=T)
#' stacked.area.chart(prop.table(table(x,y),m=1),main="stacked.area.chart()",ylab="f(x)",xlab="year",reorder=T)

stacked.area.chart<-function(x,
      type="area",
      col=(dim(x)[2]+1):2,
      xpos.text=dim(x)[1]+.1,
      reorder=FALSE,
      labels=TRUE,
      main="",
      xlab="",
      ylab="",
      cex=.9,
      cex.axis=1,
      las=1,
      ylim=c(0,max(mat))){
# get xlimits
xlim<-c(1,length(rownames(x)))
# reorder by absolutfrequency in last column
if(reorder==T) x<-x[,order((x[xlim[2],]))]
# transpose x
trans<-t(x)
# create cumulated matrix
mat<-rbind(rep(0,dim(x)[1]),trans)
for(i in 1:dim(x)[2]) mat[i+1,]<-mat[i+1,]+mat[i,]
# draw
plot(1,type="n",xlim=xlim,ylim=ylim,axes=F,xlab=xlab,ylab=ylab,main=main)
axis(1,min(xlim):max(xlim),rownames(x)[min(xlim):max(xlim)],cex=cex.axis)
axis(2,seq(0,max(ylim),len=5),round(seq(0,max(ylim),len=5),4),cex=cex.axis,las=las)
# draw polygons
if(type=="area") for(i in 1:dim(x)[2]) polygon(c(1:dim(mat)[2],dim(mat)[2]:1),c(mat[i,],rev(mat[i+1,])),col=col[i])
# draw lines
if(type=="line") for(i in 1:dim(x)[2]) points(1:dim(mat)[2],(mat[i+1,]-mat[i,])/2+mat[i,],col=i,type="l",lwd=3)
# add text
ypos<-NULL
for(i in 1:dim(x)[2]) ypos[i]<-mat[i,dim(mat)[2]]+(mat[i+1,dim(mat)[2]]-mat[i,dim(mat)[2]])/2
# draw labels on right of grapf
if(labels==TRUE){
 par(xpd=T)
 text(rep(xpos.text,dim(x)[2]),ypos,colnames(x),pos=4,cex=cex)
 par(xpd=F)
}
}

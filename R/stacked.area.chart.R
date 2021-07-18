#' stacked area chart
#'
#' stacked area chart for 2 dimensional frequency tables
#' @param x 2 dimensional frequency table
#' @param type type of grapf ("area" or "line")
#' @param col color of areas 
#' @param labels=TRUE Logicical. If TRUE labels of areas are drawn on the right side of the plot
#' @param xpos.text x position of text 
#' @param reorder=FALSE Logical. If TRUE reorders by frequency of y
#' @param xlab x axis label
#' @param ylab y axis label
#' @param cex character expansation of area labels
#' @param cex.axis character expansation of axis labels
#' @param las las for y axis labels 
#' @param ylim limits of x axis
#' @export
#' @examples
#' x<-sample(2010:2020,100000,T,.2+(1:11)*.5)
#' y<-sample(paste("category",LETTERS[1:10]),100000,T)
#' par(mar=c(4,4,2,15))
#' stacked.area.chart(table(x,y),main="stacked.area.chart()",ylab="h(x)",xlab="year",reorder=T)
#' stacked.area.chart(prop.table(table(x,y),m=1),main="stacked.area.chart()",ylab="f(x)",xlab="year",reorder=T)

stacked.area.chart<-function(x,
      type="area",
      col=NULL,
      xpos.text=dim(x)[1]+.1,
      reorder=FALSE,
      labels=TRUE,
      addFreq=TRUE,
      main="",
      xlab="",
      ylab="",
      cex=.9,
      cex.axis=1,
      las=1,
      ylim=NULL){
# prepare color
   if(is.null(col)){
      col <- RColorBrewer::brewer.pal(12, "Paired")
      col<-col[10:1]
      col<-rep(col,length.out=ncol(x))
   }
   
# get xlimits
xlim<-c(1,length(rownames(x)))
# reorder by absolutfrequency in last column
if(reorder==T&ncol(x)!=1) x<-x[,order((x[xlim[2],]))]
# transpose x
trans<-t(x)

# create cumulated matrix
#if(is.null(nrow(x)) x<-matrix(x,nrow=length(x))
   mat<-rbind(rep(0,nrow(x)),trans)
   for(i in 1:ncol(x)) mat[i+1,]<-mat[i+1,]+mat[i,]

# get ylimts if ylim==NULL
if(is.null(ylim)){
   if(sum(mat!=round(mat))==0) ylim<-ceiling(c(0,max(mat))/10)*10
   if(sum(mat!=round(mat))>0) ylim<-c(0,1)
   }

# draw
plot(1,type="n",xlim=xlim,ylim=ylim,axes=F,xlab=xlab,ylab=ylab,main=main)
axis(1,min(xlim):max(xlim),rownames(x)[min(xlim):max(xlim)],cex=cex.axis)
# y axis limits and breaks
if(sum(mat!=round(mat))==0) axis(2,round(seq(0,max(ylim),len=5)),format(round(seq(0,max(ylim),len=5)),big.mark=","),cex=cex.axis,las=las)
if(sum(mat!=round(mat))>0) axis(2,seq(0,max(ylim),len=5),format(round(seq(0,max(ylim),len=5),3),big.mark=","),cex=cex.axis,las=las)
# draw polygons
if(type=="area") for(i in 1:ncol(x)) polygon(c(1:dim(mat)[2],dim(mat)[2]:1),c(mat[i,],rev(mat[i+1,])),col=col[i])
# draw lines
if(type=="line") for(i in 1:ncol(x)) points(1:dim(mat)[2],(mat[i+1,]-mat[i,])/2+mat[i,],col=i,type="l",lwd=3)
# add text
ypos<-NULL
for(i in 1:ncol(x)) ypos[i]<-mat[i,dim(mat)[2]]+(mat[i+1,dim(mat)[2]]-mat[i,dim(mat)[2]])/2
# draw labels on right of grapf
if(labels==TRUE){
 par(xpd=T)
   if(sum((x)>0&(x)<1)==0) text(rep(xpos.text,ncol(x)),ypos,paste0(colnames(x),": ",format(x[nrow(x),],big.mark=",",trim=TRUE)),pos=4,cex=cex)
   if(sum((x)>0&(x)<1)>0) text(rep(xpos.text,ncol(x)),ypos,paste0(colnames(x),": ",round(x[nrow(x),],3)*100,"%"),pos=4,cex=cex)
par(xpd=F)
}
}


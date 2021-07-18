#' prettybarplot
#'
#' draws a barplot of a one or two dimensional frequency table with frequencies on top of bars and pretty background
#' @param x a one or two dimensional frequency table
#' @param col color of bars
#' @param col color of bars
#' @param ylab y axis label
#' @param xlab x axis label
#' @param main title
#' @param names name of bars/bar groups (in 2 dimensional table)
#' @param ylim y-axis limits
#' @param box draw box around plot
#' @param space space between bars
#' @param border border color of bars
#' @param text draw text on bars
#' @param cex text size
#' @param coltext color of text on bars
#' @param pre pre text for absolute frequencies
#' @param post post text for relative frequencies
#' @param digit digits to draw percentual frequencies with
#' @param bg draw background
#' @param bg.increase top-down increase/decrease in background color 
#' @param bg.from background starting color
#' @param bg.to background end color
#' @param legend Logical. If TRUE draws legend
#' @param axes draw axes
#' @param cex.axis size of bar labels
#' @param cex.names size of axis labels
#' @param las rotation of y limts (1: horizontal, 3: vertical)
#' @export
#' @examples
#' x<-sample(paste("group",1:5),20000,T)
#' y<-sample(c("car","bike","bus"),20000,T)
#' # one dimensional barplot
#' prettybarplot(cumsum(table(x)))
#' prettybarplot(prop.table(table(x)))
#' # two dimensional barplot
#' prettybarplot(table(y,x))
#' prettybarplot(prop.table(table(y,x),m=2))

prettybarplot<-function(
x, # a one or two dimensional frequency table
# genreal options
  col="standard", # color of bars
  ylab=NULL, # y axis label
  xlab=NULL, # y axis label
  main="", # title
  names="", # name if bars/bar groups (in 2 dimensional table)
  ylim=c(0,max(x)+max(x)/7),
  box=FALSE, # draw box around plot
# bar and text on bar options
  space=.2, # space between bars
  border="black", # border color of bars
  text=TRUE, # draw text on bars
  cex=1, # text size
  coltext=1, # color of text on bars
  pre="n = ", # pre text for absolute frequencies
  post="%", # post text for relative frequencies
  digit=1, # digits to draw percentual frequencies with
  legend=TRUE,
# background options
  bg=TRUE, # draw background
  bg.increase=TRUE, # top-down increase/decrease in background color 
  bg.from="grey", # background starting color
  bg.to="white", # background end color
# axes options
  axes = TRUE, # draw axes
  cex.axis = par("cex.axis"), # size of bar labels
  cex.names = par("cex.axis"), # size of axis labels
  las=1
){
# preperation
if(names[1]==""){
 if(is.table(x)) names<-names(x)
 if(is.vector(x)) names<-x}

# color for bars
if(col[1]=="standard"){
 if(length(dim(x))==2) col<-paste("grey",round(seq(20,80,length=dim(x)[1])),sep="")
 if(length(dim(x))<=1) col<-rep("lightblue",length=length(x))}

# 1 dimmensional
if(length(dim(x))<=1){
 # y lab
 if(sum(x)>1){
 if(is.null(ylab)) ylab<-"absolute frequency"}
 if(sum(x)==1){
 if(is.null(ylab)) ylab<-"relative frequency"}
 # draw empty barplot
 barplot(x,ylim=ylim,col=col,main=main,ylab=ylab,xlab=xlab,space=space, 
 cex.names = cex.names,cex.axis = cex.axis,axes = FALSE,names=rep("",length(x)))
 # back ground
 if(bg.increase==F) palette <- colorRampPalette(colors=c(bg.from,bg.to))
 if(bg.increase==T) palette <- colorRampPalette(colors=c(bg.to,bg.from))
 cols <- palette(1000)
 if(bg==T) abline(h=seq(ylim[2],0,length=1000),col=cols)
 abline(h=0,lwd=2)
 # add barplot
 barplot(x,ylim=ylim,col=col,main="",xlab="",ylab="",space=space, border=border, 
 cex.names = cex.names,cex.axis = cex.axis,axes = FALSE, add=T,names=names)
 if(axes==TRUE) axis(2,axTicks(2),labels=format(axTicks(2), big.mark=','),las=las)  
 
 # text above bars
 if(text==T){
   par(xpd=TRUE)
  for(i in 1:length((x))){
  if(text==T&sum(x>0&x<1)==0) text((-.5+space+i+((i-1)*space)),(x[i]),paste(pre,format(round(x[i]),big.mark=","),sep=""),pos=3,cex=cex,col=coltext)
  if(text==T&sum(x>0&x<1)>0) text((-.5+space+i+((i-1)*space)),(x[i]),paste(round(x[i]*100,digit),post,sep=""),pos=3,cex=cex,col=coltext)
    par(xpd=FALSE)
  }}
}


# 2 dimensional
if(length(dim(x))==2){
 # y axis label
 if(sum(x)>1&sum(c(rowSums(x)>dim(x)[1],colSums(x)>dim(x)[2]))>1){
  if(ylab=="") ylab<-"absolute frequencies"}
 if(sum(x)==1|sum(c(rowSums(x)==1,colSums(x)==1))==dim(x)[1]|sum(c(rowSums(x)==1,colSums(x)==1))==dim(x)[2]){
   if(ylab=="") ylab<-"relative frequencies"}
 # draw empty barplot
 barplot(x,beside=T,main=main,ylab=ylab,xlab=xlab,ylim=c(0,max(x)+max(x)/7),col=col, 
 cex.names = cex.names,cex.axis = cex.axis,axes = F,names=rep("",dim(x)[2]))
 if(axes==TRUE) axis(2,axTicks(2),labels=format(axTicks(2), big.mark=','),las=las)  
 # back ground
 if(bg.increase==F) palette <- colorRampPalette(colors=c(bg.from,bg.to))
 if(bg.increase==T) palette <- colorRampPalette(colors=c(bg.to,bg.from))
 cols <- palette(1000)
 if(bg==T) abline(h=seq(ylim[2],0,length=1000),col=cols)
 abline(h=0,lwd=2)
 # add barplot
 barplot(x,beside=T,main="",ylab="",xlab="",ylim=c(0,max(x)+max(x)/7),col=col,add=T, border=border,
 cex.names = cex.names,cex.axis = cex.axis,axes = FALSE, names=names)
if(legend==T) legend("top",rownames(x),pch=15,col=col,horiz=T,bty="n")
 # text above bars
 if(text==T){
  temp<-length(x[,1])
  for(j in 1:length(x[1,])){
  for(i in 1:length(x[,1])){
  if(sum(x)<=max(dim(x))){text(0.5+i+(j-1)+temp*(j-1),x[,j][i]+max(x)/30,paste(round(x[,j]*100,digit),"%")[i])}
  if(sum(x)>max(dim(x))){text(0.5+i+(j-1)+temp*(j-1),x[,j][i]+max(x)/30,paste("n =",format(x[,j],big.mark=","))[i])}
  }}
 }
}

if(box==TRUE) box()
} # End

## Example:
# x<-sample(paste("group",1:5),2000,T)
# y<-sample(c("car","bike","bus"),2000,T)
## 1 dimensional table
# prettybarplot(prop.table(table(x)),bg.inc=F,main="prettybarplot()")
## 2 dimensional conditioned table
# prettybarplot(prop.table(table(y,x),m=2),main="prettybarplot()")

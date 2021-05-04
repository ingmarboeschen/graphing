#' multi.line
#'
#' Draw multiple regression or lowess lines
#' @param x a categorial variable (independend variable)
#' @param x independend metric variable
#' @param y dependend metric variable
#' @param group grouping variable
#' @param main title
#' @param axes draw axes
#' @param xlab x axis label
#' @param ylab y axis label
#' @param xlim x axis limits
#' @param ylim y axis limits
#' @param points draw points
#' @param col point color
#' @param pch point character
#' @param cex point size
#' @param lines draw lines
#' @param lty line type
#' @param lwd line width
#' @param type draw regression line/s or lowess line ("line", "lowess", "both")
#' @param locator legend location
#' @param horiz align horizontally
#' @param names group names
#' @export
#' @examples
#' data(ChickWeight)
#' attach(ChickWeight)
#' Diet<-paste("Diet",ChickWeight$Diet)
#' multi.line(x=jitter(Time,2),y=weight,group=Diet,points=T,main="multi.line()",xlab="time",ylab="weight")
#' multi.line(x=jitter(Time,2),y=weight,group=Diet,points=F,main="multi.line() without points with lowess line",xlab="time",ylab="weight",type="lowess")

multi.line<-function(
x, # independend metric variable
y, # dependend metric variable
group, # grouping variable
main="", # title
# axis options
axes=TRUE, # draw axes
xlab="", # x axis label
ylab="", # y axis label
xlim=range(x,na.rm=T), # x axis limits
ylim=range(y,na.rm=T)+c(0,(max(y,na.rm=T)-min(y,na.rm=T))/10),  # y axis limits
# point options
points=TRUE, # draw points
col=rainbow(length(levels(factor(group)))), # point color
pch=1:length(levels(factor(group))), # point character
cex=1, # point size
# line options
lines=TRUE, # draw lines
lty=1:length(levels(factor(group))), # line type
lwd=2,
type="line", # draw regression line/s or lowess line ("line", "lowess", "both")
# legend options
locator="top", # legend location
horiz=TRUE, # align horizontally
names=paste(levels(group)," ")
){
# factorise group
group<-factor(group)
# create vector for coloured points and point characters
colPoints<-as.character(factor(group,levels(group),col))
pchPoints<-as.numeric(factor(group,levels(group),pch))
# count cases with missings
miss <- sum(rowSums(is.na(data.frame(x,y,group)))>0)
# delete missings
d<-data.frame(x,y,group,colPoints,pchPoints)
d<-na.omit(d)
# randomize order
d<-d[sample(1:length(d$x),length(d$x)),]
x<-d$x;y<-d$y;group<-d$group;colPoints<-d$colPoints;pchPoints<-d$pchPoints


plot(y~x,type="n",ylim=ylim,main=main,xlab=xlab,ylab=ylab,xlim=xlim,axes=axes)
if(points==TRUE) points(y~x,col=colPoints,pch=pchPoints,cex=cex)
for(i in 1:length(levels(group))){
  #if(points==TRUE) points(y[group==levels(group)[i]]~x[group==levels(group)[i]],col=col[i],pch=pch[i],cex=cex)
if(type=="line"|type=="both")   abline(lm(y[group==levels(group)[i]]~x[group==levels(group)[i]]),lty=lty[i],col=col[i],lwd=lwd)
if(type=="lowess"|type=="both") points(lowess(y[group==levels(group)[i]]~x[group==levels(group)[i]]),type="l",lty=lty[i],col=col[i],lwd=lwd)
}
if(points==TRUE) legend(locator,names,pch=pch,lty=lty,horiz=horiz,bty="n",col=col)
if(points==FALSE) legend(locator,names,lty=lty,horiz=horiz,bty="n",col=col)
if(axes==TRUE) box(lwd=1.5)

# Model output
if(type!="lowess") print(summary(lm(y~x*group)))
# Missing message
if(miss==1) print(paste(miss,"case was ignored due to a case with missing value/s"))
if(miss>1) print(paste(miss,"cases were ignored due to cases with missing values"))

}


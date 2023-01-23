#' Profile line (of multiple groups) in multiple items
#'
#' draws a profile line of means or medians in multiple items and groups
#' @param matrix a data.frame or matrix containing the likert variables to draw
#' @param group grouping variable for profile lines (optional)
#' @param type type of line: one out of "Median", "Mean", "1. Quartile", "3. Quartile"
#' @param groupnames names of groups
#' @param main title
#' @param col color of group lines
#' @param pch point character
#' @param lty point character
#' @param lwd  line width of profile line
#' @param grid draw grid
#' @param space_left space to y axis of first bar
#' @param xlab x axis label
#' @param labels labels displayed on x axes (must be of length 
#' @param extreme only draw poles of labels (senseful for likert items)
#' @param cex.axis.x cex of x axis
#' @param cex.axis.y cex of y axis
#' @param axis.top draw top axis
#' @param axis.bottom draw bottom axis
#' @param low_label lowest level
#' @param high_label highes level
#' @param padj vertical adjust of low/high levels
#' @param bottom.adj mar bottom adjust
#' @param legend draw legend
#' @param N_legend draw legend of N observations per group
#' @param cex.legend character expansation of legend
#' @param title legends title
#' @export
#' @examples
#' matrix<-data.frame(matrix(sample(-3:3,1000,T),ncol=20))
#' names(matrix)<-paste("Item",1:20); g<-paste("Group",rep(1:3,length=200))
#' for(i in 1:dim(matrix)[2]){matrix[,i]<-factor(matrix[,i],-3:3,c("fully disagree","2","3","4","5","6","fully agree"))}
#' profileline(matrix,g,main="",N_legend=T,pch="",type="Mean",ex=T,low="low",high="high",xlab="agreement")

profileline <- function (
matrix, # a data.frame or matrix containing the likert variables to draw
group=1, # grouping variable for profile lines (optional)
type="Median", # one out of "Median", "Mean", "1. Quartile", "3. Quartile"
groupnames=levels(factor(group)),
# general options
main="", # title
col=2:(length(unique(group))+1), # color of group lines
pch=1:length(levels(factor(group))), # point character
lty=1:length(levels(factor(group))), # point character
lwd = 2 , # line width of profile line
grid=TRUE, #, draw grid
space_left=max(nchar(names(matrix)))/3+2.5, # space for item names
# axes options
xlab="",  # x axis label
labels="", # labels displayed on x axes (must be of length 
extreme=FALSE, # only draw poles of labels (senseful for likert items)
cex.axis.x=1, # cex of x axis
cex.axis.y=1, # cex of y axis
axis.top=TRUE, # draw top axis
axis.bottom=TRUE, #dra bottom axis
low_label="", # lowest level
high_label="", # highes level
padj=3, # vertical adjust of low/high levels
bottom.adj=0, # mar bottom adjust
# legend options
legend=TRUE, # draw legend
N_legend=TRUE, # draw legend of N observations per group
cex.legend=1, # character expansation of legend
title=TRUE # legends title
){


ifelse(lty=="",lty2 <- "",lty2 <- 0)
# preperation
if(is.factor(matrix[,1])) xlim <- c(1,length(levels(matrix[,1])))
if(!is.factor(matrix[,1])) xlim <- range((matrix),na.rm=T)
# axis preperation
if((xlim[2]-xlim[1])<10) at<-seq(xlim[1],xlim[2])
if((xlim[2]-xlim[1])>=10) at<-seq(xlim[1],xlim[2],length=6)
# set labels 
if(sum(labels=="")==1){
if(is.factor(matrix[,1])==FALSE) labels<-min(matrix,na.rm=T):max(matrix,na.rm=T)
if(is.factor(matrix[,1])) labels<-levels(matrix[,1])
if((xlim[2]-xlim[1])>=10) labels<-labels[at-(min(at)-1)]
if(extreme==T) labels<-c(labels[1],rep("",times=length(unique(labels))-2),labels[length((labels))])
}
# set color/point-/line type vectors
if(length(col)!=length(unique(group))) col<-rep(col,length=length(unique(group)))
if(length(pch)!=length(unique(group))) pch<-rep(pch,length=length(unique(group)))
if(length(lty)!=length(unique(group))) lty<-rep(lty,length=length(unique(group)))
# convert matrix to data.frame and gruop to factor
if(!is.data.frame(matrix)) matrix<-data.frame(matrix)
if(!is.factor(group)) group<-factor(group)
# numerising 
if(is.factor(matrix[,1])){
for(i in 1:length(levels(matrix[,1]))){
matrix[,i]<-as.numeric(matrix[,i])}}
# legend title
if(title==TRUE) title<-paste(type," of                  ",sep="")
if(title==FALSE) title<-""
# legend line/type
if(sum(pch!=""&length(unique(pch))!=1)>0) legtype<-"point"
if(sum(pch==""|length(unique(pch))==1)>0) legtype<-"line"

# set graphic device layout
ifelse(legtype=="line",radj<-1.75,radj<-0)
ifelse(main=="",top <- 3.5,top <- 6)
ifelse(sum(low_label==""&high_label==""&xlab=="")>0,bottom <- 3,bottom <- 5)
if(legend==TRUE) par(mar = c(bottom+bottom.adj, space_left, top, 6+max(nchar(as.character(group)),na.rm=TRUE)/4+radj)+.1)
if(legend==FALSE|length(group)==1) par(mar = c(bottom+bottom.adj, space_left, top, 2)+.1)

# draw empty plot
plot(0, type = "n", xlim = xlim, ylab = "", xlab=xlab,ylim = c(dim(matrix)[2],1)+c(0.01,-0.01), axes=F, main="")
mtext(main,pad=-3.75,cex=1.5,font=2)
# draw axes (y, x top, x bottom)
axis(2, las=T, 1:dim(matrix)[2], names(matrix),cex.axis=cex.axis.y)
if(axis.top==TRUE) axis(3, las=T, at = at, labels=labels,cex.axis=cex.axis.x)
if(axis.bottom==TRUE) axis(1, las=T, at = at, labels=labels,cex.axis=cex.axis.x)
# draw low/high labels
if(sum(low_label!=""|high_label!="")>0) axis(1, las=T, at = xlim, labels=c(low_label,high_label),cex.axis=cex.axis.x,tick=F,padj=padj)
# draw grid
if(grid==TRUE) grid(ny= NA,lwd = 2)

# jitter points
if(length(unique(group)) == 1) help<- 0 
if(length(unique(group)) > 1)  help<- seq(-.105,.105,length=length(unique(group)))*length(unique(group))/2

# draw points 
result <- NULL
resultM<-matrix(NA,ncol=length(unique(group)),nrow=dim(matrix)[2])
for(j in 1:length(unique(group))){ 
for(i in 1:dim(matrix)[2]) {
temp<-as.numeric(matrix[,i])[group==unique(group)[j]]
if (type == "Median") result[i] <- median(temp,na.rm=T)
if (type == "1. Quartile") result[i] <- quantile(temp,.25,na.rm=T,type=4)
if (type == "3. Quartile") result[i] <- quantile(temp,.75,,na.rm=T,type=4)
if (type == "Mean") result[i] <- mean(temp,na.rm=T)}
resultM[,j]<-result
}
for(i in 1:dim(resultM)[2]) {
if(lty2!="") points(resultM[,i], 1:dim(matrix)[2]+help[i],type="l", col=col[i], lty=lty[i], lwd=lwd)
points(resultM[,i], 1:dim(matrix)[2]+help[i],pch=pch[i], col=col[i])
}

# draw legend
if(length(unique(group))>1){
 if(legend==TRUE&legtype=="point"){
  legend(at[length(at)]+.5+.5*(at[length(at)]-10)/10,.75-.05*(dim(matrix)[2]-8),groupnames, col=col, pch=pch, 
  cex=cex.legend,title=title,bty="n",title.adj=3,xpd=TRUE,xjust=0)
# draw N observation per group
 if(N_legend==TRUE){
  legend(at[length(at)]+.5+.5*(at[length(at)]-10)/10,dim(matrix)[2]/2,paste("N =",table(group)), col=col, pch=pch , 
  cex=cex.legend,title="Cases per group   ",bty="n",title.adj=3,xpd=TRUE,xjust=0)
 }
 }
 if(legend==TRUE&legtype=="line"){
  legend(at[length(at)]+.5+.5*(at[length(at)]-10)/10,.75-.05*(dim(matrix)[2]-8),groupnames, col=col, lty=lty, 
  cex=cex.legend,title=title,bty="n",title.adj=3,xpd=TRUE,xjust=0)
# draw N observation per group
 if(N_legend==TRUE){
  legend(at[length(at)]+.5+.5*(at[length(at)]-10)/10,dim(matrix)[2]/2,paste("N =",table(group)), col=col, lty=lty , 
  cex=cex.legend,title="Cases per group   ",bty="n",title.adj=3,xpd=TRUE,xjust=0)
 }
  }
 }
}




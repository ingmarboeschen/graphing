#' A barplot for 3 dimensional contingency tables
#'
#' draws the conditioned freuqencies of a categorial variable on two further categorial variables
#' @param x main categorical variable on x-axis
#' @param z categorical sub variable on x axis
#' @param y dependend categorical variable
#' @param main title
#' @param ylab y axis label
#' @param xlab x axis label
#' @param col colors of bar segments
#' @param space space between bars within groups
#' @param factor_space factor of space between groups
#' @param left_space left space of bars
#' @param box draw boxes behind goups of x
#' @param boxcol color of boxes behind groups
#' @param legend draw legend
#' @param locator location of legend: "topright", "right" "bottomright"


#' @examples
#' x <- sample(paste("time",1:4),240,T)
#' z <- rep(c("A","B"),each=120)
#' y <- paste("answer",sample(1:7,240,T))
#' bp3d(x,z,y,main="bp3d()",xlab="treatment group")

### 
bp3d <- function(
	x, # main categorical variable on x-axis
	z, # categorical sub variable on x axis
	y, # dependend categorical variable (properties)
	main="", # title
	ylab="frequencies", # y axis label
	xlab="", # x axis label
	col=heat.colors(length(levels(y))+2)[1:length(levels(y))],
	space=.1, # space between bars within groups
	factor_space=3, # factor of space between groups
	left_space=0, # left space of bars
	box=T, # draw boxes behind goups of x
	boxcol=c("white","grey"), # color of boxes behind groups
	legend=TRUE, # draw legend
	locator="topright" # location of legend: "topright", "right" "bottomright"
#	locator=c(xmax,1)
){
# factorising
if(!is.factor(x)) x <- factor(x)
if(!is.factor(z)) z <- factor(z)
if(!is.factor(y)) y <- factor(y,levels=levels(factor(y))[length(levels(factor(y))):1])

# table for barplot
btable <- NULL
for(i in 1:length(levels(x))){
btable<-cbind(btable,prop.table(table(y,z,x),m=c(2,3))[,,i])
}
# x axis max
xmax <- (length(levels(z))-1)*(length(levels(x)))*space+(length(levels(x))-1)*space*factor_space+(length(levels(x)))*(length(levels(z)))
if(legend==TRUE) xmax<-xmax+xmax/15+max(nchar(levels(y)))/60*xmax
# spacing of barplot
spacing<-rep(c(rep(space,length(levels(z))-1),space*factor_space),length(levels(x)))
spacing<-c(left_space,spacing[1:(length(spacing)-1)])
# empty barplot
barplot(btable,col="white",space=spacing,axes=F,border="white",names=rep("",length(levels(z))*length(levels(x))),ylim=c(0,1.075),xlim=c(0,xmax)) 
# boxes
if(box==T){
max <- left_space+(length(levels(z))-1)*(length(levels(x)))*space+(length(levels(x))-1)*space*factor_space+(length(levels(x)))*(length(levels(z)))
xr <- left_space+max+space*factor_space/2
xl <- left_space-space*factor_space/2
xseq<-seq(xl,xr,length=length(levels(x))+1)

par(xpd=T)
for(i in 1:(length(levels(x)))){
rect(xseq[i],-0.015,xseq[i+1],1.015,col=rep(boxcol,length(levels(x)))[i],border=rep(boxcol,length(levels(x)))[i])}
par(xpd=F)
}
# add barbplot
barplot(btable,col=col,space=spacing,main=main,xlab=xlab,ylab=ylab,ylim=c(0,1.075),add=T)
# x-axis
max <- (length(levels(z))-1)*(length(levels(x)))*space+(length(levels(x))-1)*space*factor_space+(length(levels(x)))*(length(levels(z)))
start <- ((length(levels(z))-2)*space+length(levels(z))/2)
end <- max-start
axis(3,seq(start,end,length=length(levels(x))),levels(x), lwd=0,padj=2.5)
# legend
if(legend==TRUE&locator=="topright") legend(max+space*factor_space/2,1.02,levels(y)[length(col):1],col=col[length(col):1],bty="n",pch=16)
if(legend==TRUE&locator!="topright") legend(locator,1.02,levels(y)[length(col):1],col=col[length(col):1],bty="n",pch=16)
# Missing message
miss <- sum(rowSums(is.na(data.frame(x,z,y)))>0)
if(miss==1) print(paste(miss,"case was ignored due to missing value/s"))
if(miss>1) print(paste(miss,"cases were ignored due to missing values"))
}


## Example
# x <- sample(paste("time",1:4),240,T)
# z <- rep(c("A","B"),each=120)
# y <- paste("answer",sample(1:7,240,T))

# bp3d(x,z,y,main="bp3d()",xlab="treatment group",legend=T)
 
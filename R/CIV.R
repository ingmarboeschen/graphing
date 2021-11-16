#' Confidence Intervall Violin Plot
#'
#' draws observations in metric variable for multiple groups, the group means, the 1-alpha confidence intervalls of the means and a density estimator (violin) of the distribution for every group.
#' @param x a categorial variable (independend variable)
#' @param y a numeric variable (dependend variable)
#' @param alpha error probability of confidence intervall
#' @param ylim y axis limits
#' @param ylab y axis label
#' @param xlab x axis label
#' @param main title
#' @param pch point character
#' @param col mean confidence intervall color
#' @param colpoints point color
#' @param cex point size
#' @param names names of groups on x axis
#' @param jit jitter intensity
#' @export
#' @examples
#' x<-sample(paste("group",1:4),100,T)
#' y<-10+as.numeric(factor(x))*2+rnorm(length(x))
#' CIV(x,y,main="Confidence Intervall Violin Plot")


CIV<-function(
x, # a categorial variable (independend variable)
y, # a numeric variable (dependend variable)
alpha=.05, # error probability of confidence intervall
ylim=range(y)+c(-(range(y,na.rm=T)[2]-range(y,na.rm=T)[1])/5,(range(y,na.rm=T)[2]-range(y,na.rm=T)[1])/5), #y axis limits
ylab="", # y axis label
xlab=paste("|---| ",100*(1-alpha),"% - confidence intervall",sep=""), # x axis label
main="", #title
pch=1+as.numeric(factor(x)), # point character
col="blue", # ci color
colpoints="grey", # point color
cex=.7, # point size
names=levels(factor(x)), # names of groups on x axis
jit=.2) # jitter intensity
{
if(length(x)!=length(y)) stop("x and y length differs")
if(length(x)==length(y)){
# missing count and deletion
xnNA<-sum(is.na(x));ynNA<-sum(is.na(y))
ncases<-sum(is.na(x)|is.na(y))
d<-na.omit(data.frame(x,y))
x<-d[,1];y<-d[,2]
# set x as factor
x<-as.factor(x)
# draw points
boxplot(y~x,border=F,ylim=ylim,xlab=xlab,main=main,ylab=ylab,names=names,col="white")
points(y~jitter(as.numeric(x),jit),cex=cex,pch=pch,col=colpoints)
# draw means
points(1:length(levels(x)),tapply(y,x,mean,na.rm=T),col=col,pch=16)
# confidence intervals (with t-distribution)
for(i in 1:length(unique(x))){
ci_u<-tapply(y,x,mean,na.rm=T)[i]+qt(alpha/2,length(x)-1)*tapply(y,x,sd)[i]/sqrt(tapply(y,x,length)[i])
ci_o<-tapply(y,x,mean,na.rm=T)[i]+qt(1-alpha/2,length(x)-1)*tapply(y,x,sd)[i]/sqrt(tapply(y,x,length)[i])
# draw ci
segments(i,ci_u,i,ci_o,lwd=2,col=col)
points(i,ci_u,pch="-",cex=3,col=col)
points(i,ci_o,pch="-",cex=3,col=col)
}
for(i in 1:length(unique(x))){
densy<-density(y[x==levels(x)[i]])
points(i+densy$y/2,densy$x,type="l")
points(i-densy$y/2,densy$x,type="l")
}
box()
# missing report
if((xnNA+ynNA)>0){
print(paste("You have",xnNA,ifelse(xnNA==1, "missing case","missing cases"),"in x"))
print(paste("You have",ynNA,ifelse(ynNA==1, "missing case","missing cases"),"in y"))
print(paste(ncases,ifelse(ncases==1, "case was ignored", "cases were ignored"),"due to those missings"))
}
}
}

## Example
# x<-sample(paste("group",1:4),100,T)
# y<-10+as.numeric(factor(x))*2+rnorm(length(x))

# CIV(x,y,main="Confidence Intervall Violin Plot")

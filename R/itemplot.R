#' itemplot
#'
#' Function to draw distribution of Likert scaled item with relative frequencies, mean, 1., 2. and 3. quartile.
#' @param x a likert scaled factor variable
#' @param col  color of bars
#' @param col2 color for mean and median
#' @param labels a vector of length 2 with low and high level labels
#' @param main title
#' @param dec decimal digits for percentage on bars
#' @param legend draw legend
#' @param decmean decimal digits for mean diplayed in legend
#' @param quant.type quantile type
#' @param pt.cex median and mean point size
#' @param values legend values
#' @param legendtext legends text
#' @param cex text size
#' @export
#' @examples
#' x<-factor(sample(letters[1:6],100,T))
#' itemplot(x,main="itemplot()",labels=c("I fully\n disagree","I fully\n agree"))

itemplot <- function(
x, # a Likert scaled factor variable
col="lightblue", # color of bars
col2=c("red","blue"), # color for mean and median
labels=c(levels(x)[1],levels(x)[length(levels(x))]),
main="", # title
dec=1, # decimal digits for percentage on bars
legend=TRUE, # draw legend
decmean=2, # decimal digits for mean diplayed in legend
quant.type=4, # quantile type
pt.cex=1.2, # median and mean point size
values=round(c(mean(a,na.rm=T),median(a,na.rm=T),quantile(a,.25,type=quant.type,na.rm=T),quantile(a,.75,type=quant.type,na.rm=T),length(a)),2),
legendtext=c(as.expression(bquote("Mean"==.(round(mean(as.numeric(x),na.rm=T),decmean)))),
                       as.expression(bquote("Median"==.(median(as.numeric(x),na.rm=T)))),
                       as.expression(bquote("1. Quartile"==.(quantile(as.numeric(x),.25,type=quant.type,na.rm=T)))),
                       as.expression(bquote("3. Quartile"==.(quantile(as.numeric(x),.75,type=quant.type,na.rm=T)))),
                       as.expression(bquote("N "[included]==.(sum(table(x))))),
                       as.expression(bquote("N "[missing]==.(sum(is.na(x)))))
                       ),
cex=1 # text size
){
## Preperation
 if(!is.factor(x)) x <- factor(x)
 a <- as.numeric(x)
 if(legend==T) xlim <- c(.5,length(levels(x))+.5)+c(0,length(levels(x))/3) else xlim=c(.5,length(levels(x))+.5) 
# upper y-axis limit
 dh  <- dev.size()[2]
 ms  <- (par()$mai[1]+par()$mai[2])
 fac <- (dh-ms)/dh
 max <- max(prop.table(table(x)))+max(prop.table(table(x)))*fac/3
 options(warn=-1)

# devices mar parameter
 if(main=="") par(mfrow=c(1,1),mar=c(4.1, 1.5, 2.1, 1.5),xpd=TRUE)
 if(main!="") par(mfrow=c(1,1),mar=c(4.1, 1.5, 5.1, 1.5),xpd=TRUE)

### Draw plot and axis
 barplot(prop.table(table(x)),col=col,axes=F,space=c(.75,rep(.2,length(levels(x))-1)),width=.8275,names="",main=main,ylim=c(0,max),xlim=xlim)    
         numlabels=1:length(levels(x))
 axis(1,at=c(1:length(levels(x))),tick=F,labels=numlabels,padj = -1.,hadj=.5)
 axis(1,at=c(1,length(levels(x))),tick=F,labels=labels,padj = 1.0,hadj=.5)

## y-Position of content 
 maxpercent <- max(prop.table(table(x)))+max(prop.table(table(x)))*fac/3
 maxrange   <- max(prop.table(table(x)))+max(prop.table(table(x)))*fac/7
 maxmean    <- max(prop.table(table(x)))+max(prop.table(table(x)))*fac/4.25
 maxline    <- max

## Draw Content
# range
 segments(quantile(a,.25,type=quant.type,na.rm=T),maxrange,quantile(a,.75,type=quant.type,na.rm=T),maxrange,col=col2[2],lwd=2)
 points(quantile(a,.25,type=quant.type,na.rm=T),maxrange,col=col2[2],pch=73)
 points(quantile(a,.75,type=quant.type,na.rm=T),maxrange,col=col2[2],pch=73)
# mean/median
 points(mean(a,na.rm=T),maxmean,pch=3,lwd=2,col=col2[1])
 points(median(a,na.rm=T),maxrange,pch=4,col=col2[2],lwd=2.5)
# percent values
 for(i in 1:length(levels(x))){
    text(i,maxpercent,paste(round(table(x)[i]/length(x)*100,dec),"%"),pos=3,cex=cex)}
# line under percent
 segments(1-.5,maxline,length(levels(x))+.5,maxline)
# legend
 if(legend==T) legend(length(levels(x))+length(levels(x))/8,maxpercent,legendtext,bty="n",cex=cex,pch=c(3,4,73,73,30,30),col=c(col2[1],col2[2],col2[2],col2[2],1),pt.cex=pt.cex)
#options(warn=0)
 } #End
  
## Example
# x<-factor(sample(letters[1:6],100,T))
# itemplot(x,main="itemplot()",legend=T,dec=2,labels=c("I fully\n disagree","I fully\n agree"))

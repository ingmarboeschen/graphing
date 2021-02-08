#' catplot
#'
#' Draws conditioned or unconditioned frequency table of two multi categorial variables
#' @param x multi categorial variable
#' @param y multi categorial variable
#' @param margin=1 conditioned frquencies (1: cond. on y, 2: cond. on x, NULL: unconditioned)
#' @param main="" main title
#' @param xlab="" x axis label
#' @param ylab="" y axis label
#' @param marginleft=5 left margin space
#' @param cexmax=2 max size of squares
#' @example 
#' x<-sample(2010:2020,1000,T)
#' y<-sample(letters[1:10],1000,T)
#' catplot(x,y)

catplot<-function(
x, # multi categorial x variable
y, # multi categorial y variable
margin=1, # conditioned frquencies (1: cond. on y, 2: cond. on x, NULL: unconditioned)
main="",
xlab="",
ylab="",
cex.lab=1,
marginleft=5,
cexmax=4
){
x<-factor(x)
y<-factor(y)
y<-factor(y,levels(y)[length(levels(y)):1])
kat<-names(table((y)))
tab<-prop.table(table(y,x),margin=margin)

mar<-par()$mar
mar[2]<-marginleft
par(mar=mar)
plot(1,xlim=c(0-.08*dim(tab)[2],dim(tab)[2]-1),ylim=c(dim(tab)[1]*0.05,-length(kat)),axes=F,xlab=xlab,ylab=ylab,type="n",main=main)
axis(1,0:(dim(tab)[2]-1),c(names(table(x))),tick=F,cex.axis=cex.lab);
#axis(3,0:(dim(tab)[2]-1),c(names(table(x))),tick=F)
axis(2,-1:-length(kat),rownames(tab),las=2,tick=F,cex.axis=cex.lab);
#axis(4,-1:-length(kat),rownames(tab),las=2,tick=F)
for(i in 1:dim(tab)[1]){ # Kategorien
for(j in (1:dim(tab)[2])){ # Stunden
points(j-1,-i,pch=15,cex=tab[i,j]/max(tab)*cexmax,col=rainbow(length(kat))[i])
points(0-.08*dim(tab)[2],-i,cex=(table(y)/max(table(y))*cexmax)[i],col="grey",pch=16)
points(j-1,dim(tab)[1]*0.05,cex=(table(x)/max(table(x))*cexmax)[j],col="grey",pch=16)
}}
abline(v=0-.08*dim(tab)[2],lty=2,col="grey")
abline(h=dim(tab)[1]*0.05,lty=2,col="grey")

#abline(v=c(-2.8,-1.2),lty=2,col="grey20")
#abline(h=c(-.3,.3),lty=2,col="grey20")
par(xpd=TRUE)
text(0-.08*dim(tab)[2],dim(tab)[1]*0.05,expression(Sigma),cex=cex.lab*1.5)
par(xpd=FALSE)

}


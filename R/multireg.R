#' Draws and estimates multiple regression model with two independent variables
#' @param x # a numeric or factor variable
#' @param z # a numeric or factor variable
#' @param interaction=TRUE Logical. if TRUE calculates and draws model with interaction  
#' @param ylab=NULL y axis label
#' @param xlab=NULL x axis label
#' @param main=NULL main title
#' @param col=NULL color
#' @param xlim=NULL limits of x axis
#' @param ylim=NULL limits of x axis
#' @param lwd=2 regression line width
#' @param pch=16 point character
#' @param legend=TRUE Logical, if TRUE draws legend
#' @param locator="top" locatin of legend
#' @param lty=NULL line type of legend elements
#' @param bty="n" box type of legend
#' @param horiz=TRUE adjust legend text horizontally
#' @param title=NULL legend title
#' @param shift=.15 distance of groups in interactionplot
#' @param quantiles_z=c(.25,.5,.75) vector of quantiles to split z
#' @param colquant=2:(length(quantiles_z)+1) vector of colors for groups
#' @export
#' @examples
#' # create variables
#' metricY<-rnorm(100)
#' metricX1<-rnorm(100)
#' metricX2<-rnorm(100)
#' binaryY<-rbinom(100,1,.5)
#' factorY<-factor(sample(paste("Category",1:3),100,T))
#' factorX1<-factor(sample(paste("Level X1",1:3),100,T))
#' factorX2<-factor(sample(paste("Level X2",1:3),100,T))
#' metricY<metricX1*metricX2+rnorm(100)
#' multireg(metricY,metricX1,metricX2)
#' binaryY<-ifelse(((metricX1*metricX2)+rnorm(100))>0,1,0)
#' # use multireg
#' multireg(metricY,metricX1,metricX2,legend=T,main="metric Y ~ metric X1 * metric X2")#
#' multireg(metricY,metricX1,factorX2,legend=T,title=NULL,main="metric Y ~ metric X1 * factor X2")#
#' multireg(metricY,factorX1,factorX2,legend=T,main="metric Y ~ factor X1 * factor X2")#
#' multireg(binaryY,metricX1,metricX2,legend=,main="binary Y ~ metric X1 * metric X2")#
#' multireg(binaryY,metricX1,factorX2,legend=F,title=NULL,main="binary Y ~ metric X1 * factor X2")#
#' multireg(binaryY,factorX1,factorX2,legend=T,main="binary Y ~ factor X1 * factor X2")#
#' multireg(factorY,metricX1,metricX2,legend=T,main="factor Y ~ metric X1 * metric X2")#
#' multireg(factorY,metricX1,factorX2,legend=T,main="factor Y ~ metric X1 * factor X2")#
#' multireg(factorY,factorX1,factorX2,legend=T,main="factor Y ~ factor X1 * factor X2")#


multireg<-function(y,
x, # a numeric or factor variable
z, # a numeric or factor variable
interaction=TRUE,
ylab=NULL,
xlab=NULL,
main=NULL,
col=NULL,
xlim=NULL,
ylim=NULL,
lwd=2,
pch=16,
# legend
legend=TRUE,
locator="top",
lty=NULL,
bty="n",
horiz=TRUE,
title=NULL,
#interaction plot
shift=.15,
# 2 metric IVs
quantiles_z=c(.25,.5,.75),
colquant=2:(length(quantiles_z)+1)
){

## Function to dummy.code
dummy.code<-function(x){diag(1,length((x)),length((x)))}
## main model class selection
if(is.numeric(y)&length(unique(na.omit(y)))>2){mod_main<-"lm";if(is.null(ylab)) ylab<-deparse(substitute(y))} 
if(length(unique(na.omit(y)))==2) mod_main<-"glm" 
if(is.factor(y)&length(unique(na.omit(y)))>2){ mod_main<-"nlm" ;if(is.null(ylab)) ylab<-deparse(substitute(y))}
## sub model class selection
if(is.factor(x)&is.factor(z)){mod_sub<-"2factorIVs";  if(is.null(xlab)) xlab<-deparse(substitute(x))}
if(is.factor(x)&is.numeric(z)){mod_sub<-"xfactor";  if(is.null(xlab)) xlab<-deparse(substitute(z))}
if(is.factor(z)&is.numeric(x)){mod_sub<-"zfactor";  if(is.null(xlab)) xlab<-deparse(substitute(x))}
if(is.numeric(z)&is.numeric(x)){mod_sub<-"2metricIVs";if(is.null(xlab)) xlab<-deparse(substitute(x));title_z<-deparse(substitute(z))}

if(!is.null(title)&mod_sub=="zfactor") title<-deparse(substitute(z))
if(!is.null(title)&mod_sub=="xfactor") title<-deparse(substitute(x))

## remove NA
#stopifnot(!is.na(y)&!is.na(x)&!is.na(z)
sel<-!is.na(y)&!is.na(x)&!is.na(z)
t1<-length(x)
x<-x[sel];y<-y[sel];z<-z[sel]
t2<-length(x)
sumNA<-t1-t2

#########################
## für metrische AV (LM)
######################
if(mod_main=="lm"){
# model and coefficients
if(interaction==T){ mod<-lm(y~x*z);coef<-mod$coef}
if(interaction==F){ mod<-lm(y~x+z);coef<-mod$coef}
## both IVs faktor variables
if(mod_sub=="2factorIVs"){
if(is.null(ylim)) ylim<-range(y)+c(0,abs(range(y))[2]/8)
if(is.null(col)) col<-2:(length(levels(z))+1)
#line/point shift
shift<-(1:length(levels(z))-mean(1:length(levels(z))))/15
# with interaction
if(interaction==T){
interaction.plot(x,z,y,ylim=ylim,col="white",lwd=.0001,main=main,xlab=xlab,ylab=ylab,legend=F)
 m<-matrix(tapply(y,z:x,mean),ncol=length(levels(x)),byr=T)
  for(i in 1:length(levels(z))){
    points((1:length(levels(x)))+shift[i],m[i,],type="l",col=col[i],lwd=lwd)
  }
}
# without interaction
if(interaction==F){ 
interaction.plot(x,z,y,ylim=ylim,col="white",lwd=.0001,main=main,xlab=xlab,ylab=ylab,legend=F)
  for(i in 1:dim(dummy.code(levels(factor(z))))[2]){
   ypoints<-NULL;xpoints<-NULL
    for(j in 1:dim(dummy.code(levels(factor(x))))[2]){
     temp<-c(1,dummy.code(levels(factor(x)))[j,2:length(levels(factor(x)))],dummy.code(levels(factor(z)))[i,2:length(levels(factor(z)))])
     ypoints[j]<-temp%*%lm(y~x+z)$coef
     xpoints[j]<-j+shift[i]
     }
    points(ypoints~(xpoints),type="l",col=col[i],lwd=lwd)
  }
}
if(is.character(col)) cols<-as.character(factor(z,levels(z),col))
if(is.numeric(col)) cols<-as.numeric(as.character(factor(z,levels(z),col)))
# draw points
points(as.numeric(x)+((as.numeric(z))-mean(unique(as.numeric(z))))/15,y,pch=pch,col=cols)
# draw legend
if(legend==TRUE) legend(locator,levels(z),horiz=horiz,bty=bty,pch=pch,col=col,title=title)  
}

# z is faktor
if(mod_sub=="zfactor"|mod_sub=="xfactor"){
if(mod_sub=="zfactor"){
x1<-x;x2<-z
# prediction function
if(interaction==T){curvefun<-function(x,a){rbind(c(1,x,as.numeric((a)), as.numeric((a))*x))%*%coef}}
if(interaction==F){curvefun<-function(x,a){rbind(c(1,x,as.numeric((a))))%*%coef}}
}

if( mod_sub=="xfactor"){
  # prediction function
  x1<-z;x2<-x
  if(interaction==T){curvefun<-function(x,a){rbind(c(1,as.numeric((a)),x, as.numeric((a))*x))%*%coef}}
  if(interaction==F){curvefun<-function(x,a){rbind(c(1,as.numeric((a)),x))%*%coef}}
}

if(is.null(col)) col<-2:(length(levels(factor(x2)))+1)
if(is.character(col)) cols<-as.character(factor(x2,levels(x2),col))
if(is.numeric(col)) cols<-as.numeric(as.character(factor(x2,levels(x2),col)))
if(is.null(xlim))  xlim=  range(x1,na.rm=T); 
 rangex<-xlim+c(-abs(xlim[1]),abs(xlim[2]))/8
if(is.null(ylim))  ylim=  range(y,na.rm=T); ylim<-ylim+c(0,abs(ylim[2]-ylim[1])/8)
# dummy matrix
a<-dummy.code(levels(factor(x2)))[,2:length(levels(factor(x2)))]
# draw points
plot(y~x1,type="n",main=main,xlab=xlab,ylab=ylab,ylim=ylim,xlim=xlim)
points(y~x1,col=cols,pch=pch)
# draw lines
for(i in 1:length(levels(x2))){
  points(rangex,c(curvefun(rangex[1],a[i,]),curvefun(rangex[2],a[i,])),type="l",col=col[i],lwd=lwd) }
# draw legend
if(legend==TRUE) legend(locator,levels(x2),horiz=horiz,bty=bty,pch=pch,col=col,title=title)  
} #End z or x is factor

# both IVs metric
if(mod_sub=="2metricIVs"){
# prediction function
if(interaction==T){curvefun<-function(x,z){rbind(c(1,x,z,x*z))%*%coef}}
if(interaction==F){curvefun<-function(x,z){rbind(c(1,x,z))%*%coef}}
if(is.null(ylim))  ylim=  range(y,na.rm=T); ylim<-ylim+c(0,(ylim[2]-ylim[1])/8)
if(is.null(col)) col<-"grey"
if(is.null(xlim))  xlim=  range(x,na.rm=T); 
 rangex<-xlim+c(-abs(xlim[1]),abs(xlim[2]))/8
if(is.null(ylim))  ylim=  range(y,na.rm=T); ylim<-ylim+c(0,abs(ylim[2]-ylim[1])/8)

plot(y~x,type="n",main=main,xlab=xlab,ylab=ylab,ylim=ylim,xlim=xlim)
points(y~x,pch=pch,col=col)
# draw lines
for(i in 1:length(quantiles_z)){
  points(rangex,c(curvefun(rangex[1],quantile(z,quantiles_z[i],na.rm=T)),curvefun(rangex[2],quantile(z,quantiles_z[i],na.rm=T))),type="l",col=colquant[i],lwd=lwd) }
# draw legend
if(legend==TRUE){
   predz<-round(quantile(z,quantiles_z,na.rm=T),2)
   legtext<-NULL
      for(i in 1:length(quantiles_z)){
        legtext[i] <-as.expression(bquote(.(title_z)[.(quantiles_z[i])] == .(predz[i])))
      }
  legend(locator,legend=legtext,title=title,horiz=horiz,bty=bty,lty=lty,col=colquant,pch=pch)  
  }
} #End both IVs metric

}# End LM

## dichotous DV (GLM)
if(mod_main=="glm"){
if(interaction==T){ mod<-glm(y~x*z,family="binomial");coef<-mod$coef}
if(interaction==F){ mod<-glm(y~x+z,family="binomial");coef<-mod$coef}
if(is.null(ylab)) ylab<-paste("P(y = ",levels(factor(y))[2],")",sep="")
if(is.factor(y)) y=as.numeric(y)-1
# x and z faktor
if(mod_sub=="2factorIVs"){
 x1<-x;x2<-z
 if(is.null(col)) col<-paste("grey",round(seq(20,75,length=length(levels(factor(x2))))))
 table1=table(x2,x1,y)[,,2]/(table(x2,x1,y)[,,2]+table(x2,x1,y)[,,1])
 barplot(table1,main=main,xlab=xlab,ylab=ylab,beside=T,ylim=c(0,max(table1)+max(table1)/8),col=col)
 if(legend==T) legend("top",levels(x2),title=title,col=col,pch=16,bty="n",horiz=T)
}
# x or z faktor
if(mod_sub=="zfactor"|mod_sub=="xfactor"){
# x metric z faktor
if(mod_sub=="zfactor"){    
x1<-x; x2<-factor(z)
  if(is.null(col)) col<-2:(length(levels(factor(z)))+1)
  if(is.character(col)) cols<-as.character(factor(z,levels(z),col))
  if(is.numeric(col)) cols<-as.numeric(as.character(factor(z,levels(z),col)))
  if(is.null(xlim))  xlim=  range(x1,na.rm=T) 
  if(is.null(ylim))  ylim= c(0,1.1) 
 if(is.character(col)) cols<-as.character(factor(z,levels(z),col))
 if(is.numeric(col)) cols<-as.numeric(as.character(factor(z,levels(z),col)))
   }
# z metric x faktor
if(mod_sub=="xfactor"){    
x2<-factor(x); x1<-z
  if(is.null(col)) col<-2:(length(levels(factor(x)))+1)
  if(is.character(col)) cols<-as.character(factor(x,levels(x),col))
  if(is.numeric(col)) cols<-as.numeric(as.character(factor(x,levels(x),col)))
  if(is.null(xlim))  xlim=  range(x1,na.rm=T) 
  if(is.null(ylim))  ylim= c(0,1.1) 
 if(is.character(col)) cols<-as.character(factor(x,levels(x),col))
 if(is.numeric(col)) cols<-as.numeric(as.character(factor(x,levels(x),col)))
   }
rangex=  range(x1,na.rm=T)
# draw
plot(jitter(y,.1)~x1,type="p",pch=pch,main=main,xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,col=cols)
  if(interaction==T){ curvefun<-function(x){rbind(as.numeric(cbind(1,x,rbind(a),rbind(a)*x))%*%glm(y~x1*x2,fam=binomial(link = "logit"))$coef)} }
  if(interaction==F){ curvefun<-function(x){rbind(as.numeric(cbind(1,x,rbind(a)))%*%glm(y~x1+x2,fam=binomial(link = "logit"))$coef)}}
  levels=levels(x2)
  for(i in 1:length(levels)){
#    points(jitter(y[x2==levels(x2)[i]],.1)~ x1[x2==levels(x2)[i]],col=col[i],pch=pch)
    a<-  unique(dummy.code(levels(x2)))[i,2:length(unique(levels(x2)))]
    xtemp=seq(rangex[1],rangex[2],length=20)
    ytemp=NULL
    for(j in 1:length(xtemp)){ytemp[j]=curvefun(xtemp[j])}
    ytemp=exp(ytemp)/(1+exp(ytemp))
    points(ytemp~xtemp,type="l",col=col[i],lwd=lwd)}
 if(legend==T)   legend(locator,levels(x2),title=title,horiz=horiz,bty="n",pch=pch,col=col)
}# END x or z is factor

# both IVs metric
if(mod_sub=="2metricIVs"){
if(interaction==T){  curvefun<-function(x,z){(cbind(1,x,z,x*z)%*%coef)}}
if(interaction==F){  curvefun<-function(x,z){(cbind(1,x,z)%*%coef)}}
if(is.null(ylim))  ylim=  range(y,na.rm=T); ylim<-ylim+c(0,(ylim[2]-ylim[1])/8)
if(is.null(col)) col<-"grey"
if(is.null(xlim))  xlim=  range(x,na.rm=T); 
 rangex<-xlim+c(-abs(xlim[1]),abs(xlim[2]))/8
if(is.null(ylim))  ylim= c(0,1.1) 
plot(jitter(y,.1)~x,type="p",main=main,xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,pch=pch,col=col)
#points(y~x,pch=pch,col=col)
levels=quantile(z,quantiles_z,na.rm=T)
quartx2<-round(quantile(z,quantiles_z,na.rm=T),1)
xtemp=seq(min(rangex,na.rm=T),max(rangex,na.rm=T),length=30)
for(i in 1:length(levels)){
  a<- levels[i]
  ytemp=NULL
   for(j in 1:length(xtemp)){
      ytemp[j]=curvefun(xtemp[j],a)}
      ytemp=exp(ytemp)/(1+exp(ytemp))
    points(ytemp~xtemp,type="l",col=i+1,lwd=2)
  }
# draw legend
  if(legend==TRUE){
   predz<-round(quantile(z,quantiles_z,na.rm=T),2)
   legtext<-NULL
      for(i in 1:length(quantiles_z)){
        legtext[i] <-as.expression(bquote(.(title_z)[.(quantiles_z[i])] == .(predz[i])))
      }
   legend(locator,legend=legtext,title=title,horiz=horiz,bty=bty,pch=pch,col=colquant,lty=lty)  
  }
}# END both IVs metric
}# End GLM

## polychotous IV (MNL)
if(mod_main=="nlm"){
library(ggplot2)
library(nnet)
library(reshape2)
library(mypack)

if(interaction==T){ mod<-multinom(y~x*z);coef<-mod$wts}
if(interaction==F){ mod<-multinom(y~x+z);coef<-mod$wts}
# both IVs factor
if(mod_sub=="2factorIVs") {
bp3d(z,x,y,main=main,xlab=xlab,ylab=ylab,legend=legend)
} # END both IVs factor

# x or z faktor
if(mod_sub=="zfactor"|mod_sub=="xfactor"){
  # x metric z faktor
  if(mod_sub=="zfactor"){x1<-x; x2<-factor(z)}
  # z metric x faktor
  if(mod_sub=="xfactor"){x2<-factor(x); x1<-z}

  if(interaction==T){ ml<-data.frame(y=y,x2=x2,x1=x1);model <- multinom(y~ x2 * x1, data = ml)}
  if(interaction==F){ ml<-data.frame(y=y,x2=x2,x1=x1);model <- multinom(y~ x2 + x1, data = ml)}

  zv <- summary(model)$coefficients/summary(model)$standard.errors
  p <- (1 - pnorm(abs(zv), 0, 1)) * 2
  dx2 <- data.frame(x2 = levels(factor(x2)), x1 = mean(ml$x1))
  predict(model, newdata = dx2, "probs")
  dx1 <- data.frame(x2 = rep(levels(factor(x2)), each = 20), x1 = rep(seq(min(x1,na.rm=T),max(x1,na.rm=T),length=20),length(levels(factor(x2)))))
  pp.x1 <- cbind(dx1, predict(model, newdata = dx1, type = "probs", se = TRUE))
  by(pp.x1[, 3:5], pp.x1$x2, colMeans)
  lpp <- melt(pp.x1, id.vars = c("x2", "x1"), value.name = "probability")
  # plot
  plot<-ggplot(lpp, aes(x = x1, y = probability, colour = x2)) 
  plot<- plot + geom_line() + facet_grid(variable ~.)+theme(panel.background = element_rect(fill=NA))
  plot<-plot +ylab(ylab)+xlab(xlab)+labs(title=main)+scale_y_continuous(breaks=seq(0,1,length=11))
  plot<- plot + scale_colour_discrete(name = title)
  plot<- plot + theme(axis.title.y=element_text(vjust=1)) +theme(axis.title.x=element_text(vjust=-0.4))
  print(plot)
}# END x or z is factor

# both IVs metric
if(mod_sub=="2metricIVs"){
  x1<-x;x2<-z
  predz<-quantile(x2,quantiles_z,na.rm=T)
  if(interaction==T){ ml<-data.frame(y=y,x2=x2,x1=x1);model <- multinom(y~ x2 * x1, data = ml)}
  if(interaction==F){ ml<-data.frame(y=y,x2=x2,x1=x1);model <- multinom(y~ x2 + x1, data = ml)}

  zv <- summary(model)$coefficients/summary(model)$standard.errors
  p <- (1 - pnorm(abs(zv), 0, 1)) * 2
  dx2 <- data.frame(x2 = predz, x1 = mean(ml$x1))
  dx1 <- data.frame(x2 = rep(predz, each = 240), x1 = rep(seq(min(x1,na.rm=T),max(x1,na.rm=T),length=120),length(predz)))
  pp.x1 <- cbind(dx1, predict(model, newdata = dx1, type = "probs", se = TRUE))
  lpp <- melt(pp.x1, id.vars = c("x2", "x1"), value.name = "probability")
  lpp$x2<-factor(lpp$x2,levels(factor(lpp$x2)),levels(factor(round(lpp$x2,2))))
  #if(!is.null(title)) title<-paste("Value in",title_z)
  legtext<-NULL
  for(i in 1:length(quantiles_z)){legtext[i] <-as.expression(bquote(.(title_z)[.(quantiles_z[i])] == .(round(predz[i],2))))}
  # plot
  plot<-ggplot(lpp, aes(x = x1, y = probability, colour = x2)) 
  plot<- plot + geom_line() + facet_grid(variable ~.)+theme(panel.background = element_rect(fill=NA))
  plot<-plot +labs(title=main)+scale_y_continuous(breaks=seq(0,1,length=11))+ylab(ylab)+xlab(xlab)
  plot<- plot + scale_colour_discrete(name=title,breaks=levels(lpp$x2),labels=legtext)
  plot<- plot + scale_fill_discrete(breaks=predz,labels=legtext)
  plot<- plot + theme(axis.title.y=element_text(vjust=1)) +theme(axis.title.x=element_text(vjust=-0.4))
  print(plot)
}# END both IVs metric
} # END MNL

## Model output
print(summary(mod))
## NA report
if(sumNA==1) warning(paste(sumNA,"case was deleted, due to missings in x, y, and/or z"))
if(sumNA>1) warning(paste(sumNA,"cases where deleted, due to missings in x, y, and/or z"))
}


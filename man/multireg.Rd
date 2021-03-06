% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multireg.R
\name{multireg}
\alias{multireg}
\title{Draws and estimates multiple regression model with two independent variables}
\usage{
multireg(
  y,
  x,
  z,
  interaction = TRUE,
  ylab = NULL,
  xlab = NULL,
  main = NULL,
  col = NULL,
  xlim = NULL,
  ylim = NULL,
  lwd = 2,
  pch = 16,
  legend = TRUE,
  locator = "top",
  lty = NULL,
  bty = "n",
  horiz = TRUE,
  title = NULL,
  shift = 0.15,
  quantiles_z = c(0.25, 0.5, 0.75),
  colquant = 2:(length(quantiles_z) + 1)
)
}
\arguments{
\item{x}{# a numeric or factor variable}

\item{z}{# a numeric or factor variable}

\item{interaction=TRUE}{Logical. if TRUE calculates and draws model with interaction}

\item{ylab=NULL}{y axis label}

\item{xlab=NULL}{x axis label}

\item{main=NULL}{main title}

\item{col=NULL}{color}

\item{xlim=NULL}{limits of x axis}

\item{ylim=NULL}{limits of x axis}

\item{lwd=2}{regression line width}

\item{pch=16}{point character}

\item{legend=TRUE}{Logical, if TRUE draws legend}

\item{locator="top"}{locatin of legend}

\item{lty=NULL}{line type of legend elements}

\item{bty="n"}{box type of legend}

\item{horiz=TRUE}{adjust legend text horizontally}

\item{title=NULL}{legend title}

\item{shift=.15}{distance of groups in interactionplot}

\item{quantiles_z=c(.25, .5, .75)}{vector of quantiles to split z}

\item{colquant=2:(length(quantiles_z)+1)}{vector of colors for groups}
}
\description{
Draws and estimates multiple regression model with two independent variables
}
\examples{
# create variables
metricY<-rnorm(100)
metricX1<-rnorm(100)
metricX2<-rnorm(100)
binaryY<-rbinom(100,1,.5)
factorY<-factor(sample(paste("Category",1:3),100,T))
factorX1<-factor(sample(paste("Level X1",1:3),100,T))
factorX2<-factor(sample(paste("Level X2",1:3),100,T))
metricY<metricX1*metricX2+rnorm(100)
multireg(metricY,metricX1,metricX2)
binaryY<-ifelse(((metricX1*metricX2)+rnorm(100))>0,1,0)
# use multireg
multireg(metricY,metricX1,metricX2,legend=T,main="metric Y ~ metric X1 * metric X2")#
multireg(metricY,metricX1,factorX2,legend=T,title=NULL,main="metric Y ~ metric X1 * factor X2")#
multireg(metricY,factorX1,factorX2,legend=T,main="metric Y ~ factor X1 * factor X2")#
multireg(binaryY,metricX1,metricX2,legend=,main="binary Y ~ metric X1 * metric X2")#
multireg(binaryY,metricX1,factorX2,legend=F,title=NULL,main="binary Y ~ metric X1 * factor X2")#
multireg(binaryY,factorX1,factorX2,legend=T,main="binary Y ~ factor X1 * factor X2")#
multireg(factorY,metricX1,metricX2,legend=T,main="factor Y ~ metric X1 * metric X2")#
multireg(factorY,metricX1,factorX2,legend=T,main="factor Y ~ metric X1 * factor X2")#
multireg(factorY,factorX1,factorX2,legend=T,main="factor Y ~ factor X1 * factor X2")#
}

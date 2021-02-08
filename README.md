# graphing
Some convenient graphic functions for R

## prettybarplot()
```r
x<-sample(letters[1:4],100,T)
y<-sample(1:3,100,T)
prettybarplot(table(x)) # one dimensional
prettybarplot(table(x,y)) # two dimensional
``` 
<img src="./preview/prettybarplot1d.png" height="400"><img src="./preview/prettybarplot2d.png" height="400">


## profileline()
```r
x<-sample(letters[1:4],100,T)
anibarplot(table(x))
``` 
<img src="./preview/profileline2.png" height="400">

## CIV(), confidence interval violin plot
```r
x<-sample(paste("group",1:4),100,T)
y<-10+as.numeric(factor(x))*2+rnorm(length(x))
CIV(x,y,main="Confidence Intervall Violin Plot")

``` 
<img src="./preview/Confidence Intervall Violin Plot.png" height="400">

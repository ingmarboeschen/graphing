# graphing
Some convenient graphic functions for R

## prettybarplot()
```r
x<-sample(letters[1:4],100,T)
y<-sample(1:3,100,T)
prettybarplot(table(x))
prettybarplot(table(x,y))
``` 
<img src="./preview/prettybarplot1d.png" height="400"><img src="./preview/prettybarplot2d.png" height="400">


## ani.barplot()
```r
x<-sample(letters[1:4],100,T)
anibarplot(table(x))
``` 
<img src="./preview/ani.barplot.gif" height="400">

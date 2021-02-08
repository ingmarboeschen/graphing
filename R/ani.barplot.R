#' Animated Barplot
#'
#' Creates an animation of a one dimensional frequency table or vector. Animation can be saved to a folder in several formats.
#' @param x a vector or one dimensional frequwncy table of values describing the bars which make up the plot.
#' @param col a vector of colors for the bars. By default col is set to "lightblue".
#' @param ylab a label for the y axis 
#' @param xlab a label for the x axis 
#' @param main title of plot
#' @param ylim   limits for the y axis.
#' @param box a logical value. If FALSE no box around plot will be drawn
#' @param names a vector of names to be plotted below each bar. If this argument is left empty, the names are taken from the names freuqency table of x
#' @param cex.names expansation of names below bars
#' @param border color of bar border
#' @param space space between bars
#' @param text a logical value. If TRUE text will be drawn on bars
#' @param cex expansation of text on bars
#' @param coltext text color
#' @param pre pre text of absolute frequencies on bars
#' @param post post text of absolute frequencies on bars
#' @param format the image output format e.g.: "png", "pdf", "jpeg", etc.
#' @param digit the numer of decimal places to display relative frequencies with e.g.: digits=3 -> 22,2
#' @param frames animation length in frames
#' @param folder folder to save animation as png frames to. If "" no images will be saved
#' @param ani.text a logical value. If TRUE text on bars will be animated as well. If FALSE no text on bars will apear.
#' @param width png width (only works if format="png")
#' @param height png height (only works if format="png")
#' @param point png point size (only works if format="png")
#' @param res png resolution px/in (only works if format="png")
#' @param bg a logical value. If TRUE background will be drawn.
#' @param bg.from background color starting color
#' @param bg.to background color end color
#' @param bg.increase a logical value. If TRUE background color will increase from bg.from to bg.to
#' @param axes a logical value. If TRUE axes will be drawn.
#' @param cex.axis text size of axis
#' @export
#' @examples
#' x<-sample(paste("group",letters[1:5]),200,T)
#' ani.barplot(prop.table(table(x)))


ani.barplot<-function(
x, # a one dimensional table or vector of frquencies
col="lightblue", # color of bars
ylab="", # y axis label
xlab="", # x axis label
main="", # title
ylim=c(0,max(x)+max(x)/6),
box=TRUE, # draw box around plot
## bar options
names="", # names of bars
cex.names = par("cex.axis"), # text size of bar names
border="black", # color of bar border
space=.4, #space between bars
## text on bars ooptions
text=TRUE, # draw text
cex=1, # text size
coltext=1, # text color
pre="n =", # pre Text of absolute frequencies on bars
post="%", # pre Text of absolute frequencies on bars
## animation options
format="png", # output image format
digit=2, # digits to display relative frequencies with e.g.: digits=3 -> 22,2%
frames=100, # animation length in frames
folder="", # folder to save animation as png frames to. If "" no images will be saved
ani.text=TRUE, # animate text
width=1920, # png width
height=1080, # png height
point=8, # png point size
res=300, # png resolution px/in
## background options
bg=TRUE, # draw background
bg.from="grey", # background color starting color
bg.to="white", # background color end color
bg.increase=TRUE, # increase in background color from bottom to to
## axes options
axes = TRUE, # draw axes
cex.axis = par("cex.axis") # text size of axis
){

if(names==""){
if(is.table(x)) names<-names(x)
if(is.vector(x)) names<-x
}
 options(warn=-1)

## Animation matrix
if(sum(x)==1) xtemp<-x*frames
if(sum(x)>1) xtemp<-prop.table(x)*frames

for(i in 1:length(x)){
 if(i==1){ 
   rows1<-round(xtemp[i])
   animatrix<-c(seq(0,x[i],length=rows1),rep(x[i],times=sum(round(xtemp[2:length(x)]))))
 }
 if(i>1&i<length(x)){ 
   rows2<-round(xtemp[i])
   animatrix<-cbind(animatrix,c(rep(0,times=rows1),seq(0,x[i],length=rows2),rep(x[i],times=sum(round(xtemp[(i+1):length(x)])))))
   rows1<-rows1+rows2
 }
 if(i==length(x)){
   rows2<-round(xtemp[i])
   animatrix<-cbind(animatrix,c(rep(0,times=rows1),seq(0,x[i],length=rows2)))
 }
}
  
# preperation
colnames(animatrix)<-names(x)
rownames(animatrix)<-NULL
if(ylab==""&sum(x)>1) ylab<-"absolute frequencies"
if(ylab==""&sum(x)==1) ylab<-"relative frequencies"
# create folder if it does not exist
if(folder!=""|dir.exists(paste(getwd(),"/",folder,sep=""))==F) dir.create(paste(getwd(),"/",folder,sep=""))
# start animation
for (i in 1:dim(animatrix)[1]){
# open png device
if(folder!=""&format=="png")  png(paste("./",folder,"/plot",i+1000000,".png",sep=""),width = width, height = height,point=point,res=res)
if(folder!=""&format=="pdf")  pdf(paste("./",folder,"/plot",i+1000000,".pdf",sep=""))
# plot
prettybarplot(as.table(animatrix[i,]),col=col,ylab=ylab,main=main,xlab=xlab,space=space, cex=cex, coltext=coltext, text=F, 
 cex.names = cex.names,cex.axis = cex.axis,axes = axes, ylim=ylim,bg=bg,bg.from=bg.from,bg.to=bg.to,bg.increase=bg.increase,names=names,
 box=box,border=border)
# Text
if(text==TRUE){
 for(j in 1:length((x))){
 if(ani.text==T&sum(x)>1.05) text((-.5+space+j+((j-1)*space)),(animatrix[i,j]),paste(pre,round(animatrix[i,j]),sep=" "),pos=3,cex=cex,col=coltext)
 if(ani.text==T&sum(x)<=1.05) text((-.5+space+j+((j-1)*space)),(animatrix[i,j]),paste(round(animatrix[i,j],digit)*100,post,sep=" "),pos=3,cex=cex,col=coltext)
 }}
# close png device
if(folder!="")  dev.off()
}

} # end

## Example:
# x<-sample(paste("group",letters[1:5]),200,T)
# ani.barplot(prop.table(table(x)),folder="ani.barplot",format="pdf")

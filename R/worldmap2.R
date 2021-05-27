#require(countrycode)
#require(rworldmap)
#require(geosphere)
#require(rgeos)
#library(RColorBrewer)
#load("/home/ingmar/JATSdecoderEvaluation/01_JATSdecoder/JATSdecoder - Language Resources and Evaluation/tags/country.rda")
#country<-head(country,2000)

#' worldmap2
#' @param country a list with vectors of country names
#' @param connection logical. If TRUE draws country connections
#' @param palette colour palette from brewer.pal(). 
#'  One of 'Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 
#'  'Oranges', 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 
#'  'RdPu', 'Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 
#'  'BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 
#'  'RdYlGn' 'Spectral'
#' @param legend logical. If TRUE legends are drawn
#' @param legend1adjust adjust y position of upper legend and title
#' @param legend2adjust adjust y position of lower legend
#' @param region one of: ('eurasia' 'africa' 'latin america' 'north america' 'uk' 'oceania' 'asia')
#' @param main title
#' @param ocean ocean color
#' @param xpd logical. If TRUE ocean color is set to background color
#' @param missingCol color of non present countries
#' @param mar mar adjustment
#' @param mai mai adjustment
#' @export
#' @examples
#' worldmap2(country=list(c("Germany","Austria","S. Korea"),
#' c("Germany","Domenican Rep.","S. Korea"),c("Germany","Austria","S. Korea"),
#' c("Germany","Spain"),c("Germany","Spain"),c("United States","Germany"),
#' c("Germany","Cuba","Bolivia","South Africa")),
#'           connection=T,legend=T,xpd=F,
#'           legend1=-10,legend2=-15)

worldmap2<-function(country,
                    connections=TRUE,
                    region=NULL, # one of : (eurasia africa latin america north america uk oceania asia )
                    main="country and country connection frequency",
                    ocean="grey95",
                    missingCol="white",
                    legend=TRUE,
                    xpd=TRUE,
                    mar=c(0,0,0,0),
                    mai=c(0,0,ifelse(legend==TRUE,.1,0),0),
                    palette="Blues", # colour palette from brewer.pal()
                    legend1adjust=0,
                    legend2adjust=0
   ){
  tab<-NULL
  # if is vector convert to list
  if(is.vector(country)) country<-as.list(country)
  # if country is empty
  if(sum(unlist(lapply(country,length)))==0) country=""
  # set graphic parameters
  par(mar=mar,mai=mai,xpd=xpd,bg=ifelse(xpd,ocean,"white"))
  # recode countries
  country<-lapply(country,function(x) gsub("S. Korea","South Korea",x))
  country<-lapply(country,function(x) gsub("N. Korea","North Korea",x))
  country<-lapply(country,function(x) gsub("Czech Rep\\.","Czech Republic",x))
  country<-lapply(country,function(x) gsub("Sint Maarten","Saint-Martin",x))
  country<-lapply(country,function(x) gsub("^Micronesia$","Federated States of Micronesia",x))
  country<-lapply(country,function(x) gsub("Bosnia and Herz\\.","Bosnia and Herzegovina",x))
  country<-lapply(country,function(x) gsub("Dominican Rep\\.","Dominican Republic",x))
  country<-lapply(country,function(x) gsub("Eq\\. Guinea","Equatorial Guinea",x))
  country<-lapply(country,function(x) gsub("Fr\\. Polynesia","French Polynesia",x))
  country<-lapply(country,function(x) gsub("GAZA$","Gaza Strip",x))
  
  # frequency table of countries
  data<-data.frame(table(unlist(country)))
  colnames(data)<-c("country","value")
  # get scale [0;scale]
  scale<-10^(nchar(max(data$value))-1)
  # if scale is too large -> shorten
  while((scale*10)/2 >= max(data$value)) scale<-scale/2
  # reset scale if has 1 or two countries only
  if(scale==.125) scale=.1
  if(scale==.25) scale=.2
  # prepare grafic by country
  sPDF <- rworldmap::joinCountryData2Map(data,joinCode = "NAME",
                              nameJoinColumn = "country",
                              verbose = TRUE)
  # draw grafic  
  # define legend splits dependen on scale
  if(scale==.1){
    colourPalette <- c(missingCol,RColorBrewer::brewer.pal(3,palette)[3])
    legendcat<-c(0,5,10)
  }else{
  if(scale==.2){
    colourPalette <- c(missingCol,RColorBrewer::brewer.pal(3,palette)[2:3])
    legendcat<-c(0,1,5,10)
  }else{
  colourPalette <- RColorBrewer::brewer.pal(5,palette)
  legendcat<-c(0,2,4,6,8,10)
  }
  }
  if(is.null(region)){
    mapParams <- rworldmap::mapCountryData(sPDF, 
                                nameColumnToPlot="value", 
                                colourPalette=colourPalette, 
                                catMethod=legendcat*scale, 
                                mapTit="",
                                missingCoun=missingCol,
                                ocean=ocean,
                                addLegend=FALSE)
  }
  if(!is.null(region)){
    mapParams <- rworldmap::mapCountryData(sPDF, 
                              nameColumnToPlot="value", 
                              colourPalette=colourPalette, 
                              catMethod=c(0,2,4,6,8,10)*scale, 
                              mapTit="",
                              mapReg=region,
                              missingCoun=grey(.95),
                              ocean=ocean,
                              addLegend=FALSE)
  }
  
  if(legend==TRUE){
    do.call( rworldmap::addMapLegend, c( mapParams
                            , legendLabels="all"
                            , legendWidth=0.5 ))  
  text(0,-120+legend2adjust,"n country detections")
  }
#########################################
# add connections
if(connections==TRUE&sum(unlist(lapply(country,length))>1)>0){
  country<-lapply(country,rename.country)
  worldmap<-rworldmap::getMap(res="low")
  centroids<-rgeos::gCentroid(worldmap,byid=T)
  d<-as.data.frame(centroids)
  # connection matrix
  if(!is.list(country)){net<-future.apply::future_lapply(list(country),get.net)
    }else net<-future.apply::future_lapply(country,get.net)
  net<-do.call(rbind,net)
  # frequency of country connections
  connection<-paste(net[,1],net[,2],sep=" - ")
  tab<-table(connection)
  # as matrix
  mat<-matrix(unlist(strsplit(names(tab)," - ")),ncol=2,byrow=T)
  mat<-cbind(mat,as.numeric(tab))
  linewidth<-round(((as.numeric(mat[,3])-min(as.numeric(mat[,3])))/max(as.numeric(mat[,3])))*8+1.5,2)/2
  range(linewidth)
  # color of lines
  a<-grDevices::colorRampPalette(c("lightyellow","blue"))
  steps<-max(ceiling(linewidth))
  hexline<-a(steps+1)[-1]
  # add 75% alpha channel (BF)/50% (80)
  hexline<-paste(hexline,"80",sep="")
  linecol<-hexline[round(linewidth)]
  # prepare mat
  mat<-matrix(mat[,-4:-5],ncol=3)
  mat<-cbind(mat,linewidth,linecol)
  #mat<-mat[order(mat[,4]),]
  for(i in 1:length(mat[,1])){
    points(geosphere::gcIntermediate(d[grep(paste("^",mat[i,1],"$",sep=""),rownames(d)),],d[grep(paste("^",mat[i,2],"$",sep=""),rownames(d)),],100),
           type="l",col=mat[i,"linecol"],lwd=mat[i,"linewidth"])
  }

# add legend
 if(legend==TRUE){
    if(is.null(dim(mat[,3]))) text<-"no connections"
    if(max(as.numeric(mat[,3]))==1) text<-"one connection"
    if(max(as.numeric(mat[,3]))==2) text<-c("one connection","two connections")
    if(max(as.numeric(mat[,3]))==3) text<-paste(1:3,"connect.")
    legend(0,110+legend1adjust/2,grep("^1 cons",unique(c("1 con",paste(round(ceiling(max(as.numeric(mat[,3])))/2),"cons"),paste(max(as.numeric(mat[,3])),"cons"))),invert=TRUE,value=TRUE),
       horiz=TRUE,xjust=.5,col=unique(hexline),bg=ocean,bty="n",title="n country connections",
       lwd=unique(c(min(mat[,"linewidth"]),ceiling(max(as.numeric(mat[,"linewidth"]))/2),max(mat[,"linewidth"]))))
    text(0,120+legend1adjust*.8,main,font=2,cex=1.2)
#    text(0,120+legend1adjust,"n connections")
  } # end add legend
  }else{
    # add title if no connections
    text(0,115+legend1adjust*.8,main,font=2,cex=1.2)
    if(connections==TRUE) legend(0,110+legend1adjust/2,"no country connections",horiz=TRUE,xjust=.5,bty="n")
  } # end add connections
  # output frequency table of detected countries
    if(connections==TRUE) return(list(countryFreq=sort(table(unlist(country))),connectionFreq=sort(tab)))
    if(connections!=TRUE) return(list(countryFreq=sort(table(unlist(country)))))
}


#####################################################
# for centroids
rename.country<-function(x){
  x<-gsub("S\\. Korea","South Korea",x)
  x<-gsub("Tanzania","United Republic of Tanzania",x)
  x<-gsub("USA|United States|United States of America","United States of America",x)
  x<-gsub("Bosnia and Herz\\.","Bosnia and Herzegovina",x)
  x<-gsub("Czech Rep\\.","Czech Republic",x)
  x<-gsub("Hong Kong","Hong Kong S\\.A\\.R\\.",x)
  x<-gsub("Serbia","Republic of Serbia",x)
  x<-gsub("Dominican Rep\\.","Dominican Republic",x)
  x<-gsub("Eq\\. Guinea","Equatorial Guinea",x)
  x<-gsub("Fr\\. Polynesia","French Polynesia",x)
  x<-gsub("Bahamas","The Bahamas",x)
  x<-gsub("Saint-Maarten","Sint Maarten",x)
  x<-gsub("Macau","Macau S\\.A\\.R\\",x)
  x<-gsub("Micronesia","Federated States of Micronesia",x)
  return(x)
}

# create connection matrix
get.net<-function(x){
  if(is.character(x)&!is.null(x)){
    if(length(x)>1){
      n<-NULL; x<-sort(x)
      for(i in 1:(length(x)-1)){
        for(j in (i+1):length(x)){
          n<-cbind(rbind(n,c(x[i],x[j])))
        }}
      return(n)
    }}
}

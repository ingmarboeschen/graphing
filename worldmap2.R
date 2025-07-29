#require(countrycode)
#require(rworldmap)
#require(geosphere)
#require(sf)
#library(RColorBrewer)

#' worldmap2
#' @param country a list with vectors or a frequency table of country names
#' @param connections an optional vector with connections seperated with " - " (e.g.: "Germany - USA")
#' @param reduce2country character. reduces connections to those of one specific country 
#' @param connect logical. If TRUE draws country connections
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
#' c("Germany","Cuba","Bolivia","South Africa")),reduce2country="USA",
#'           #connections=list("NA"," - ",c("Germany - USA","Germany - Peru"),"Germany - USA","Germany - Austria","Germany - USA","Germany - USA"),legend=T,xpd=F,
#'           legend1=-10,legend2=-15)

worldmap2<-function(country,
                    connect=TRUE,
                    connections=NULL,
                    reduce2country=NULL,
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
  temp<-FALSE
  if(is.table(country)&!is.table(connections)) connect<-FALSE
  # set graphic parameters
  par(mar=mar,mai=mai,xpd=xpd,bg=ifelse(xpd,ocean,"white"))
# for vector and list input
  if(!is.table(country)){
    # if is vector convert to list
    if(is.vector(country)) country<-as.list(country) 
    # if country is empty
    if(sum(unlist(lapply(country,length)))==0) country=""
    # recode countries
    country<-lapply(country,function(x) gsub("S. Korea","South Korea",x))
    country<-lapply(country,function(x) gsub("N. Korea","North Korea",x))
    country<-lapply(country,function(x) gsub("Czech Rep\\.","Czech Republic",x))
    country<-lapply(country,function(x) gsub("Sint Maarten","Saint-Martin",x))
    country<-lapply(country,function(x) gsub("^Micronesia$","Federated States of Micronesia",x))
    country<-lapply(country,function(x) gsub("Bosnia and Herz\\.","Bosnia and Herzegovina",x))
    country<-lapply(country,function(x) gsub("Dom[ei]nican Rep\\.","Dominican Republic",x))
    country<-lapply(country,function(x) gsub("Eq\\. Guinea","Equatorial Guinea",x))
    country<-lapply(country,function(x) gsub("Fr\\. Polynesia","French Polynesia",x))
    country<-lapply(country,function(x) gsub("GAZA","Gaza Strip",x))
  
    # frequency table of countries
    data<-data.frame(table(unlist(country)))
    colnames(data)<-c("country","value")
  }
  if(is.table(country)){
    names(country)<-gsub("S. Korea","South Korea",names(country))
    names(country)<-gsub("N. Korea","North Korea",names(country))
    names(country)<-gsub("Czech Rep\\.","Czech Republic",names(country))
    names(country)<-gsub("Sint Maarten","Saint-Martin",names(country))
    names(country)<-gsub("^Micronesia$","Federated States of Micronesia",names(country))
    names(country)<-gsub("Bosnia and Herz\\.","Bosnia and Herzegovina",names(country))
    names(country)<-gsub("Dom[ei]nican Rep\\.","Dominican Republic",names(country))
    names(country)<-gsub("Eq\\. Guinea","Equatorial Guinea",names(country))
    names(country)<-gsub("Fr\\. Polynesia","French Polynesia",names(country))
    names(country)<-gsub("GAZA","Gaza Strip",names(country))
    
    # frequency table of countries
    data<-data.frame(country)
    colnames(data)<-c("country","value")
    }
  
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
if(connect==TRUE&!is.null(connections)){
  # prepare
  worldmap<-rworldmap::getMap(res="low")
  # with sf package
  world_sf <- sf::st_as_sf(worldmap)
  world_sf_valid <- sf::st_make_valid(world_sf)
  centroids_sf <- suppressWarnings(sf::st_point_on_surface(world_sf_valid))
  centroids_coords <- sf::st_coordinates(centroids_sf)
  attributes <- world_sf_valid[, c("NAME")]
  coords <- sf::st_coordinates(centroids_sf)
  d <- data.frame(lon = coords[, 1],  lat = coords[, 2])
  land<-as.character(attributes$NAME)
  land[is.na(land)]<-"NA"
  rownames(d)<- rename.country(land)
  
  # with rgeos
#  centroids<-rgeos::gCentroid(worldmap,byid=T)
#    d<-as.data.frame(centroids)

  # get country connections if country is not a table and connections not set
  if(is.null(connections)&!is.table(country)){
    country<-lapply(country,rename.country)
    # connection matrix
    if(!is.list(country)){net<-future.apply::future_lapply(list(country),get.net)
      }else net<-future.apply::future_lapply(country,get.net)
    net<-do.call(rbind,net)
    connection<-paste(net[,1],net[,2],sep=" - ")
    # frequency of country connections
    tab<-table(connection)
  }else{
    if(!is.table(connections)){
    connection<-unlist(lapply(connections,rename.country))
    # frequency of country connections
    tab<-table(connection)
    }
    if(is.table(connections)){
      names(connections)<-rename.country(names(connections))
      tab<-connections
    }
  }
    tab<-tab[grep("[A-Z]",names(tab))]
    tab<-tab[grep("^NA$",names(tab),invert=TRUE)]
    # reduce to one country involvements
    if(length(grep("[A-Z]",reduce2country))>0)  tab<-tab[grep(rename.country(reduce2country[1]),names(tab))]
    # prepare objects
    linewidth<-1.2
  a<-grDevices::colorRampPalette(c("lightyellow","blue"))
  steps<-max(ceiling(linewidth))
  hexline<-a(steps+1)[-1]
  # add 75% alpha channel (BF)/50% (80)
  hexline<-paste(hexline,"80",sep="")
  linecol<-hexline[round(linewidth)]
  
  if(length(tab)>0){
  # as matrix
  mat<-matrix(unlist(strsplit(names(tab)," - ")),ncol=2,byrow=T)
  mat<-cbind(mat,as.numeric(tab))
  linewidth<-round(((as.numeric(mat[,3])-min(as.numeric(mat[,3])))/max(as.numeric(mat[,3])))*8+1.5,2)/2
  if(length(linewidth)==1) linewidth=1.2
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
   # 0 connections 
    if(is.null(dim(mat[,])[1])) text<-"no connections"
    # 1 connection strength
    if(length(unique(mat[,3]))==1){
      text<-paste(max(as.numeric(mat[,3])),"connections")
      text<-gsub("^(1 connection)s","\\1",text)
    }
    # 2 connection strengths
    if(length(unique(mat[,3]))==2){
      text<-paste(c(min(as.numeric(mat[,3])),max(as.numeric(mat[,3]))),"connections")
      text<-gsub("^(1 connection)s","\\1",text)
    }
    # 3 or more connection strengths
    if(length(unique(mat[,3]))>=3){
      text<-paste(
        format(c(1,ceiling(max(as.numeric(mat[,3]))/2),max(as.numeric(mat[,3]))),big.mark=";",trim=TRUE)
        ,"connections")
      text<-gsub("^(1 connection)s","\\1",text)
    }

    # add legend
    legend(0,110+legend1adjust/2,text,
       horiz=TRUE,xjust=.5,col=unique(hexline),bg=ocean,bty="n",
       title="n country connections",
       lwd=unique(c(min(mat[,"linewidth"]),ceiling(max(as.numeric(mat[,"linewidth"]))/2),max(mat[,"linewidth"]))))
    text(0,120+legend1adjust*.8,main,font=2,cex=1.2)
    temp<-TRUE
#    text(0,120+legend1adjust,"n connections")
  } # end add legend
  }else{
  if(temp==FALSE){
       # add title if no connections
       text(0,115+legend1adjust*.8,main,font=2,cex=1.2)
       if(connect==TRUE) legend(0,110+legend1adjust/2,paste("no connections with",reduce2country),horiz=TRUE,xjust=.5,bty="n")
       temp<-TRUE
  }}
    }else{
      if(temp==FALSE){
        # add title if no connections
        text(0,115+legend1adjust*.8,main,font=2,cex=1.2)
       if(connect==TRUE) legend(0,110+legend1adjust/2,"no country connections",horiz=TRUE,xjust=.5,bty="n")
      }
  } # end add connections
  
  # output frequency table of detected countries
  if(is.table(tab)&is.table(country)) return(list(countryFreq=sort(country),connectionFreq=sort(tab)))
  if(connect==TRUE&!is.table(country)) return(list(countryFreq=sort(table(unlist(country))),connectionFreq=sort(tab)))
  if(connect!=TRUE&!is.table(country)) return(list(countryFreq=sort(table(unlist(country)))))
  if(connect!=TRUE&is.table(country)) return(list(countryFreq=sort(country)))
  
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

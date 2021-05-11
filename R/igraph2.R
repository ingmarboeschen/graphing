#' igraph2
#'
#' draw network graph from list input
#' @param x a list with connected elements as vector 
#' @param freq logical. If TRUE displays frequency of connections in edge labels
#' @param label.cex numerical. Node label size
#' @param vertex.size numerical. Size of verices
#' @param split2words logical. If TRUE each element is further split at spaces
#' @param rm.punctuation logical. If TRUE punctuation/spaces are removed
#' @param lowerize logical. If TRUE lowerize to unify elements
#' @param stemming logical. If TRUE enables stemming
#' @param capitalize logical. If TRUE capitalize to display
#' @param rm.stopwords logical. If TRUE uses stop word removal
#' @param rm.numbers logical. IF TRUE excludes numbers
#' @param tkplot logical. If TRUE enables interactive adjustment with tkplot
#' @param seed numerical. Set seed for reproducible graphs
#' @param min.n numerical. Absolute minimum of involvement per element
#' @param min.freq numerical. Relative minimum of involvement per element
#' @param max.freq numerical. Relative maximum of involvement per element
#' @param layout numerical. Select out of:
#' 1: layout.fruchterman.reingold(g)
#' 2: layout.random(g)
#' 3: layout.kamada.kawai(g)
#' 4: layout.circle(g)
#' 5: layout.reingold.tilford(g)
#' 6: layout1 <- layout.sphere(g)
#' @export
#' @example 
#' x<-list(
#'      c("Laura","Ingmar"),
#'      c("Peter","Renate","Ingmar","Andrea"),
#'      c("Nassim","Ingmar","Sergej"),
#'      c("Laura","Rike","Andra"),
#'      c("Marlene","Nassim","Christina","Sabine"),
#'      c("Bela","Ingmar","Mariola","Nassim"),
#'      c("Gloria","Kim","Olek","Bolek"))
#' igraph2(x,seed=2)
igraph2<-function(x,
                  freq=FALSE, # display frequency of connections in edge labels
                  label.cex=1.7, # node label size
                  vertex.size=5, # 
                  split2words=FALSE, # each element is split at spaces
                  rm.punctuation=FALSE, # space is a punctuation
                  lowerize=FALSE, # lowerize to unify
                  stemming=FALSE, # enable stemming
                  capitalize=FALSE, # capitalize to display
                  rm.stopwords=FALSE, # use stop word removal
                  rm.numbers=FALSE, # exclude numbers
                  tkplot=FALSE, # enable interactive adjustment with tkplot
                  seed=NULL, # for reproducible results
                  min.n=1, # absolute minimum of involvement per element
                  min.freq=0, # relative minimum of involvement per element
                  max.freq=1, # relative maximum of involvement per element
                  layout=1 # select from:
){
# split at ' ,| '
  options(warn = -1)
  if(split2words==TRUE){ 
    x<-lapply(x,function(x) unlist(strsplit(x,"[-,\\.\\)] |[\\)\\]]")))
    x<-lapply(x,function(x) unlist(strsplit(x," ")))
  }
  if(split2words==FALSE){ 
    x<-lapply(x,function(x) gsub(" ","_",x))
  }
  # remove emty cells
  x<-lapply(x,function(x) x[nchar(x)>1]) 
  # tolower
  if(lowerize==TRUE) x<-lapply(x,tolower) 
  # create data frame and corpus
  docs<-data.frame(text=unlist(lapply(x,paste,collapse=" ")))
  corpus <- tm::Corpus(tm::VectorSource(docs$text))
  # clean up white spaces
#  corpus =  tm::tm_map(corpus,stripWhitespace)
  #create TDM
  tdm <-tm::TermDocumentMatrix(corpus,control = list(
                                    stripWhitespace=TRUE,
                                    tolower=FALSE,
                                    removeNumbers=rm.numbers,
                                    removePunctuation = rm.punctuation,
                                    stopwords = rm.stopwords,
                                    stemDocument=stemming))
  tdm.matrix <- as.matrix(tdm)
  # remove overly frequent words
  if(max.freq<1&length(dim(tdm.matrix))==2) tdm.matrix<-tdm.matrix[rowSums(tdm.matrix)/dim(tdm.matrix)[2]<=max.freq,]
  if(dim(tdm.matrix)[1]==0) stop("There is no elements left. Choose a different setting.")
  # remove uninformative words
  if(min.freq>0&length(dim(tdm.matrix))==2) tdm.matrix<-tdm.matrix[rowSums(tdm.matrix)/dim(tdm.matrix)[2]>=min.freq,]
  if(dim(tdm.matrix)[1]==0) stop("There is no elements left. Choose a different setting.")
  if(min.n>1&length(dim(tdm.matrix))==2) tdm.matrix<-tdm.matrix[rowSums(tdm.matrix)>=min.n,]
  if(dim(tdm.matrix)[1]==0) stop("There is no elements left. Choose a different setting.")
  # binary TDM
  tdm.matrix[tdm.matrix>=1] <- 1
  # coinciding term
  tdm.matrix <- tdm.matrix %*% t(tdm.matrix)
rownames(tdm.matrix)<-gsub("_"," ",rownames(tdm.matrix))
colnames(tdm.matrix)<-gsub("_"," ",colnames(tdm.matrix))
# capitalize initial letters
if(capitalize==TRUE) rownames(tdm.matrix)<-gsub("^([[:lower:]])|([- ][[:lower:]])",toupper("\\U\\1\\2"),perl=T,rownames(tdm.matrix))
if(capitalize==TRUE) colnames(tdm.matrix)<-gsub("^([[:lower:]])|([- ][[:lower:]])",toupper("\\U\\1\\2"),perl=T,colnames(tdm.matrix))
# build a graph from the above matrix
  g <- igraph::graph.adjacency(tdm.matrix , weighted=T, mode= "undirected")
  # remove loops
  g <- igraph::simplify(g)
  # set labels and degrees of vertices
  igraph::V(g)$label <- igraph::V(g)$name
  igraph::V(g)$degree <- igraph::degree(g)
  igraph::V(g)$color<-rep("lightblue",length(igraph::V(g)))
  igraph::V(g)$label.cex <- label.cex * igraph::V(g)$degree/max(igraph::V(g)$degree)+.7
 # V(g)$vertex.size<- vertex.size * V(g)$degree/max(V(g)$degree)
  node.size<-setNames(vertex.size*(igraph::V(g)$degree/max(igraph::V(g)$degree)+.1),igraph::V(g)$label)
  igraph::V(g)$vertex.size<- node.size
  
  igraph::V(g)$label.color <- rgb(0, 0, .5, 1)
  igraph::V(g)$frame.color <- NA
  # set labels and degrees of edges
if(freq==TRUE)  igraph::edge_attr(g, "label") <- as.character(unlist(igraph::edge_attr(g))) 
  #  egam <- (log(E(g)$weight+.4)) / max(log(E(g)$weight+.4))
  #  E(g)$color <- rgb(.5, .5, 0, egam)
  #  E(g)$width <- egam
  # set seed to make the layout reproducible
  if(!is.null(seed)) set.seed(seed)
  if(layout==1) layout1 <- igraph::layout.fruchterman.reingold(g)
  if(layout==2) layout1 <- igraph::layout.random(g)
  if(layout==3) layout1 <- igraph::layout.kamada.kawai(g)
  if(layout==4) layout1 <- igraph::layout.circle(g)
  if(layout==5) layout1 <- igraph::layout.reingold.tilford(g)
  if(layout==6) layout1 <- igraph::layout.sphere(g)

    # draw plot or enable interactive adjustment
    if(tkplot==TRUE){
      igraph::tkplot(g, layout=layout1,vertex.color=igraph::V(g)$color,vertex.size=node.size)
      }else{
        plot(g, layout=layout1,vertex.color=igraph::V(g)$color,vertex.size=node.size)
        }
}



#' orcid2name
#' 
#' Function to convert ORCiD to name with orcid.org api (needs a valid ORCID token for OAuth authentication)
#' @param x vector that contains a valid ORCID-address (e.g.: "https://orcid.org/0000-0003-1159-3991") and/or names.
#' @param useDB logical. IF TRUE a self curated list with ORCIDs from within the PMC database is beeing used before connecting to the API to gather still missing ORCIDs.
#' @param useDB logical. IF TRUE a self curated list with ORCIDs from within the PMC database is beeing used before connecting to the API to gather still missing ORCIDs.
#' @param api logical. IF TRUE the ORCID api is used to convert ORCiDs to names. Set to FALSE if no ORCiD authentification is possible. 
#' @export
#' @examples 
#' orcid2name(c("https://orcid.org/0000-0003-1159-3991","Einstein, Albert"))

#library(rorcid)
orcid2name<-function(x,useDB=TRUE,api=TRUE){
  x<-unlist(x)
  # correct some errors in ORCID-address
  x<-gsub("/$|;$","",x)
  x<-gsub("//","/",x)

  
  if(useDB==TRUE){
  x<-ORCID2nameDB(x) # function defintion on bottom of script
  }
  
## if still has ID or useDB==FALSE, convert with ORCID API
  if(api==TRUE){
  # get index
  index<-grep("[0-9a-zA-Z]{4}-[0-9a-zA-Z]{4}-[0-9a-zA-Z]{4}-[0-9a-zA-Z]{4}",x)
  if(length(index)>0){
  x<-gsub(".*orcid\\.org/","",x)
  x<-gsub("([0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{4}).*","\\1",x)
    for(i in 1:length(index)){
      tryCatch({
        o<-rorcid::orcid_id(orcid=x[index[i]])
        n<-o[[1]]["name"]
        given<-unname(unlist(n[[1]]["given-names"]))
        family<-unname(unlist(n[[1]]["family-name"]))
        out<-(paste0(family,", ",given))
        x[index[i]]<-out
      },error=function(e) x[index[i]])
    }}
  }
  return(x)
}


ORCID2nameDB<-function(x){
  output<-x
  # which have ORCiD
  hasORCiD<-grep("[0-9a-zA-Z]{4}-[0-9a-zA-Z]{4}-[0-9a-zA-Z]{4}-[0-9a-zA-Z]{4}",output)
  # if has ORCID
  if(length(hasORCiD)>0){
    # ORCiDs in input that are present in data base
    j<-which(is.element(gsub(".*rcid\\.org/","",output[hasORCiD]),graphing::ORCiD$ID))
    if(length(j)>0){
      # Index of these ORCIDS
      for(k in j) output[hasORCiD][k]<-graphing::ORCiD$Author[grep(gsub(".*rcid\\.org/","",output[hasORCiD][k]),graphing::ORCiD$ID,fixed=TRUE)]
      #        i<-grep(paste0(gsub(".*rcid\\.org/","",output[j]),collapse="|"),ORCiD$ID,fixed=F)
      # replace ORCID with name
      #        if(length(i)>0) output[j]<-ORCiD$Author[i]
    }
  }
  return(output)
}

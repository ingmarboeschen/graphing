#' orcid2name
#' 
#' function to convert ORCID to name with orcid.org api (needs valid ORCID token for OAuth authentication)
#' @param x vector that contains ORCID and/or names
#' @export
#' @examples 
#' orcid2name(x<-c("0000-0001-8594-9511","0000-0003-1159-3991","0000-0003-1159-3991"))

#library(rorcid)
orcid2name<-function(x){
  x<-unlist(x)
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
  return(x)
}

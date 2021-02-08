#' Convert an image sequence to a video or animated gif
#'
#' requires ffmpeg for video conversion: https://www.ffmpeg.org/
#' @param cex text size
#' @param folder folder with image sequence
#' @param i.format image format (png, jpeg, pdf only working for gif conversion)
#' @param container movie container: mp4, mov, avi, gif
#' @param fps frames per second used
#' @param last length of last frame in seconds
#' @param loop loop gif (0=True,1=False)
#' @export
#' @examples
#' x<-sample(paste("group",LETTERS[1:5]),200,T)
#' ani.barplot(prop.table(table(x)),folder="ani.barplot",format="png")
#' image2video(folder="./ani.barplot",cont="mov",i.f="png")

image2video<-function(
folder=getwd(), # folder with image sequence
i.format="png", # image format (png, jpeg, pdf only working for gif conversion)
container="mp4", # movie container: mp4, mov, avi, gif
fps=8, # frames per second used
last=2, # length of last frame in seconds
loop=0 # loop gif (0=True,1=False)
){
wd.folder<-getwd()
setwd(folder)
# message for pdf to movie
if(container!="gif"&i.format=="pdf") message("Sorry, pdf to video is not provided.\n A .gif animation is being created instead.")
# start message
message("The rendering process has started. Please be patient...")
# files to be converted
f<-list.files()[grep(paste(".",i.format,sep=""),list.files())]
# covert to gif
if(container=="gif"|i.format=="pdf") {
file<-list.files()[grep(paste(".",i.format,sep=""),list.files())]
 file<-rep(file[length(file)],last*fps)
 temp<-paste(file,100001:(100000+length(file)),".",i.format,sep="")
 file.copy(file,temp)
 system(paste("convert -delay ",fps," -loop ",loop," *",i.format," animation.gif",sep=""))
 file.remove(temp)
 }
# convert to movie
if(container!="gif"&i.format!="pdf"){
 files<-list.files()[grep(paste(".",i.format,sep=""),list.files())]
 files<-c(files,rep(files[length(files)],last*fps))
 temp<-paste("temp",100001:(100000+length(files)),".",i.format,sep="")
 file.copy(files,temp)
 system(paste("ffmpeg -r ",fps," -i temp1%05d.",i.format," -c:v libx264 -r 25 -pix_fmt yuv420p animation.",container,sep=""))
 file.remove(temp)
 }
# end message
if(length(f)>0) message("Rendering completed.")
# message for wrong specified image format/folder
if(!file.exists(folder)) message(paste("The specified folder '",folder,"' does not exist.",sep=""))
if(length(f)==0&file.exists(folder)) message(paste("There is no images in the specified format '",i.format,"' in folder '",folder,"'.\n No video was created.",sep="")) 

# setwd(wd.folder)

}

## Example
# for(i in 1:100){
# png(paste("Test",i+100,".png",sep=""),width=1920,height=1080,res=300,points=8)
# ani.barplot(1:4)
# dev.off()
# }

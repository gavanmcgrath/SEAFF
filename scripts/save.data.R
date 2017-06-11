save.data <- function(data,name){
  save(data,file=paste("./data/",name,".RData",sep=""))
}
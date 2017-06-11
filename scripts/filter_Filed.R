#' filterField  Low pass filtering of a matrix or an array along one dimension
#'
#' @param dat The array
#' @param filt the low pass filter, a vector of weights
#' @param tdim the dimension of dat to apply the filter to
#'
#' @return an array of dim(dat) low pass filtered
#'
#' @examples
#' dat <- array(NA, c(2,2,120))
#' for (i in 1:2){ for (j in 1:2) {  dat[i,j,] <- 3*cos(1:120*2*pi/12)}}
#' dat.filt <- filter_Field(dat,filt=rep(1,12),tdim=3)
#' 
filter_Field <- function(dat,filt=c(1/36,1/84),method=c("butter","ma","forecast")){

  tdim <- attributes(dat)$tdim
  sdim <- setdiff(1:length(dim(dat)),tdim)
  
  if (method=="butter") {
    require(signal)
    bf <- butter(2,W=c(min(filt),max(filt)))
  }
 
  dat.filt <- dat 
  if (method == "ma"){
       if (length(dim(dat))==3){
         poss <- expand.grid(1:dim(dat)[sdim[1]],1:dim(dat)[sdim[2]])
         
         if (tdim == 1){
           for (i in 1:dim(poss)[1]){
             dat.filt[,poss[i,1],poss[i,2]] <- c(stats::filter(dat[,poss[i,1],poss[i,2]],filter=filt,sides=1))
           }
         }
         if (tdim == 2){
           for (i in 1:dim(poss)[1]){
             dat.filt[poss[i,1],,poss[i,2]] <- c(stats::filter(dat[poss[i,1],,poss[i,2]],filter=filt,sides=1))
           }
         }
         if (tdim == 3){
           for (i in 1:dim(poss)[1]){
             dat.filt[poss[i,1],poss[i,2],] <- c(stats::filter(dat[poss[i,1],poss[i,2],],filter=filt,sides=1))
           }
         }
       } else {
         poss <- 1:dim(dat)[sdim[1]]
         if (tdim == 1){
           for (i in 1:length(poss)){
             dat.filt[,poss[i]] <- c(stats::filter(dat[,poss[i]],filter=filt,sides=1))
           }
         }
         if (tdim == 2){
           for (i in 1:length(poss)){
             dat.filt[poss[i],] <- c(stats::filter(dat[poss[i],],filter=filt,sides=1))
           }
         }
       }
     }
  
     if (method == "butter") {
  
       if (length(dim(dat))==3){
         poss <- expand.grid(1:dim(dat)[sdim[1]],1:dim(dat)[sdim[2]])
         
         if (tdim == 1){
           for (i in 1:dim(poss)[1]){
             dat.filt[,poss[i,1],poss[i,2]] <- c(signal::filter(bf,dat[,poss[i,1],poss[i,2]]))
           }
         }
         if (tdim == 2){
           for (i in 1:dim(poss)[1]){
             dat.filt[poss[i,1],,poss[i,2]] <- c(signal::filter(bf,dat[poss[i,1],,poss[i,2]]))
           }
         }
         if (tdim == 3){
           for (i in 1:dim(poss)[1]){
             dat.filt[poss[i,1],poss[i,2],] <- c(signal::filter(bf,dat[poss[i,1],poss[i,2],]))
           }
         }
       } else {
         poss <- 1:dim(dat)[sdim[1]]
         if (tdim == 1){
           for (i in 1:length(poss)){
             dat.filt[,poss[i]] <- c(signal::filter(bf,dat[,poss[i]]))
           }
         }
         if (tdim == 2){
           for (i in 1:length(poss)){
             dat.filt[poss[i],] <- c(signal::filter(bf,dat[poss[i],]))
           }
         }
       }
     }
  
     if (method == "forecast"){
       if (length(dim(dat))==3){
         poss <- expand.grid(1:dim(dat)[sdim[1]],1:dim(dat)[sdim[2]])
         
         if (tdim == 1){
           for (i in 1:dim(poss)[1]){
             dat.filt[,poss[i,1],poss[i,2]] <- c(forward.filter(dat[,poss[i,1],poss[i,2]],
                                                                filt=filt))
           }
         }
         if (tdim == 2){
           for (i in 1:dim(poss)[1]){
             dat.filt[poss[i,1],,poss[i,2]] <- c(forward.filter(dat[poss[i,1],,poss[i,2]],
                                                                filt=filt))
           }
         }
         if (tdim == 3){
           for (i in 1:dim(poss)[1]){
             dat.filt[poss[i,1],poss[i,2],] <- c(forward.filter(dat[poss[i,1],poss[i,2],],
                                                                filt=filt))
           }
         }
       } else {
         poss <- 1:dim(dat)[sdim[1]]
         if (tdim == 1){
           for (i in 1:length(poss)){
             dat.filt[,poss[i]] <- c(forward.filter(dat[,poss[i]],filt=filt))
           }
         }
         if (tdim == 2){
           for (i in 1:length(poss)){
             dat.filt[poss[i],] <- c(forward.filter(dat[poss[i],],filt=filt))
           }
         }
       }
     }
  pos=c()
  if (length(dim(dat))==2){
    if (tdim == 1) {
      pos <- is.na(dat.filt[,1])
      if (length(pos)>0) {
        dat.filt <- dat.filt[which(!pos),]
      }
    }
    if (tdim == 2) {
      pos <- is.na(dat.filt[1,])
      if (length(pos)>0) {
        dat.filt <- dat.filt[,which(!pos)]
      }
    }
  } else {
    print ("Need to edit filterFiled.R for length(dim(field)) = 3")
  }
  dat.filt <- transfer.attributes(dat,dat.filt)
  if (length(pos>0)) {
    attr(dat.filt,"time") <- attr(dat.filt,"time")[which(!pos),]
    attr(dat.filt,"date") <- attr(dat.filt,"date")[which(!pos)]
  }
  
  
  return(dat.filt)
}

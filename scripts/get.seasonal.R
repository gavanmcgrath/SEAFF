#' Get Seasonal mean
#'
#' @param dat 
#' @param mons
#'
#' @return
#' @export
#'
#' @examples
get.seasonal <- function(dat,mons=c(12,1,2),sequential=FALSE){
  require(parallel)
  #data with time on rows, space on columns
  #mons a numeric vector of months assumed sequential
  # mons = c(12,1,2) corresponds to DJF
  # mons = c(1,2,3) corresponds to JFM
  
  #If sequental then if mons = c(1,4,7,10)
  # gives 3 month seasonal ts i.e. JFM AMJ JAS OND
  #if mons =c(1,5,9) gives JFMA MJJA SOND
  
  tdim <- attributes(dat)$tdim
  sdim <- setdiff(1:length(dim(dat)),tdim)
  
  dat.mon <- as.numeric(format(attributes(dat)$date,"%m"))
  nt <- length(dat.mon)
  nm <- length(mons)
  pos.start.mon <- which(dat.mon == mons[1])
  
  if (!sequential){
    #effectively produces an annual time series of season x
    seq.mons <- list()
    for (i in 1:length(pos.start.mon)){
      indx <- seq(pos.start.mon[i],by=1,length.out = nm)
      if (max(indx) < nt){
        seq.mons[[i]] <- indx
      }  
    }
  } else {
    #produces a 2, 3,4, or 6 monthly time series
    #in this case mon should represent start months and its length the number of "seasons" per year
    ln <- abs(max(diff(mons)))
    seq.mons <- list()
    cntr <- 1
    for (i in 1:length(pos.start.mon)){
      for (j in 1:nm){
        indx <- seq(pos.start.mon[i]+(j-1)*ln,by=1,length.out = ln)
        if(max(indx) < nt){
          seq.mons[[cntr]] <- indx
          cntr <- cntr + 1
        }
      }
    }
  }
    
  
  cl <- makeCluster(4)
  clusterExport(cl, "seq.mons",envir=environment())
  
  seasonal.dat <- parApply(cl=cl,dat, MARGIN=sdim,
                    FUN=function(x) {
                      unlist(lapply(seq.mons,
                        FUN=function(y) {
                          ifelse((sum(!is.na(x[y])) > 0),mean(x[y], na.rm=TRUE),NA)
                        }
                      ))
                    })
  seasonal.dat[is.nan(seasonal.dat)] <- NA
    
  if (length(dim(dat)) == 3) {
      if (tdim == 2){
        seasonal.dat <- aperm(seasonal.dat,c(2,1,3))
      }
      if (tdim == 3) {
        seasonal.dat <- aperm(seasonal.dat,c(2,3,1))
      }
  }
    
  if (length(dim(dat)) == 2) {
      if (tdim == 2){
        seasonal.dat <- t(seasonal.dat)
      }
  }
  
  stopCluster(cl)
    
    seasonal.dat <- transfer.attributes(dat,seasonal.dat)
    #assign last date of each "season" as the time stamp 
    pos <- unlist(lapply(seq.mons,FUN=function(x) x[length(x)]))
    attr(seasonal.dat,"date") <-  attr(dat,"date")[pos]
    attr(seasonal.dat,"time") <-  attr(dat,"time")[pos,]
    attr(seasonal.dat,"season") <- paste(c("J","F","M","A","M","J","J","A","S","O","N","D")[mons],collapse="")
    
    return(seasonal.dat)
}

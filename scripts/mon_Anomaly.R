#' mnthly_Anom  Calculate monthly anomalies of a 3D array 
#'
#' @param dat  the 3D numeric array
#' @param sdMethod character string determining whether standardization should be performed and how. If "none" anomaly not divided by sd. If "mon" then a monthly standard deviation is determined and standardization perfomed on a monthly basis. If "all" then the standard deviation of the whole of each time series is used. If "global" a global sd is used.
#' @param anomPeriodA charachter or numeric vector of length 2 containing the year range to calculate anomalies relative to
#'
#' @return an Array of dim(dat) of anomaly (zero mean) values
#'
#' @examples
#' dat <- array(NA, c(2,2,36))
#' for (i in 1:2){ for (j in 1:2) {  dat[i,j,] <- 3*cos(1:36*2*pi/12)}}
#' dat.anom <- mnthly_Anom(dat,tdim=3)
#' 

mnthly_Anom <- function(dat,sdMethod = "none", anomPeriod=NULL,
                        save.mus =  FALSE,
                        save.mus.name= NULL){
  
  #Function generates a monthly anomaly of a field
  
  tdim <- attributes(dat)$tdim
  n <- dim(dat)[tdim]
  sdim <- setdiff(1:length(dim(dat)),tdim)
  dat.dates <- attributes(dat)$date
  
  if (!is.null(anomPeriod)) {
    anom.pos <- which(dat.dates >= paste(anomPeriod[1],"-01-01",sep="") & 
                      dat.dates <= paste(anomPeriod[2], "-12-01",sep=""))
  } else {
    anom.pos<- 1:n
  }
  
  n.unique.mons <- length(unique(format(dat.dates,"%b")))
  
  m.lst <- list()
  for (i in 1:n.unique.mons) {
    m.lst[[i]] <- seq(i,n,by= n.unique.mons)
  }
  ndx <- lapply(m.lst, FUN=function(y) {y[which(y %in% anom.pos)]})
  ndx.unlisted <- unlist(ndx)
  ndx.sort.pos <- sort(ndx.unlisted,index.return=TRUE)$ix
  ndx.all.unlisted <- unlist(m.lst)
  ndx.all.sort.pos <- sort(ndx.all.unlisted,index.return=TRUE)$ix

  calc.anom <- function(x){
    res <- unlist(lapply(1:n.unique.mons,FUN=
                    function(i){
                      y <- ndx[[i]]
                      mn <- mean(x[y],na.rm=TRUE)
                      sdx <- ifelse(sdMethod == "mon",sd(x[y],na.rm=TRUE),1)
                      (x[m.lst[[i]]] - mn)/sdx
                    }))[ndx.all.sort.pos]
    sdx <- ifelse(sdMethod == "all",sd(res,na.rm=TRUE),1)
    res/sdx
  }
  
  calc.mu <- function(x){
    res <- unlist(lapply(1:n.unique.mons,FUN=
                           function(i){
                             y <- ndx[[i]]
                             mn <- mean(x[y],na.rm=TRUE)
                             x[m.lst[[i]]] <- rep(mn,length(m.lst[[i]]))
                           }))[ndx.all.sort.pos]
  }
  
  calc.sd <- function(x){
    if (sdMethod == "mon") {
      res <- unlist(lapply(1:n.unique.mons,FUN=
                           function(i){
                             y <- ndx[[i]]
                             sdx <- sd(x[y],na.rm=TRUE)
                             x[m.lst[[i]]] <- rep(sdx,length(m.lst[[i]]))
                           }))[ndx.all.sort.pos]
    }
    if (sdMethod == "all") {
      res <- rep(sd(x,na.rm=TRUE),length(x))
    }
    res
  }
  
  
  d.anom <- dat
  if (save.mus) {
    d.mu <- dat
    d.sd <- dat
  }
  if (length(dim(dat))==3){
    poss <- expand.grid(1:dim(dat)[sdim[1]],1:dim(dat)[sdim[2]])
    
    if (tdim == 1){
      for (i in 1:dim(poss)[1]){
        d.anom[,poss[i,1],poss[i,2]] <- calc.anom(dat[,poss[i,1],poss[i,2]])
        if (save.mus) {
          d.mu[,poss[i,1],poss[i,2]] <- calc.mu(dat[,poss[i,1],poss[i,2]])
          d.sd[,poss[i,1],poss[i,2]] <- calc.sd(dat[,poss[i,1],poss[i,2]])
        }
      }
    }
    if (tdim == 2){
      for (i in 1:dim(poss)[1]){
        d.anom[poss[i,1],,poss[i,2]] <- calc.anom(dat[poss[i,1],,poss[i,2]])
        if (save.mus) {
          d.mu[poss[i,1],,poss[i,2]] <- calc.mu(dat[poss[i,1],,poss[i,2]])
          d.sd[poss[i,1],,poss[i,2]] <- calc.sd(dat[poss[i,1],,poss[i,2]])
        }
      }
    }
    if (tdim == 3){
      for (i in 1:dim(poss)[1]){
        d.anom[poss[i,1],poss[i,2],] <- calc.anom(dat[poss[i,1],poss[i,2],])
        if (save.mus) {
          d.mu[poss[i,1],poss[i,2],] <- calc.mu(dat[poss[i,1],poss[i,2],])
          d.sd[poss[i,1],poss[i,2],] <- calc.sd(dat[poss[i,1],poss[i,2],])
        }
      }
    }
  } else {
    poss <- 1:dim(dat)[sdim[1]]
    if (tdim == 1){
      for (i in 1:length(poss)){
        d.anom[,poss[i]] <- calc.anom(dat[,poss[i]])
        if (save.mus) {
          d.mu[,poss[i]] <- calc.mu(dat[,poss[i]])
          d.sd[,poss[i]] <- calc.sd(dat[,poss[i]])
        }
      }
    }
    if (tdim == 2){
      for (i in 1:length(poss)){
        d.anom[poss[i],] <- calc.anom(dat[poss[i],])
        if (save.mus) {
          d.mu[poss[i],] <- calc.mu(dat[poss[i],])
          d.sd[poss[i],] <- calc.sd(dat[poss[i],])
        }
      }
    }
  }
  
  if(sdMethod == "global"){
    d.anom <- d.anom/sd(c(d.anom),na.rm=TRUE)
    if (save.mus) {
      d.sd[] <- sd(c(d.anom),na.rm=TRUE)
    }
  }
  
  gc()
  wh.nan <- which(is.nan(d.anom))
  for (i in wh.nan) d.anom[i] <- 0
  wh.nan <- which(abs(d.anom) == Inf)
  d.anom[wh.nan] <- 0
  
  d.anom <- transfer.attributes(dat,d.anom)
  if (save.mus) {
    d.mu <- transfer.attributes(dat,d.mu)
    d.sd <- transfer.attributes(dat,d.sd)
    musd <- list(mus = d.mu,sds=d.sd)
    rm(d.mu,d.sd)
    gc()
    saveRDS(musd,file= save.mus.name)
  }
  
  return(d.anom)
}
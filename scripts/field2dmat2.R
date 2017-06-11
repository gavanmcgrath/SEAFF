#' Title field2dmat2  Modified  field2dmat to extract two regions the second field could be lagged by x time periods
#'
#' @param field 
#' @param years 
#' @param months 
#' @param xlim 
#' @param ylim 
#' @param lag 
#'
#' @return
#' @export
#'
#' @examples
field2dmat2 <- 
  function (field, start.date=NULL, end.date = NULL,
            years = "all", months = "all", xlim = "all", 
            ylim = "all",lag = 0) {
    
    xs <- attributes(field)$lon
    ys <- attributes(field)$lat
    tims <- attributes(field)$time
    dates <- attributes(field)$date
    
    #temporal filter
    mons <- tims[,2]
    if (months[1] == "all") {
      i.mons <- rep(TRUE, length = length(mons))
    } else {
      i.mons <- sapply(mons, FUN = function(a) {
        a %in% months
      })
    }
    
    yeas <- tims[,1]
    if (years[1] == "all") {
      i.yeas <- rep(TRUE, length = length(yeas))
    } else {
      i.yeas <- sapply(yeas, FUN = function(a) {
        a %in% years
      })
    }
    i.tims <- i.mons & i.yeas
    if (!is.null(start.date)){
      if (is.null(end.date)) stop("Must specify end.date as a %Y-%m-%d string or as a Date object")
      if (class(start.date) == "character") {start.date <- as.Date(start.date,format="%Y-%m-%d") }
      if (class(end.date) == "character") {end.date <- as.Date(end.date,format="%Y-%m-%d") }
      which.indates <- dates %in% seq(from=start.date,to=end.date,by="month") 
      i.tims <- i.tims & which.indates
    }
    
    #Spatial filter on rectangle(s) with lags
    xys <- expand.grid(xs,ys)
    
    if (xlim[1] == "all" & ylim[1] == "all") {
      i.s <- rep(TRUE, length = length(xys[,1]))
      #can only be one lag (i.e. an offset for the whole field)
      dmat <- matrix(NA,nrow=length(i.s),ncol=length(i.tims))
      p.tims <- which(i.tims)
      for (j in 1:length(p.tims)){
        dmat[,j] <- c(field[,,p.tims[j]-lag[1]])
      }
    }
    
    if (xlim[1] != "all" & ylim[1] == "all") {
        xlim <- matrix(xlim,ncol=2)
        i.s <- rep(FALSE, length = length(xys[,1]))
        dmat <- matrix(NA,nrow=length(i.s),ncol=length(i.tims))
        p.tims <- which(i.tims)
        for (j in 1:nrow(xlim)){
          i.xs2 <- (xs <= xlim[j,2]) & (xs >= xlim[j,1])
          i.s2 <- xys[,1] %in% xs[i.xs2]
          for (i in 1:length(p.tims)){
            dmat[i.s2,i] <- c(field[,,p.tims[i]-lag[j]][i.s2])
          }
          i.s <- i.s | i.s2
        }
        
    }
    
    if (xlim[1] == "all" & ylim[1] != "all") {
      ylim <- matrix(ylim,ncol=2)
      i.s <- rep(FALSE, length = length(xys[,1]))
      dmat <- matrix(NA,nrow=length(i.s),ncol=length(i.tims))
      p.tims <- which(i.tims)
      for (j in 1:nrow(ylim)){
        i.ys2 <- (ys <= ylim[j,2]) & (ys >= ylim[j,1])
        i.s2 <- xys[,1] %in% ys[i.ys2]
        for (i in 1:length(p.tims)){
          dmat[i.s2,i] <- c(field[,,p.tims[i]-lag[j]][i.s2])
        }
        i.s <- i.s | i.s2
      }
    }
    
    if (xlim[1] != "all" & ylim[1] != "all") {
      xlim <- matrix(c(xlim),ncol=2)
      ylim <- matrix(c(ylim),ncol=2)
      
      i.s <- rep(FALSE, length = length(xys[,1]))
      dmat <- matrix(NA,nrow=length(i.s),ncol=length(i.tims))
     
      p.tims <- which(i.tims)
      for (j in 1:nrow(xlim)){
        i.xs2 <- (xys[,1] <= xlim[j,2]) & (xys[,1] >= xlim[j,1])
        i.ys2 <- (xys[,2] <= ylim[j,2]) & (xys[,2] >= ylim[j,1])
        i.s2 <- i.xs2 & i.ys2
        for (i in 1:length(p.tims)){
          dmat[i.s2,i] <- c(field[,,p.tims[i]-lag[j]][i.s2])
        }
        i.s <- i.s | i.s2
      }
    }
    ixys <- expand.grid(1:length(xs),1:length(ys))
    i.xs <- ixys[i.s,1]
    i.ys <- ixys[i.s,2]
   
    #select spatial points in the rectangle(s)
    dmat <- dmat[which(i.s),]
    
    #select spatial points with data
    isna <- apply(dmat, FUN = function(x) {(sum(is.na(x)) == length(x))},MARGIN = c(1))
    dmat <- dmat[!isna,]
    i.xs <- i.xs[!isna]
    i.ys <- i.ys[!isna]
    
    #select temporal points
    isna <- apply(dmat, FUN = function(x) (sum(is.na(x)) > 0),MARGIN = c(2))
    dmat <- dmat[,!isna]
    dmat <- t(dmat)
    
    dmat <- transfer.attributes(field,dmat)
    attr(dmat, "time") <- tims[i.tims,]
    attr(dmat, "date") <-  attr(dmat, "date")[i.tims]
    attr(dmat, "ixs") <- i.xs
    attr(dmat, "iys") <- i.ys
    attr(dmat,"tdim") <- 1
    attr(dmat,"type") <- "grid"
    
    dmat
  }


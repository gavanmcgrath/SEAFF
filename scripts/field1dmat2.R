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
field1dmat2 <- 
  function (field, start.date=NULL, end.date=NULL,
            years = "all", months = "all", xlim = "all", 
            ylim = "all",lag = 0) {
    
    xs <- attributes(field)$lon
    ys <- attributes(field)$lat
    tims <- attributes(field)$time
    dates <- attributes(field)$date
    site.ids <- attributes(field)$site.id
    
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
    if (lag[1] != 0){
      i.tims.1 <- i.tims.2 <- i.tims
      i.tims.2[which(i.tims.2)[1:lag] - lag] <- TRUE
      i.tims.2[which(i.tims.2)[length(which(i.tims.2)) - 0:(lag[1]-1)]] <- FALSE
    } else {
      i.tims.1 <- i.tims.2 <- i.tims
    }
    
    if (!is.null(start.date)){
      if (is.null(end.date)) stop("Must specify end.date as a %Y-%m-%d string or as a Date object")
      if (class(start.date) == "character") {start.date <- as.Date(start.date,format="%Y-%m-%d") }
      if (class(end.date) == "character") {end.date <- as.Date(end.date,format="%Y-%m-%d") }
      which.indates <- dates %in% seq(from=start.date,to=end.date,by="month") 
      i.tims <- i.tims & which.indates
    }
    p.tims <- which(i.tims)
    xys <- cbind(xs,ys)
    nt <- length(p.tims)
    nx <- length(xs)
    dmat <- matrix(NA,nrow = nx,ncol = nt)
    #print(dim(dmat))
    i.s <- rep(FALSE, length = nx)
    
    #Spatial filter on rectangle(s) with lags
    if (xlim[1] == "all" & ylim[1] == "all") {
      i.s <- !i.s
      #can only be one lag (i.e. an offset for the whole field)
      for (j in 1:nt){
        dmat[,j] <- c(field[p.tims[j]-lag[1],])
      }
    }
    
    if (xlim[1] != "all" & ylim[1] == "all") {
      xlim <- matrix(xlim,ncol=2)
      for (j in 1:nrow(xlim)){
        i.xs2 <- (xs <= xlim[j,2]) & (xs >= xlim[j,1])
        i.s2 <- xys[,1] %in% xs[i.xs2]
        for (i in 1:nt){
          dmat[i.s2,i] <- c(field[p.tims[i]-lag[j],][i.s2])
        }
        i.s <- i.s | i.s2
      }
    }
    
    if (xlim[1] == "all" & ylim[1] != "all") {
      ylim <- matrix(ylim,ncol=2)
      for (j in 1:nrow(ylim)){
        i.ys2 <- (ys <= ylim[j,2]) & (ys >= ylim[j,1])
        i.s2 <- xys[,2] %in% ys[i.ys2]
        print(paste("lag[j]",lag[j]))
        for (i in 1:nt){
          dmat[i.s2,i] <- c(field[p.tims[i]-lag[j],][i.s2])
        }
        i.s <- i.s | i.s2
      }
    }
    
    if (xlim[1] != "all" & ylim[1] != "all") {
      xlim <- matrix(xlim,ncol=2)
      ylim <- matrix(ylim,ncol=2)
      for (j in 1:nrow(xlim)){
        i.xs2 <- (xs <= xlim[j,2]) & (xs >= xlim[j,1])
        i.ys2 <- (ys <= ylim[j,2]) & (ys >= ylim[j,1])
        i.s2 <- xys[,1] %in% xs[i.xs2] & xys[,2] %in% ys[i.ys2]
        for (i in 1:nt){
          dmat[i.s2,i] <- c(field[p.tims[i]-lag[j],][i.s2])
        }
        i.s <- i.s | i.s2
      }
    }
    
    
    #select spatial points in the rectangle(s) and temporal points in the tbounds
    dmat <- dmat[i.s,]
    i.xs <- 1:length(xs)
    i.ys <- 1:length(ys)
    ixys <- cbind(i.xs,i.ys)
    i.xs <- ixys[i.s,1]
    i.ys <- ixys[i.s,2]
    site.ids <- site.ids[i.s]
    tims <- tims[p.tims,]
    dates <- dates[p.tims]
    
    #select spatial points with data
    isna <- apply(dmat, FUN = function(x) (sum(is.na(x)) == length(x)),MARGIN = c(1))
    dmat <- dmat[!isna,]
    i.xs <- i.xs[!isna]
    i.ys <- i.ys[!isna]
    site.ids <- site.ids[!isna]
    
    #select temporal points with complete data
    isna <- apply(dmat, FUN = function(x) (sum(is.na(x)) == length(x)),MARGIN = c(2))
    dmat <- dmat[,!isna]
    tims <- tims[!isna,]
    dates <- dates[!isna]
    
    #transpose and select spatial points with data complete data
    dmat <- t(dmat)
    ndx2 <- apply(dmat,MARGIN=2,FUN= function(x) sum(is.na(x)) == 0)
    dmat <- dmat[,ndx2]
    i.xs <- i.xs[ndx2]
    i.ys <- i.ys[ndx2]
    site.ids <- site.ids[ndx2]
    
	dmat <- transfer.attributes(field,dmat)
    attr(dmat, "time") <- tims  
    attr(dmat, "date") <- dates  
    attr(dmat, "ixs") <- i.xs
    attr(dmat, "iys") <- i.ys
    attr(dmat,"tdim") <- 1
    attr(dmat,"site.id") <- site.ids
    attr(dmat,"type") <- "points"
    
    dmat
  }


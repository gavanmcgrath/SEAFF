plot.pca.points <- 
  function (vvv, col = "cool2warm.colors", breaks, 
            nlevels = 15,xlim=NULL,ylim=NULL,newproj, 
            plotscale=FALSE,legend.pos=NULL,zero.center,
            fig=NULL,projectn) {
    
    require(sp)
    require(maps)
    require(mapdata)
    
    slon <- attr(vvv, "lon")[attr(vvv,"ixs")]
    slat <- attr(vvv, "lat")[attr(vvv,"iys")]
    oldproj <- attr(vvv,"crs")
    if (is.null(oldproj)) {
      print("resetting points projection")
      oldproj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    }
    coords <- cbind(slon,slat)
    site.ids <- attr(vvv, "site.id")
    which.scheme <- which(unlist(lapply(strsplit(site.ids,split="scheme"),FUN=length))>1)
    
    xmn <- min(attributes(vvv)$lon[attributes(vvv)$ixs])
    xmx <- max(attributes(vvv)$lon[attributes(vvv)$ixs])
    ymn <- min(attributes(vvv)$lat[attributes(vvv)$iys])
    ymx <- max(attributes(vvv)$lat[attributes(vvv)$iys])
    xmn2 <- min(attributes(vvv)$lon)
    xmx2 <- max(attributes(vvv)$lon)
    ymn2 <- min(attributes(vvv)$lat)
    ymx2 <- max(attributes(vvv)$lat)
    if (is.null(xlim)) xlim <- c(xmn2,xmx2) 
    if (is.null(ylim)) ylim <- c(ymn2,ymx2)
    
    spoints <- SpatialPoints(coords=coords,proj4string=CRS(oldproj)) 
    #if (!missing(newproj)) {
    #  spoints <- spTransform(spoints,CRSobj=CRS(newproj) )
    #}
    if(missing(zero.center)) zero.center <- TRUE
    if (missing(breaks)) {
      if (zero.center) {
        maxx <- max(abs(vvv))
        minx <- -maxx
        breaks <- c(pretty(c(minx, +maxx), nlevels))
      }
      else {
        maxx <- max(vvv)
        minx <- min(vvv)
        breaks <- c(pretty(range(vvv, finite = TRUE), 
                           nlevels))
      }
    }
    if (exists(col)) {
      colfun <- get(col)
      col <- colfun(length(breaks) + 1,middle="white")
    }
    vvv <- cbind(value = vvv, lon = slon, lat = slat)
    attr(vvv, "grid.type") <- "lonlat"
    
    r.breaks <- c(min(vvv[,1], na.rm = TRUE) - 1, breaks, 
                  max(vvv[,1], na.rm = TRUE) + 1)
    
    col.sel <- function(XX) {
      if (is.na(XX)) {
        return(NA)
      }
      else {
        return(col[which(XX <= r.breaks)[1] - 1])
      }
    }
    p.col <- sapply(vvv[,1], FUN = col.sel)
    
    
    if (is.null(legend.pos)){
      legend.pos=c(0.1,0.2,0.5,0.9)
    }
    
    if (is.null(fig)){
      fig=c(0,1,0,1)
    }
    
    pch <- c(rep(21,length(site.ids)-length(which.scheme)) , rep(25,length(which.scheme)))
    #par(new = TRUE)
    # spoints
    plot(spoints,ylim=ylim,xlim=xlim,xlab="",ylab="",
         axes=FALSE,col="black",bg=p.col,pch=pch,cex=1.3)
    map('world2Hires',col=1,interior=FALSE,add=TRUE,ylim=ylim,xlim=xlim)
    plot(spoints,xlab="",ylab="",
         axes=FALSE,col="black",bg=p.col,pch=pch,cex=1.3,add=TRUE)
    #box()
    prettyx <- pretty(c(xmn,xmx))
    prettyx <- prettyx[prettyx> xmn & prettyx < xmx]
    prettyy <- pretty(c(ymn,ymx))
    prettyy <- prettyy[prettyy > ymn & prettyy < ymx]
    #Fix this up to (a) suppress map outside xlim and ylim and (b) offset xlim and ylim from points and minx and min y
    #axis(1,tick=TRUE,pos=ymn,las=1,at=prettyx) 
    #axis(2,tick=TRUE,pos=xmn,las=1,at=prettyy)
    
    
    if (plotscale){
      # insert scalebar
      par(new = TRUE, fig = legend.pos)
      image_scale(vvv[,1], zlim = c(minx,maxx), col = col, 
                  horiz=FALSE,las=2,xlab="",ylab="",nlevels=nlevels)
      box()
    } 
  }

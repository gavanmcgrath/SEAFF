#save grid.topology of a field
plot.projected <- function(dat,new.crs=NULL, col = "cool2warm.colors", breaks,
                           addmap = TRUE, zlim = NULL,tindex = NULL,
                           nlevels = 15,zero.center,...){
  require(sp)
  require(raster)
  require(Grid2Polygons)
  require(rgdal)
  require(rgeos)
  
  #set colours
  if (missing(breaks)) {
    if(missing(zero.center)) zero.center <- TRUE
    if (zero.center) {
      maxx <- max(abs(c(dat)))
      minx <- -maxx
      breaks <- c(pretty(c(minx, +maxx), nlevels))
    }
    else {
      maxx <- max(c(dat))
      minx <- min(c(dat))
      breaks <- c(pretty(range(c(dat), finite = TRUE), 
                         nlevels))
    }
    if (is.null(zlim)){
      breaks <- pretty(c(dat),n=nlevels,min.n=nlevels)
    } else {
      breaks <- c(seq(min(zlim),max(zlim),length.out=nlevels))
    }  
  }
  
  if (exists(col)) {
    colfun <- get(col)
    col.pal <- colfun(length(breaks) + 1,middle="white")
  }
  
  
  #Convert matrix to raster
  old.crs <- attributes(dat)$crs
  if (is.null(old.crs)) old.crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  xmn <- min(attributes(dat)$lon[attributes(dat)$ixs])
  xmx <- max(attributes(dat)$lon[attributes(dat)$ixs])
  ymn <- min(attributes(dat)$lat[attributes(dat)$iys])
  ymx <- max(attributes(dat)$lat[attributes(dat)$iys])
  xmn2 <- min(attributes(dat)$lon)
  xmx2 <- max(attributes(dat)$lon)
  ymn2 <- min(attributes(dat)$lat)
  ymx2 <- max(attributes(dat)$lat)
  xlim <- c(xmn2,xmx2); 
  ylim <- c(ymn2,ymx2)
  
  if (is.null(tindex)){
    dat.r <- raster(t(dat),xmn=xmn2, xmx=xmx2, ymn=ymn2, ymx=ymx2,crs=CRS(old.crs))
  } else {
    dat.r <- raster(t(dat[,,tindex]),xmn=xmn2, xmx=xmx2, ymn=ymn2, ymx=ymx2,crs=CRS(old.crs))
  }
 
  #Convert raster to SpatialPolygonsDataFrame with levels set by colours
  e <- extent(c(xmn,xmx,ymn,ymx))
  dat.r <- crop(dat.r,e)
  dat.sgdf <- as(dat.r,"SpatialGridDataFrame")

 
  e.m <- matrix(c(xmn,xmx,ymn,ymx),ncol=2,byrow=TRUE)
  p1 <- Polygon(cbind(c(e.m[1,][c(1,1,2,2,1)]),c(e.m[2,][c(1,2,2,1,1)])))
  p11 <- Polygons(list(p1),"p1")
  croppingPoly <- SpatialPolygons(list(p11), proj4string = CRS(old.crs))
  
  dat.spg <- Grid2Polygons(dat.sgdf,level=TRUE,at=breaks)
 
  #set colours for plotting
  col.idxs <- findInterval(dat.spg[[1]], breaks)
  cols <- col.pal[col.idxs]

  #Transform spatial projection 
   if (is.null(new.crs)){
    dat.proj  <- dat.spg    
  } else {
    dat.proj <- spTransform(dat.spg, CRS = CRS(new.crs))
  }  
  
  plot(dat.proj,border="transparent",col=cols)
  prettyx <- pretty(c(xmn,xmx))
  prettyx <- prettyx[prettyx> xmn & prettyx < xmx]
  prettyy <- pretty(c(ymn,ymx))
  prettyy <- prettyy[prettyy > ymn & prettyy < ymx]
  axis(1,tick=TRUE,pos=ymn,las=1,at=prettyx) 
  axis(2,tick=TRUE,pos=xmn,las=1,at=prettyy)
  if (addmap) {
    #plot.land(xylims=matrix(as.vector(e),nrow=1),center=mean(as.vector(e)[1:2]))
    plot.land2(center=180,extent=e)
    plot(croppingPoly,col=NA,add=TRUE)
  }  
}  


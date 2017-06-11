plot.land2 <- function(center=180,extent=NULL,col="black"){
  require(rgdal)
  require(sp)
  require(raster)
  mp.simp <- readOGR("./data/WorldSimpleShapeFile",verbose=FALSE)
  mp.simp <- as(mp.simp,"SpatialPolygons")
  mp.simp1 <- shift(mp.simp,x=center+180)
  mp.simp2 <- shift(mp.simp,x=center-180)
  
  if (!is.null(extent)){ 
    mp.simp1 <- crop(mp.simp1,extent)
    mp.simp2 <- crop(mp.simp2,extent)
  }
  
 lines(mp.simp1)
 lines(mp.simp2)
}

field2df <- function(fld){
  #takes a rectangular field object and returns a dataframe object
  require(raster)
  r <- raster(t(fld),
            xmn=min(attributes(fld)$lon), xmx=max(attributes(fld)$lon), 
            ymn=min(attributes(fld)$lat), ymx=max(attributes(fld)$lat),
            crs=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84'))

  temp <- as.data.frame(r,xy=TRUE)
  names(temp) <- c("lon","lat","layer")
  temp
}
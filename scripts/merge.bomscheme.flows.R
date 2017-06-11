merge.bomscheme.flows <- function(bom,scheme,unit.diff=c(0.001,1)){
  
  tims.scheme <- attributes(scheme)$time
  tims.bom <- attributes(bom)$time
  yrs <- unique(c(tims.scheme[,1],tims.bom[,1]))
  new.tims <- cbind(rep(yrs,each=12),rep(1:12,length(yrs)))
  newflows <- matrix(NA,ncol=dim(bom)[2]+dim(scheme)[2],nrow=length(new.tims[,1]))
  
  pos1 <- which(new.tims[,1] == tims.bom[1,1] & new.tims[,2] == tims.bom[1,2] )
  pos2 <- which(new.tims[,1] == tims.scheme[1,1] & new.tims[,2] == tims.scheme[1,2] )
  
  newflows[pos1:(pos1+dim(bom)[1]-1),1:dim(bom)[2]] <- c(bom)*unit.diff[1]
  newflows[pos2:(pos2+dim(scheme)[1]-1),(dim(bom)[2]+1):(dim(bom)[2] + dim(scheme)[2])] <- c(scheme)*unit.diff[2]
  
  site.id <- c(attr(bom,"site.id"),paste("scheme_",attr(scheme,"site.id"),sep=""))
  colnames(newflows) <- site.id
  area <-   c(attr(bom,"area"),attr(scheme,"area"))
  lat <-    c(attr(bom,"lat"),attr(scheme,"lat"))
  lon <-    c(attr(bom,"lon"),attr(scheme,"lon"))
  
  attr(newflows,"site.id") <- site.id
  attr(newflows,"area") <- area
  attr(newflows,"lat") <- lat
  attr(newflows,"lon") <- lon
  attr(newflows,"date") <- as.Date(paste(new.tims[,1],new.tims[,2],"01",sep="-"),format="%Y-%m-%d")
  attr(newflows,"time") <- new.tims
  attr(newflows,"tdim") <- 1
  attr(newflows,"units") <- c("Flow: GL/month", "Area: km^2", "Lat: Deg N", "Lon: Deg E")
  
  newflows
}

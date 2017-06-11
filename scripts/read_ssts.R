read_ssts <- function(fid = './data/SST/sst.mnmean.nc'){ 
  require(ncdf4)
  sst.nc <- nc_open(fid);
  lats <- ncvar_get(sst.nc,"lat");
  longs <- ncvar_get(sst.nc,"lon");
  sst <- ncvar_get(sst.nc,"sst");
  sst[sst==-9.96920996838687e+36] <- NA
  tim <- ncvar_get(sst.nc,"time");
  nc_close( sst.nc )
  
  #time units: days since 1800-01-01 00:00:0.0
  mdy <- as.Date(as.POSIXct(tim*24*60*60, origin = "1800-01-01 00:00:0.0") )
  ym <- matrix(as.numeric(unlist(strsplit(as.character(mdy),split="-"))),ncol=3,byrow=TRUE)[,1:2]
  
  attr(sst,"date") <- mdy
  attr(sst,"time") <- ym
  attr(sst,"lat") <- lats
  attr(sst,"lon") <- longs
  attr(sst,"tdim") <- 3
  attr(sst,"units") <- c("temp: degC","lat: degN","lon: degE","time: year")

  sst
}

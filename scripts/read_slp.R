read_slp <- function(fid = './data/SLP/slp.mon.mean.nc'){ 
  require(ncdf4)
  slp.nc <- nc_open(fid);
  lats <- ncvar_get(slp.nc,"lat");
  longs <- ncvar_get(slp.nc,"lon");
  slp <- ncvar_get(slp.nc,"slp");
  #sst[sst==-9.96920996838687e+36] <- NA
  tim <- ncvar_get(slp.nc,"time");
  nc_close( slp.nc )
  
  #time units: days since 1800-01-01 00:00:0.0
  mdy <- as.Date(as.POSIXct(tim*60*60, origin = "1800-01-01 00:00:0.0") )
  ym <- matrix(as.numeric(unlist(strsplit(as.character(mdy),split="-"))),ncol=3,byrow=TRUE)[,1:2]
  
  attr(slp,"date") <- mdy
  attr(slp,"time") <- ym
  attr(slp,"lat") <- lats
  attr(slp,"lon") <- longs
  attr(slp,"tdim") <- 3
  attr(slp,"units") <- c("pressure: millibars","lat: degN","lon: degE","time: year")
  #list(mdy=mdy,ym=ym,lat=lats,lon=longs,sst=sst)
  slp
}

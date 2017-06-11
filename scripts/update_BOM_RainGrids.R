update_BOM_RainGrids <- function(bounding.poly = cbind(c(139,155,155,150,139,139),c(-45,-45,-27,-27,-35,-45)),save.add = "./data/BoM/BoMrainGrids/",start.year=1900){

#Function downloads and unzips BOM 0.05 x 0.05 degree monthly rainfall grids for Australia 
#It checks if you already have the data stored as unzipped grid files in save.add
#Then it extracts terrestrial rainfall and crops to a bounding polygon
#Saves the data to SEA_monthly_precip.RData as a nx x ny x nt array where nt is the number of months, nx and ny is the dimensions of the grid
  
require(raster)
require(sp)
require(rgdal)
require(R.utils)


mon.end.day <- c(31,28,31,30,31,30,31,31,30,31,30,31)
yr.leap <- c(1904, 1908, 1912, 1916, 1920, 1924, 1928, 1932, 1936, 1940, 1944, 1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020, 2024, 2028,2032, 2036)
mons <- c("01","02","03","04","05","06","07","08","09","10","11","12")

url.add <- "http://www.bom.gov.au/web03/ncc/www/awap/rainfall/totals/month/grid/0.05/history/nat/"

print("Checking for new precip grids to download")
#fls <- list.files(path = save.add,pattern=".grid")
fls.g <- as.matrix(read.table(paste(save.add,"ListOfGrids.txt",sep=""),
                  header=FALSE,stringsAsFactors = FALSE))
fls <- fls.g[,1]

current.date <- as.character(Sys.Date())
current.year <- as.numeric(substr(current.date,1,4))
current.month <- as.numeric(substr(current.date,6,7))
for (i in start.year:(current.year-1)){
  for (mon in mons){
    if (mon == "02") { 
      monend <- ifelse(i %in% yr.leap,29,28)
    } else {
      monend <- mon.end.day[which(mon == mons)]
    }
   
    fid <- paste(i,mon,"01",i,mon,monend,".grid",sep="")
    if (fid %in% fls) {
      #Skipping as already downloaded
    } else {  
      print(paste("Downloading:",fid, "from", url.add,sep=" "))
      is.cur <- tryCatch(
      
      R.utils:::downloadFile(
        url = paste(url.add,fid,".Z",sep=""), 
        filename = paste(save.add,fid,".Z",sep=""),
        overwrite = TRUE,
        skip=FALSE,verbose=FALSE)
     
       ,error = function(e) {
        warning("Likely no internet connection. Skipping check for new BOM rain grids.")
        TRUE})
    }
  }
}

i <- current.year
if (current.month>1){
  for (mon in mons[1:(current.month-1)]){
    if (mon == "02") { 
      monend <- ifelse(i %in% yr.leap,29,28)
    } else {
      monend <- mon.end.day[which(mon == mons)]
    }
    
    fid <- paste(i,mon,"01",i,mon,monend,".grid",sep="")
    if (fid %in% fls) {
      #Skipping
    } else {  
      #print(paste("Downloading:",fid, "from", url.add,sep=" "))
      is.cur <- tryCatch(
        R.utils:::downloadFile(
        url = paste(url.add,fid,".Z",sep=""), 
        filename = paste(save.add,fid,sep=""),
        overwrite = TRUE,
        skip=FALSE,verbose=FALSE)
      ,error = function(e) {
          warning("Likely no internet connection. Skipping check for new BOM rain grids.")
          TRUE})
    }
  }
}


#Decompress downloaded grids
#source("./scripts/zipping.functions.R")
fls.Z <- list.files(path = save.add,pattern=".Z")
if (length(fls.Z)>0){
  for (i in 1:length(fls.Z)){
    Decompress7Zip(zipFileName = 
                     paste(save.add,fls.Z[i],sep=""), 
                   outputDirectory=save.add, 
                   delete=TRUE)
  }
}
fls.grid <- list.files(path = save.add,pattern=".grid")

#Process newly downloaded grid files
if (length(fls.grid) > 0) {
  print("Extracting clls in polygon")
  mp.simp <- readOGR("./data/WorldSimpleShapeFile",verbose=FALSE)

  precip <- matrix(1,nrow=691,ncol=886,byrow=TRUE)
  p.r <- raster(precip, xmn=111.975, xmx=156.275, ymn=-44.525, ymx=-9.975, 
              crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
  polys <- SpatialPolygonsDataFrame(SpatialPolygons(
    list(Polygons(list(Polygon(bounding.poly)), 1))),data.frame(ID=c(1)))

  # Extract raster values to polygons in 2 stages
  #1st by Australian region, shapefile
  #2nd by bounding polygon of region pof interest

  v2 <-  raster::extract(p.r, mp.simp,cellnumbers=TRUE)
  whch.polys <- which(unlist(lapply(v2,FUN=length))>0)
  cell.ids <- c(); 
  for (i in 1:length(whch.polys)){ 
    cell.ids <- c(cell.ids, v2[[whch.polys[i]]][,1]) 
  }

  p.r2 <- p.r
  p.r2[!((1:length(p.r)) %in% cell.ids)] <- NA
  v <- raster::extract(p.r2, polys,cellnumbers=TRUE)
  p.r2[!((1:length(p.r)) %in% v[[1]][,1])] <- NA

  final.cell.values <- which(!is.na(p.r2[]))
  blanked <- which(is.na(p.r2[]))

  #Read all new grid files and save data
  dimarr <- c(length(unique(coordinates(p.r2)[,1])),
            length(unique(coordinates(p.r2)[,2])),
            ncol=length(fls.grid))
 
    precip2 <- array(NA,dim=dimarr)
    for (i in 1:length(fls.grid)){
      handle <- file(paste(save.add,fls.grid[i],sep=""), open="rb")
        data <- scan(handle,nlines=691,skip=6,quiet=TRUE)
      close(handle)
      p.m <- matrix(data,nrow=691,ncol=886,byrow=TRUE)
      p.r <- raster(p.m, xmn=111.975, xmx=156.275, ymn=-44.525, ymx=-9.975, 
                crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
      p.r[blanked] <- NA
      p.m2 <- t(as.matrix(p.r))
      precip2[,,i] <- p.m2
    }
}

if (file.exists("./data/processed/precip.rds") & length(fls.grid) > 0){
  print("Appending new data to precip.rds")
  precip <- readRDS("./data/processed/precip.rds")
  dp <- dim(precip)
  precip3 <- array(NA,dim=c(dimarr[1:2],dimarr[3]+dp[3]))
  precip3[,,1:dim(precip)[3]] <- precip
  precip3[,,(dp[3]+1):dim(precip3)[3]] <- precip2
  rm(precip,precip2)
  gc()
  precip <- precip3
  rm(precip3)
  unlink(paste(save.add,fls.grid))
}

if (!file.exists("./data/processed/precip.rds") & length(fls.grid) > 0){
  precip <- precip2
  rm(precip2)
  gc()
  unlink(paste(save.add,fls.grid))
}

if (length(fls.grid) > 0){
  fls <- c(fls,fls.grid)
  write.table(paste(save.add,"ListOfGrids.txt",sep=""),row.names=FALSE,col.names = FALSE)
  
  mdy <- as.Date(unlist(lapply(cbind(fls),FUN=function(x) as.character(as.Date(paste(substr(x,1,4),"-",substr(x,5,6),"-01",sep=""),format="%Y-%m-%d")))),format="%Y-%m-%d")

  ym <- matrix(as.numeric(unlist(strsplit(as.character(mdy),split="-"))),ncol=3,byrow=TRUE)[,1:2]

  attr(precip,"date") <- mdy
  attr(precip,"time") <- ym
  attr(precip,"lat") <- sort(unique(coordinates(p.r2)[,2]),decreasing=TRUE)
  attr(precip,"lon") <- sort(unique(coordinates(p.r2)[,1]))
  attr(precip,"tdim") <- 3
  attr(precip,"units") <- c("precip: mm/month","lat: degN","lon: degE","time: monthly")

  print("Saving precip.rds")
  saveRDS(precip,file=paste("./data/processed/precip.rds",sep=""))
} else {
  print("No precip data to update")
}

}



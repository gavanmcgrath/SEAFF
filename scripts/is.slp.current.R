#' Checks if the current monthly sst data file is up to date
#'
#' @param fid folder id of sst.mnmean.v4.nc data file
#'
#' @return TRUE if the file sst.log exists and the date in it is the current date
#' @export
#'
#' @examples is.sst.current()
#' 
is.slp.current <- function(fid="./data/SLP"){
  nc.fid <- paste(fid,"/slp.mon.mean.nc",sep="")
  if (file.exists(nc.fid)) {
    #read file info at noaa
    require(curl)
    con <- curl("ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.derived/surface/")
    dat <- readLines(con)
    close(con)
    datename <- grep("slp.mon.mean.nc",dat,value=TRUE)
    date_and_name <- sub("^[[:alnum:][:punct:][:blank:]]{43}", "", datename)
    date_and_name <- strsplit(date_and_name,split=" ")[[1]]
    
    #get current year from Sys.Date
    current.yr <- strsplit(as.character(Sys.Date()),split="-")[[1]][1]
    url.nc.date <- paste(current.yr,date_and_name[1],date_and_name[2],sep=" ")
    #Convert dates to POSIXct
    url.mtime <- as.POSIXct(url.nc.date,format="%Y %b %d")
    sst.mtime <- file.info(nc.fid)$mtime
    
    #check if url date is newer
    is.current <- sst.mtime >= url.mtime
  } else {
    is.current <- FALSE
  }
  return(is.current)	
}




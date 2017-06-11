dmi.download <- function(){
  #Function downloads and saves the latest Dipole Mode Index
  #USes Two sources the infrequently updated but longer ESRL
  #and
  #The shorter but more up to date BOM dataset 
  require(RCurl)
  
  fid1 <- "./data/DMI/dmi_ESRL.txt"
  download.file("https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/dmi.long.data",fid1,quiet=FALSE)
  
  fid2 <- "./data/DMI/dmi_BOM.txt"
  download.file("http://www.bom.gov.au/climate/enso/iod_1.txt",fid2,quiet=FALSE)
  
  
  #Extract ESRL data
  dmi1 <- readLines(fid1)
  x <- unlist(regmatches(dmi1, gregexpr('\\(?[-,0-9,.]+', dmi1)))
  x <- as.numeric(gsub('\\(', '-', gsub(',', '', x)))
  if (x[length(x)] <= -999) {
    x <- x[3:(length(x)-1)]
  }
  pos.yr <- seq(from = 1,by=13,to =length(x))
  yrs <- x[pos.yr]
  n <- (length(x) - length(yrs))
  all.yrs <- rep(yrs,each=12)[1:n ]
  all.mons <- rep(1:12,length(yrs))[1:n ]
  all.days <- rep(15,n)
  date1 <- vector("character",length=n)
  for (i in 1:((length(x) - length(yrs)))){
    date1[i] <- paste(c(all.yrs[i],all.mons[i],all.days[i]),collapse="-")
  }
  date1 <- as.Date(date1,format="%Y-%m-%d")
  dmi.vec <- c()
  for (i in 1:length(pos.yr)){
    pos.this.yr <- (pos.yr[i]+1):(pos.yr[i]+12)
    ps.inc <- which(pos.this.yr <= (length(x)))
    dmi.vec <- c(dmi.vec,x[pos.this.yr[ps.inc]])
  }
  dmi1 <- dmi.vec
  
  
  #Extract BOM Data
  dmi2 <- read.csv(fid2)
  date2 <- as.Date(as.character(dmi2[,2]),format="%Y%m%d",origin = "1800-01-01")
  dmi2 <- dmi2[,3]
  
  pos.append <- which(date2 > date1[length(date1)])
  if (length(pos.append)>0) {
    dmi <- c(dmi1,dmi2[pos.append])
    dates <- c(date1,date2[pos.append])
  } else {
    dmi <- dmi1
    dates <- date1
  }
   saveRDS( list(index = dmi, dates = dates), file = "./data/processed/dmi.rds")
}


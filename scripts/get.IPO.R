ipo.download <- function(){
  #Function downloads and saves the latest Interdecdal Pacific Oscillation
  require(RCurl)
  
  fid1 <- "./data/IPO/ipo_ESRL.txt"
  download.file("https://www.esrl.noaa.gov/psd/data/timeseries/IPOTPI/ipotpi.ersst.filt.data",fid1,
                quiet=FALSE)
  
  #Extract ESRL data
  dmi1 <- readLines(fid1)
  yr.range <- as.numeric(unlist(regmatches(dmi1[1], gregexpr('\\(?[-,0-9,.]+', dmi1[1]))))
  n <- length(yr.range[1]:yr.range[2])
  x <- unlist(regmatches(dmi1[2:(n+1)], gregexpr('\\(?[-,0-9,.]+', dmi1[2:(n+1)])))
  x <- as.numeric(gsub('\\(', '-', gsub(',', '', x)))
  
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
  dmi1[dmi1 < -90] <- NA
  saveRDS( list(index = dmi1, dates = date1), file = "./data/processed/ipo.rds")
}

flow.quantiles.4.scheme <- function(fl.yr.strt.mon = 4){
  #require(lubridate)
  
  flows <- readRDS("./data/processed/flows.rds")
  schm.index <- grep("scheme",colnames(flows))
  ids <- unlist(lapply(colnames(flows)[schm.index],FUN=function(x) strsplit(x,split="scheme_")))
  ids <- ids[!(ids == "")]
  
  current.date <- Sys.Date()
  current.year <- as.numeric(format(current.date,"%Y"))
  current.mon <- as.numeric(format(current.date,"%m"))
  pos.2start.of.current.flow.year <- ifelse(current.mon>4,4-current.mon,5-current.mon)
  pos.2.next.flowyr.year <- pos.2start.of.current.flow.year + 12
  
  pos.2start.of.flow.years <- which(attr(flows,"time")[,2] == fl.yr.strt.mon)
  
  
  n.flows <- dim(flows)[1]
  
  max.n <- length(pos.2start.of.flow.years)-1
  
  pos.current.mon <- which(attr(flows,"time")[,2] == current.mon)
  
  probs <- c(0,0.05,0.1,0.25,0.5,0.75,0.9,0.95,1)
  stats <- list()
  for (i in 1:6){
    stats[[i]] <-  apply(
      apply(
      cbind(pos.2start.of.flow.years[-length(pos.2start.of.flow.years)]),
      MARGIN=1,
      FUN=function(x){
        if( dim(flows)[1] > (x+ pos.2.next.flowyr.year + (i-1)*12)) {
            apply(flows[(x:(x+ pos.2.next.flowyr.year + (i-1)*12)),schm.index],
                  MARGIN=2,FUN=sum)
        } else {
          rep(NA,length(schm.index))
        }
      }),
      MARGIN = 1, FUN = function(z) {
        quantile(z,probs=probs,na.rm = TRUE)
      }
    )
  }
  
 
  attr(stats,"probs") <- probs
  attr(stats,"ids") <- ids
  attr(stats,"t.plus") <-  (pos.2.next.flowyr.year + (1:6-1)*12)/12 + current.year
  attr(stats,"what") <- paste("FY",current.year - 2000 + 0:5,sep="")
  
  saveRDS(stats,"./data/processed/flow.stats.rds")
}



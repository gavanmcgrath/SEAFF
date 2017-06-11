#' Combines BOM monthly, seasonal or annual flow data from Hydrological Reference Stations into a single dataframe.
#'
#' @param fid Name of folder where BOM flow data files are located. Defaukts to "./data/LongTermFlows/"
#' @param what = c("monthly","seasonal","annual") specifiec which type of flow to work with
#'
#' @return A data frame with merged results with columns: year, month, ym (fractional year), site_id1 (flow in ML/month),...site_idn.
#' @export Saves BOM_Mon_Flows.Rdata, BOM_Seas_Flows.Rdata, or BOM_Ann_Flows.Rdata to ./data
#'
#' @examples \donotrun{collate_monthlyBOMflows()}
#' 
collate_BOMflows <- function(fid="./data/BoM/LongTermFlows/",
                                    what=c("monthly","seasonal","annual")){
  #collate fie names, site ids and months
  lof <- list.files(fid,pattern=paste("*.",what,sep=""))
  if (what == "monthly") spacer <- "_"
  if (what == "seasonal") spacer <- "_"
  if (what == "annual") spacer <- ""
  
  #get site ids
  site.ids <- sapply(lof,
                     FUN=function(x) {
                       strsplit(x,split=paste("_",what,"_total",spacer,sep=""))[[1]][1]
                       }
                     )
  unique.ids <- sort(unique(site.ids))
  #get month or season as a string
  
    site.mons <- 
      sapply(
        sapply(lof,
             FUN=function(x) {
               strsplit(x,split=paste("_",what,"_total",spacer,sep=""))[[1]][2]
               }),
        FUN=function(y) {
            strsplit(y,split=".csv")[[1]][1]
            }
      )
  
  if (what == "monthly") {
    site.mons <- as.numeric(site.mons)
  }  
  if (what == "seasonal") {
    site.mons <- sapply(site.mons, 
                        FUN=function(x) {
                          switch(x,
                                 "Summer" = 2,
                                 "Autumn" = 5,
                                 "Winter" = 8,
                                 "Spring" = 11)}
                                 )
  }
  if (what == "annual") {
    site.mons <- rep(0,length(site.ids))
  }  
  
  yrs <- c()
  for (i in 1:length(site.ids)){
    site.dat <- read.csv(paste(fid,lof[i],sep=""),skip=20,header=TRUE)
    names(site.dat) <- c("year",site.ids[i])
    site.dat$month <- rep(site.mons[i],length(site.dat[,1]))
    yrs <- unique(c(yrs,site.dat$year+site.dat$month/12)) 
  }  
  yrs <- sort(yrs)
  BOM_Monthly_Flows <- matrix(NA,nrow=length(yrs),ncol=length(unique.ids)+3)
  cnames <- c("year","month","ym",unique.ids)
  colnames(BOM_Monthly_Flows) <- cnames 
  BOM_Monthly_Flows[,c(3)] <- yrs
  
  for (i in 1:length(site.ids)){
    site.dat <- read.csv(paste(fid,lof[i],sep=""),skip=20,header=TRUE)
    names(site.dat) <- c("year",site.ids[i])
    site.dat$month <- rep(site.mons[i],length(site.dat[,1]))
    site.dat$ym <- site.dat$month/12 + site.dat$year
    pos <- which(yrs %in% site.dat$ym)
    col.id <- which(cnames == site.ids[i])
    BOM_Monthly_Flows[pos,col.id] <- site.dat[,2]
    BOM_Monthly_Flows[pos,1] <- site.dat[,1]
    BOM_Monthly_Flows[pos,2] <- site.dat[,3]
  }
  
  lats <- c(); longs <- c();  station.ids <- c(); areas <- c(); meanQs <- c(); mons <- c();
  for (i in 1:length(lof)){
    metadata    <- readLines(paste(fid,lof[i],sep=""),n=20)
    station.id 	<- strsplit(metadata[8],split="[\\(\\)]")[[1]][2]
    mean.streamflow <- as.numeric(strsplit(metadata[9],split=",")[[1]][3])
    area 				<- as.numeric(strsplit(metadata[10],split=",")[[1]][3])
    longlat 		<- as.numeric(unlist(strsplit(strsplit(metadata[11],split=",")[[1]][c(3,5)],split=" ")))
    longlat 			<- as.numeric(longlat[!is.na(longlat)])
    lats 			<- c(lats, longlat[2]); 
    longs 			<- c(longs,longlat[1]); 
    station.ids 	<- c(station.ids,station.id); 
    areas <- c(areas,area); 
    meanQs 			<- c(meanQs, mean.streamflow);
  }
  db <- data.frame(station.id = station.ids,area=areas,lat=lats,lon=longs,mon=site.mons,meanQ=meanQs)
  attr(db,"units") <- c("char","km^2","degS","degN","month","ML")
  
  #Save output
  if (what == "monthly"){
    BOM_db_mon <- db
    saveRDS(list(BOM_Monthly_Flows=BOM_Monthly_Flows,BOM_db_mon=BOM_db_mon),
            file="./data/BoM/BoMFlows_processed/BOM_Mon_Flow.rds")
  }
  if (what == "seasonal"){
    BOM_db_seas <- db
    BOM_Seasonal_Flows <- BOM_Monthly_Flows
    saveRDS(list(BOM_Seasonal_Flows=BOM_Seasonal_Flows,BOM_db_seas=BOM_db_seas),
            file="./data/BoM/BoMFlows_processed/BOM_Seas_Flow.rds")
  }
  if (what == "annual"){
    BOM_db_ann <- db
    BOM_Annual_Flows <- BOM_Monthly_Flows
    saveRDS(list(BOM_Annual_Flows=BOM_Annual_Flows, BOM_db_ann=BOM_db_ann),
            file="./data/BoM/BoMFlows_processed/BOM_Ann_Flow.rds")
  }
  
}


#collate_BOMflows(what="annual")
#collate_BOMflows(what="seasonal")
#collate_BOMflows(what="monthly")

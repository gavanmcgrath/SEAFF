get_BOM_Flow <- function(what=c("monthly","seasonal","annual")){
  
  if (what == "monthly") {
    bomMon <- readRDS("./data/BoM/BoMFlows_processed/BOM_Mon_Flow.rds")
    tims <-  bomMon$BOM_Monthly_Flows[,1:3]
    flows <- bomMon$BOM_Monthly_Flows[,4:dim(bomMon$BOM_Monthly_Flows)[2]]
    db <- bomMon$BOM_db_mon
  }  
  if (what == "seasonal") {
    bomSeas <- readRDS("./data/BoM/BoMFlows_processed/BOM_Seas_Flow.rds")
    tims <-   bomSeas$BOM_Seasonal_Flows[,1:3]
    flows <-  bomSeas$BOM_Seasonal_Flows[,4:dim( bomSeas$BOM_Seasonal_Flows)[2]]
    db <-  bomSeas$BOM_db_seas
  }  
  if (what == "annual") {
    bomAnn <- readRDS("./data/BoM/BoMFlows_processed/BOM_Ann_Flow.rds")
    tims <-  bomAnn$BOM_Annual_Flows[,1:3]
    tims[,2] <- tims[,2]+1
    flows <- bomAnn$BOM_Annual_Flows[,4:dim(bomAnn$BOM_Annual_Flows)[2]]
    db <- bomAnn$BOM_db_ann
  }
  
  site.id <- unique(db[,"station.id"])
  area <-sapply(site.id, FUN = function(x) db[which(x == db[,"station.id"])[1],"area"])
  lat <- sapply(site.id, FUN = function(x) db[which(x == db[,"station.id"])[1],"lat"])
  lon <- sapply(site.id, FUN = function(x) db[which(x == db[,"station.id"])[1],"lon"])
  attr(flows,"site.id") <- site.id
  attr(flows,"area") <- area
  attr(flows,"lat") <- -lat
  attr(flows,"lon") <- lon
  attr(flows,"date") <- as.Date(
    paste(tims[,1],tims[,2],"01",sep="-"),format="%Y-%m-%d")
  attr(flows,"time") <- cbind(tims[,1],tims[,2])
  attr(flows,"tdim") <- 1
  tim.unit <- switch(what,"monthly" = "month", "seasonal" = "quarter",annual = "year")
  attr(flows,"units") <- c(
    paste("Flow:",ifelse(what %in% c("monthly","seasonal"),"ML/","GL/"),tim.unit,sep=""), 
    "Area: km^2", 
    "Lat: Deg N", 
    "Lon: Deg E")
  flows
}

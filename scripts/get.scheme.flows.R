get.scheme.flows <- function(){
  scheme.ids <- read.csv("./data/SchemeFlows/Station_Info.csv")

  strp_white <- function(x) {
    gsub("^\\s+|\\s+$", "",x)
  }

  flows.list <- list()
  for (i in 1:nrow(scheme.ids)){
    file1 <- read.csv(paste("./data/SchemeFlows/",strp_white(scheme.ids$File_id[i]),sep=""),
                    skip=scheme.ids$skip[i],header=FALSE)
    if (strp_white(scheme.ids$Format[i]) == "MonthInCols") {
      flow.id <- file1[,c(scheme.ids$Yr_Col[i],scheme.ids$Mon_Col[i],scheme.ids$Flow_Col[i])]
      pos <- which(!is.na(flow.id[,3]))
      flow.id <- flow.id[min(pos):max(pos),]
      flows.list[[i]] <- flow.id
    }
    if (strp_white(scheme.ids$Format[i]) == "MonthInRows") {
      col.mons <- scheme.ids$Mon_Col[i]:(scheme.ids$Mon_Col[i]+11)
      flow.id <- file1[,c(scheme.ids$Yr_Col[i],col.mons)]
      pos <- which(apply(flow.id,MARGIN=1,FUN=function(x) !(sum(is.na(x)) == length(x))))
      flow.id <- flow.id[pos,]
    
      newflow <- matrix(NA,nrow=length(pos)*12,ncol=3)
      for (j in 1:length(pos)){
        newpos <- ((j-1)*12+1):(j*12)
        newflow[newpos,1] <- c(rep(flow.id[j,1],12))
        newflow[newpos,2] <- c(1:12)
        newflow[newpos,3] <- as.numeric(c(flow.id[j,2:13]))
      }
      flows.list[[i]] <- newflow
    }
  }


  yrs <- c()  
  for (i in 1:length(flows.list)){
    yrs <- unique(c(yrs,flows.list[[i]][,1]))
  }
  yrs <- sort(yrs)
  tims <- cbind(rep(yrs,each=12),rep(1:12,length(yrs)))

  scheme.flows <- matrix(NA,nrow=length(tims[,1]),ncol=length(flows.list))
  for (i in 1:length(flows.list)){
    for (j in 1:length(flows.list[[i]][,1])){
      pos <- which(tims[,1] == flows.list[[i]][j,1] & tims[,2] == flows.list[[i]][j,2])
      scheme.flows[pos,i] <- flows.list[[i]][j,3]
    }
  }

  site.id <- as.character(sapply(scheme.ids$Station_id,FUN=strp_white))
  colnames(scheme.flows) <- site.id
  area <-   as.numeric(sapply(scheme.ids$Area,FUN=strp_white))
  lat <-    as.numeric(sapply(scheme.ids$Latitude,FUN=strp_white))
  lon <-    as.numeric(sapply(scheme.ids$Longitude,FUN=strp_white))
  attr(scheme.flows,"site.id") <- site.id
  attr(scheme.flows,"area") <- area
  attr(scheme.flows,"lat") <- lat
  attr(scheme.flows,"lon") <- lon
  attr(scheme.flows,"date") <- as.Date(
    paste(tims[,1],tims[,2],"01",sep="-"),format="%Y-%m-%d")
  attr(scheme.flows,"time") <- cbind(tims[,1],tims[,2])
  attr(scheme.flows,"tdim") <- 1
  tim.unit <- "month"
  attr(scheme.flows,"units") <- c("Flow: GL/month", "Area: km^2", "Lat: Deg N", "Lon: Deg E")

  scheme.flows
}
  
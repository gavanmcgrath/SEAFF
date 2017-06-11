process.data.4Allmodels <- function(maindir="./data/ForecastModels/Default/"){
  
  curr.mon <- as.numeric(format(Sys.Date(),"%m"))
  mod.table <- get.ccaModel.par.list(curr.mon)
  curr.MON <- format(Sys.Date(),"%b")
  
  #Initialise regions and lags
  windows.by.modelndat <- list()
  windows.by.modelndat[["flows"]] <- lapply(1:14, FUN = function(x) {
    list(lon = rbind(c(140,155),c(151,155)),
       lat = rbind(c(-45,-30),c(-29,-25)),
       lag = c(0))})
  windows.by.modelndat[["scheme"]] <- lapply(1:14, FUN = function(x) {
    list(lon = rbind(c(100,151),c(151,155)),
         lat = rbind(c(-45,-30),c(-30,-25)),
         lag = c(0))})
  windows.by.modelndat[["precip"]] <- lapply(1:14, FUN = function(x) {
    list(lon = rbind(c(139,155)),
         lat = rbind(c(-45,-27)),
         lag = c(mod.table[x,"minlag"]))})
  windows.by.modelndat[["ssts"]] <- lapply(1:14, FUN = function(x) {
      switch(x,
             "1" = list(lon = rbind(c(180,250),c(280,360)),
                        lat = rbind(c(-60,-20),c(10,70)),
                        lag = c(36,156)),
             "2" = list(lon = rbind(c(180,250),c(280,360)),
                        lat = rbind(c(-60,-20),c(10,70)),
                        lag = c(36,156)),
             "3" = list(lon = rbind(c(180,250),c(280,360)),
                        lat = rbind(c(-60,-20),c(10,70)),
                        lag = c(36,156)),
             "4" = list(lon = rbind(c(180,250),c(280,360)),
                        lat = rbind(c(-60,-20),c(10,70)),
                        lag = c(36,156)),
             "5" = list(lon = rbind(c(180,250),c(280,360)),
                        lat = rbind(c(-60,-20),c(10,70)),
                        lag = c(36,156)),
             "6" = list(lon = rbind(c(180,250),c(280,360)),
                        lat = rbind(c(-60,-20),c(10,70)),
                        lag = c(36,156)),
             "7" = list(lon = rbind(c(180,250),c(280,360)),
                        lat = rbind(c(-60,-20),c(10,70)),
                        lag = c(36,156)),
             "8" = list(lon = rbind(c(180,250),c(280,360)),
                        lat = rbind(c(-60,-20),c(10,70)),
                        lag = c(48,168)),
             "9" = list(lon = rbind(c(180,250),c(280,360)),
                        lat = rbind(c(-60,-20),c(10,70)),
                        lag = c(60,180)),
             "10" = list(lon = rbind(c(180,250),c(280,360)),
                         lat = rbind(c(-60,-20),c(10,70)),
                         lag = c(72,192)),
             "11" = list(lon = rbind(c(180,250),c(280,360)),
                         lat = rbind(c(-60,-20),c(10,70)),
                         lag = c(48,168)),
             "12" = list(lon = rbind(c(180,250),c(280,360)),
                         lat = rbind(c(-60,-20),c(10,70)),
                         lag = c(60,180)),
             "13" = list(lon = rbind(c(180,250),c(280,360)),
                         lat = rbind(c(-60,-20),c(10,70)),
                         lag = c(36,156)),
             "14" = list(lon = rbind(c(180,250),c(280,360)),
                         lat = rbind(c(-60,-20),c(10,70)),
                         lag = c(72,192))
             )
    })
    windows.by.modelndat[["slps"]] <- lapply(1:14, FUN = function(x) {
      list(lon = rbind(c(90,170)),
           lat = rbind(c(-65,20)),
           lag = c(mod.table[x,"minlag"]))
      }
      )
    
  
  
  for(dat in c("flows","scheme","ssts","slps")){
    print(dat)
    
    data.orig <- switch(dat,
                 "flows" = readRDS("./data/processed/flows.rds"),
                 "scheme" = readRDS("./data/processed/scheme.rds"),
                 "precip" = readRDS("./data/processed/precip.rds"),
                 "ssts" = readRDS("./data/processed/ssts.rds"),
                 "slps" = readRDS("./data/processed/slp.rds"))
    
  start.date <-  as.Date("1960-01-01")
    #attr(data.orig,"date")[max(unlist(lapply(windows.by.modelndat[[dat]],FUN=function(x) x$lag))+1)]
  end.date <- Sys.Date()
  
  yr1 <- NULL
  yr2 <- NULL
  anomaly.savemus <-  TRUE
  trend.savecoeffs <- FALSE  #let default = FALSE
  weightbylat <-      ifelse(dat %in% c("flows","scheme"), FALSE,TRUE)
  doseasonal <-       TRUE
  doscale <-          ifelse(dat %in% c("flows","scheme"),TRUE,FALSE)
  dodetrend <-        FALSE
  doanomaly <-        TRUE
  dopca <-            TRUE
  
  for (i in 1:14){
    print(i)
    savedir <- paste(maindir,curr.MON,ifelse(i<10,"0",""),i,sep="")
    
    windws <- windows.by.modelndat[[dat]][[i]]
    if (dat %in% c("flows","scheme")){
      data <- field1dmat2(data.orig,
                          start.date=start.date,
                          end.date = end.date,
                          xlim = windws$lon,
                          ylim = windws$lat,
                          lag = 0)
    } else {
      data <- field2dmat2(data.orig,
                        start.date=start.date,
                        end.date = end.date,
                        xlim = windws$lon,
                        ylim = windws$lat,
                        lag = windws$lag)

    }
    
    if (doseasonal){
      #mons <- proc.seq[[i]]$mons
     #seas.mon.start <- curr.mon+mod.table[i,"start"]
      #if (seas.mon.start>12) seas.mon.start <- seas.mon.start - floor(seas.mon.start/12)*12
      #mons <- rep(seas.mon.start, mod.table[i,"win"])
      smth <- mod.table[i,"win"]
      data <- filter_Field(data,method="ma",filt=rep(1,smth)/smth)
      saveRDS(data,file=paste(savedir,"/seasonal_",i,"_",dat,".rds",sep=""))
    } else {
      #delete the seasonaldata file
    }
    
    if (doscale) {
      data <- scale_Flow(data)
    }   
    
    if (dodetrend){
        trend.savename <- paste(savedir,"/trendcoeffs_",i,"_",dat,".rds",sep="")
        yr1 <- yr1
        yr2 <- yr2
        trend.savecoeffs <- proc.seq[[i]]$save.trendcoeffs
        data <- detrend_Field(data,anomPeriod=as.numeric(c(yr1,yr2)),
                             trend.savecoeffs = TRUE,
                             trend.savename = trend.savename)
    } else {
      #delete the trend coeffs file
    }
      
    if (doanomaly){
      sdMethod <- "mon"
      anomaly.savename <- paste(savedir,"/musd_",i,"_",dat,".rds",sep="")
      data <- mnthly_Anom(data,anomPeriod = c(yr1, yr2),sdMethod =sdMethod,
                         save.mus =  TRUE,
                         save.mus.name= anomaly.savename)
      saveRDS(data,file=paste(savedir,"/anomaly_",i,"_",dat,".rds",sep=""))
    } else {
      #delete the anomaly coeffs file
    }
    
    #Remove na rows
    data <- trim.field(data)
    
    if (dopca){
      dat.pca <- pca(data,neof=min(30,dim(data)[2]),weightbylat = weightbylat, ceof=FALSE)
      saveRDS(dat.pca,file=paste(savedir,"/pca_",i,"_",dat,".rds",sep=""))
    }
  }
  
  }
  
}

compare.pred2obs <- function(dat1="flows",dat2="ssts"){
    #dat1=c("flows","scheme")
    #dat2=c("ssts","slps","precip")
    
    curr.mon <- as.numeric(format(Sys.Date(),"%m"))
    curr.MON <- format(Sys.Date(),"%b")
    res <- list()
    
    for (i in 1:14){
      print(i)
      wdir <- paste(maindir,curr.MON,ifelse(i<10,"0",""),i,sep="")
      
      obs.orig <- readRDS(file=paste(wdir,"/seasonal_",i,"_",dat1,".rds",sep=""))
      
      obs.anom <- readRDS(file=paste(wdir,"/anomaly_",i,"_",dat1,".rds",sep=""))
      pred.anom <- readRDS(pred,file=paste(wdir,"/",dat2,"_",dat1,"_pred.rds",sep=""))
      
      
      seas.stats <- readRDS(paste(wdir,"/musd_",i,"_",dat1,".rds",sep=""))
      
      match.dates <- which(attr(seas.stats$mus,"date") %in% attr(pred.anom,"date"))
      #obs.orig <- obs.orig[match.dates,]
      obs.seas <- obs.anom * seas.stats$sds + seas.stats$mus
      obs.seas <- obs.seas[match.dates,]
      pred.seas <- pred.anom * seas.stats$sds[match.dates,] + seas.stats$mus[match.dates,]
      
      obs.rescale <- 10^(obs.seas) - 1
      pred.rescale <-  10^(pred.seas) - 1
        
      res[[i]] <- list(obs = obs.rescale,pred = pred.rescale)
      
    }
    res
}


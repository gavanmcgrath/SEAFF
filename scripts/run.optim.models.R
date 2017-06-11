run.optim.models <- function(dat="ssts",maindir="./data/ForecastModels/Default/"){
  #dat=c("ssts","slps","precip")
  
  curr.mon <- as.numeric(format(Sys.Date(),"%m"))
  curr.MON <- format(Sys.Date(),"%b")
  
  for (i in 1:14){
    print(i)
    wdir <- paste(maindir,curr.MON,ifelse(i<10,"0",""),i,sep="")
    predictand <- readRDS(file=paste(wdir,"/anomaly_",i,"_flows.rds",sep=""))
    predictor <- readRDS(file=paste(wdir,"/anomaly_",i,"_",dat,".rds",sep=""))
    
    predictand <- match.fields.by.date(predictand,predictor)
    predictor <- match.fields.by.date(predictor,predictand)
    predictand <- match.fields.by.date(predictand,predictor)
    predictor <- match.fields.by.date(predictor,predictand)
    
    results.list <- optim.txty4cca(predictor,predictand,0)
    
    tx <- results.list$tx
    ty <- results.list$ty
    pred <- cca.pred(predictor,predictor,predictand,tx,ty)
    saveRDS(pred,file=paste(wdir,"/",dat,"_flows_pred.rds",sep=""))
  }
}


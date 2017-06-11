forcastTable <- function(site.id){
  
  flow.stats <- readRDS("./data/processed/flow.stats.rds")
  probs <- attr(flow.stats,"probs")
  q.times <- attr(flow.stats,"t.plus")
  t.sort <- sort(q.times,index.return=TRUE)$ix
  pos.id <- which(attr(flow.stats,"ids") == site.id)
  Qstat <- matrix(unlist(lapply(flow.stats,FUN = function(x) x[,pos.id])),
                  nrow=length(probs))
  
  #1. load forecasts
  #forecasts have attributes such as
  #month of forecast
  #cumulative months in a forecast
  
  #2. load historical flows 
  #calculate historical cumulative flows into list
  #obs
  
  #create a poe table as below
  
  obs <- list()  #length of forc
  obs[[1]] <- NA; obs[[2]] <- NA;  obs[[3]] <- NA;   #dummy delete when forecast added
  obs[[4]] <- NA;  obs[[5]] <- NA;  obs[[6]] <- NA; 
  forc <- rep(NA,6) 
  hist.poe <- round(c(
    apply(cbind(1:length(forc)),MARGIN = 1,FUN = function(x) {
      sum(obs[[x]] > c(forc[x]))/length(obs[[x]]) * 100})),0)
  
  
  flows.table <- data.frame( 
    Flow_Year = floor(q.times[t.sort]),		
    Q_90POE =  round(Qstat[which(probs == 0.9),],0),
    Q_50POE =  round(Qstat[which(probs == 0.5),],0),
    Q_10POE =   round(Qstat[which(probs == 0.1),],0),
    Forecast = round(forc,0), 
    SE = round(forc,0),  #Fix to put in rms error
    Hist_POE = round(hist.poe,3)
  )
}



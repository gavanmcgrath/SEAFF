match.obs.for <- function(pred,obs,hist.obs=NULL) {
  
  sites.in.pred <- attributes(pred$predictand.center)$site.id
  sites.in.obs <- attributes(obs)$site.id
  if (!is.null(hist.obs)) sites.in.hist <- attributes(hist.obs)$site.id
  
  which.pred.id <- which(sites.in.pred %in% sites.in.obs)
  which.obs.id <- which(sites.in.obs %in% sites.in.pred)
  if (!is.null(hist.obs)) which.hist.id <- which(sites.in.hist %in% sites.in.pred[which.pred.id])
  
  dates.in.pred <- attributes(pred$predictor.center)$date
  dates.in.obs <- attributes(obs)$date
  which.dates.pred <-  which(dates.in.pred %in% dates.in.obs)
  which.dates.obs <-  which(dates.in.obs %in% dates.in.pred)

  which.pred.id0 <- which.pred.id
  which.obs.id0 <-  which.obs.id
  which.hist.id0 <- which.hist.id
  nwin <- attr(pred$predictand.center,"window")
  if (is.null(nwin)) nwin <- 1
  if (nwin > 1){
    for (i in 2:nwin){
      which.pred.id <- c(which.pred.id,which.pred.id0+(i-1)*
                           length(attr(pred$predictand.center,"ixs")))
      
      which.obs.id <- c(which.obs.id,which.obs.id0+(i-1)*
                           length(attr(obs,"ixs")))
      
      which.hist.id <- c(which.hist.id,which.hist.id0+(i-1)*
                           length(attr(hist.obs,"ixs")))
    }
  }
  
  matched.pred <- pred$predicted.dmat[which.dates.pred, which.pred.id]
  matched.obs <- obs[which.dates.obs,which.obs.id]
  if (!is.null(hist.obs)) matched.hist.obs <- hist.obs[,which.hist.id]
  
  if (!is.null(hist.obs)) res <- list(pred=matched.pred,obs=matched.obs,hist.obs=matched.hist.obs)
  if (is.null(hist.obs)) res <- list(pred=matched.pred,obs=matched.obs)
  
  all.atts <- names(attributes(obs))
  trans.atts <- all.atts[!(all.atts %in% 
                             c("dim","dimnames","names","row.names","class"))]
  for (a in trans.atts) {
    attr(res,a) <- attr(obs,a)
  }
  attr(res,"date") <- dates.in.obs[which.dates.obs]
  attr(res,"tims") <- attributes(obs)$tims[which.dates.obs,]
  attr(res,"site.id") <- sites.in.obs[which.obs.id[1:(length(which.obs.id)/nwin)]]
  attr(res,"lat") <- attributes(obs)$lat[which.obs.id[1:(length(which.obs.id)/nwin)]]
  attr(res,"lon") <- attributes(obs)$lon[which.obs.id[1:(length(which.obs.id)/nwin)]]
  attr(res,"area") <- attributes(obs)$area[which.obs.id[1:(length(which.obs.id)/nwin)]]
  
  res
}


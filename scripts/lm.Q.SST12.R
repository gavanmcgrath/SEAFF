lm.Q.SST12 <- function(lag1 = 3*12,lag2=13*12){
  
  s12 <- readRDS("./data/processed/s1s2.rds")
  flows <- readRDS("./data/processed/flows.rds")
  f.t <- attr(flows,"time")[,1] + attr(flows,"time")[,2]/12
  s.t <- s12$yrmon[,1]+s12$yrmon[,2]/12
  
  flows.ma <- apply(flows,MARGIN=2,FUN=function(x) c(filter(x,filter=rep(1/36,36))))
  flows.ma <- log(flows.ma)
  flows.ma[is.nan(flows.ma)] <- NA
  flows.ma[is.infinite(flows.ma)] <- NA
  flows.ma <- flows.ma[f.t > 1949,]
  f.t <- f.t[f.t>1949]
  #detrend
  flows.ma <- apply(flows.ma,MARGIN=2,FUN=function(x) {
    if(sum(!is.na(x)) == 0) {
      rep(NA,length(x))
    } else {
      pos <- which(!is.na(x))
      x[pos] <- as.numeric(residuals(lm(x~f.t)))
      x
    } } )
  
  res <- matrix(NA,ncol=ncol(flows),nrow=10)
  rownames(res) <- c("ar2","m0","m1","m2","se0","se1","se2","p0","p1","p2")
  colnames(res) <- colnames(flows)
  for (i in 1:dim(flows.ma)[2]){
    
    pos.f <- which(!is.na(flows.ma[,i]))
    fi <- flows.ma[pos.f,i]
    pos.ft <- which(f.t[pos.f] %in% s.t)
    pos.st <- which(s.t %in% f.t[pos.f])
    if (min(pos.st) < max(lag1,lag2)+1) {
      pos.st <- seq(max(lag1,lag2)+1,max(pos.st),by=1)
      pos.ft <- pos.ft[(length(pos.ft) - length(pos.st) + 1):length(pos.ft)]
    }
    f <- fi[pos.ft]
    ps1 <- pos.st - lag1
    ps2 <- pos.st - lag2
    s1 <- s12$s1[pos.st-lag1]
    s2 <- s12$s2[pos.st - lag2]
    if (sum(!is.na(f)) >3) {
      r.f <- lm(f~ s1 + s2)
      r.s <- summary(r.f)
      res[,i] <- c(r.s$adj.r.squared,c(r.s$coefficients[,c(1,2,4)]))
    } else {
      res[,i] <- rep(NA,10)
    } 
  }
  attr(res,"lat") <- attr(flows,"lat")
  attr(res,"lon") <- attr(flows,"lon")
  res
}

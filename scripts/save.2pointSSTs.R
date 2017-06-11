save.2pointSSTs <- function(){
  sst <- readRDS("./data/processed/ssts.rds")
  sst.points <- rbind(c(202,-46),c(314,22))
  p1 <- c(which(attr(sst,"lon") == sst.points[1,1]),which(attr(sst,"lat") == sst.points[1,2]))
  p2 <- c(which(attr(sst,"lon") == sst.points[2,1]),which(attr(sst,"lat") == sst.points[2,2]))

  sst1 <- sst[p1[1],p1[2],]
  sst2 <- sst[p2[1],p2[2],]
  
  sst1.ma <- c(filter(sst1,rep(1/36,36)))
  sst2.ma <- c(filter(sst2,rep(1/36,36)))
  
  pos <- which(attr(sst,"time")[,1] > 1949)
  tm <- attr(sst,"time")[,1] + attr(sst,"time")[,2]/12
  res1 <- lm( sst1.ma[pos] ~ tm[pos])
  res2 <- lm( sst2.ma[pos] ~ tm[pos])
  
  saveRDS(list(s1 = residuals(res1), s2 = residuals(res2), yrmon = attr(sst,"time")[pos,], 
       coeff1 = coefficients(res1),coeff2 = coefficients(res2) ),
       file = "./data/processed/s1s2.rds")
}



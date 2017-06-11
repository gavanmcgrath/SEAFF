fill.missing.flows <- function(flows=NULL){
  if (is.null(flows)) flows <- get.scheme.flows()
  newflows <- flows
  #Use regressions on Dartmouth Dam Wall Data to fill missing numbers for others
  colflows <- colnames(flows)
  wh.d <- which(colflows == "Dartmouth_DamWall")
  
  for (i in colflows){
    if (i != "Dartmouth_DamWall"){
      lf2 <- log(flows[,wh.d])
      lf1 <- log(flows[,i])
      lmf <- lm(lf1~lf2)
      pos2 <- !is.na(lf2)
      pos1 <- is.na(lf1)
      pos3 <- which(pos2 & pos1)
      
      p.lf1 <- predict(lmf,newdata = data.frame(lf2 = lf2[pos3]))
      #print(p.lf1))
      p.f1 <- exp(p.lf1)
      newflows[pos3,i] <- p.f1
    }
  }
  newflows
}

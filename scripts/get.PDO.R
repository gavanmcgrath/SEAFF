get.PDO <- function(){

  
  require("RCurl")
  if (url.exists("http://research.jisao.washington.edu/pdo/PDO.latest")) {
    
    pdo <- readLines(textConnection(getURL("http://research.jisao.washington.edu/pdo/PDO.latest")))
    
    header.line <- which(pdo == "YEAR     JAN    FEB    MAR    APR    MAY    JUN    JUL    AUG    SEP    OCT    NOV    DEC")
    last.line <- which(pdo == "")[which(which(pdo == "")> header.line+1)[1]]
    
    pdo2 <- apply(cbind(pdo[(header.line+2):(last.line-1)]), MARGIN=1,
                  FUN=function(x) {
      unlist(strsplit(x,split=" "))
    })

    pdo.yrs <- as.numeric(unlist(lapply(
      pdo2,FUN=function(x) strsplit(x[1],split="\\*")[[1]][1])))

    pdo.vals <- lapply(lapply(pdo2,
                    FUN=function(x) as.numeric(x[2:length(x)])), 
                       FUN = function(x) x[!is.na(x)])
    pdos <- c()
    for (i in 1:length(pdo.yrs)){
      for (j in 1:length(pdo.vals[[i]])){
        pdos <- rbind(pdos,c(pdo.yrs[i],j,pdo.vals[[i]][j]))
      }	
    }
    pdo.dates = as.Date(paste(pdos[,1],pdos[,2],rep("01",length(pdos[,2])), sep = "-"),origin = "1800-01-01")
    pdos <- list(index = pdos[,3],dates = pdo.dates)
    saveRDS(pdos,file="./data/processed/pdo.rds")
  }
}

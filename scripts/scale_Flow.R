scale_Flow <- function(dat){
  
  res <- log10(dat+1)
  
  res <-  transfer.attributes(dat,res)

  return(res)
}

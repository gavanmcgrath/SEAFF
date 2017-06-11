read_precip <- function(){
  precip <- readRDS(file="./data/processed/precip.rds")
  return(precip)
}

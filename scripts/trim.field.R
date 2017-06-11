trim.field <- function(field){
  whch.not.na <- !(is.na(apply(field,MARGIN=1,FUN=sum)))
  newfield <- field
  newfield <- newfield[whch.not.na,]
  newfield <- transfer.attributes(field,newfield)
  attr(newfield,"time") <- attr(newfield,"time")[whch.not.na,]
  attr(newfield,"date") <- attr(newfield,"date")[whch.not.na]
  newfield
}
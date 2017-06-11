match.fields.by.date <- function(field1,field2,anomPeriod){
  #extract the sites between the anomPeriod years which have complete data
  
  #Find rows matching dates in anomPeriod
  y1 <- as.numeric(format(attr(field1,"date"),"%Y"))
  p1 <- y1 %in% c(anomPeriod[1]:anomPeriod[2])
  #Find matching dates in both fields
  date.match <- attr(field1,"date") %in% attr(field2,"date")
  p1 <- p1 & date.match
  p1 <- which(p1)
  
  #Find sites with complete data in anomPeriod
  pos.col1 <- which(!is.na(apply(field1[p1,],MARGIN=2,FUN=function(x) sum(x))))
  
  #determine the valid dates, i.e. matching, in anomPeriod and having full data
  valid.dates <-  which(!is.na(apply(field1[p1,pos.col1],MARGIN=1,
                                FUN=function(x) sum(x))))
  row.match <- p1[valid.dates]
 # print(paste("Dim field",paste(dim(field1),collapse=", "),sep=" "))
#  print(paste("Dim newfield",paste(c(length(row.match),length(pos.col1)),collapse = ", "),sep=" "))
  
  
  field.return <- field1[row.match,pos.col1]
  field.return <- transfer.attributes(field1,field.return)
  attr(field.return,"date") <- attr(field.return,"date")[row.match]
  attr(field.return,"time") <- attr(field.return,"time")[row.match,]
  if ("site.id" %in% names(attributes(field.return))){
    attr(field.return,"lat") <- attr(field.return,"lat")[pos.col1]
    attr(field.return,"lon") <- attr(field.return,"lon")[pos.col1]
    attr(field.return,"site.id") <- attr(field.return,"site.id")[pos.col1]
    attr(field.return,"area") <- attr(field.return,"area")[pos.col1]
  }
  
  field.return
}

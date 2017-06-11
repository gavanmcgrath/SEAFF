forward.filter <- function(x,filt) {
  # A linear filter returning a vector of values shifted to the past by the length of the filter
  #and NAs appended to the end of the vector 
  #x=c(NA,NA,NA,1,2,3,4,5,6,1)
  # filt=c(1,1)
  # returns:
  # NA NA  3  5  7  9 11  7 NA NA
  x <- as.numeric(stats:::filter(x,filt,side=1))
  x <- c(x[(length(filt) + 1):length(x)],rep(NA,length(filt)))
  x
}
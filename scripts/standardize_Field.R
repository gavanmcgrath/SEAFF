#' standardize_Field  Standardise a 3D array 
#'
#' @param dat  the 3D numeric array
#' @param tdim the dimension of dat for time
#' @param anomPeriod  A charachter or numeric vector of length 2 containing the year range to calculate trends
#'
#' @return an Array of dim(dat) of detrended values
#'
#' @examples
#' dat <- array(NA, c(2,2,36))
#' for (i in 1:2){ for (j in 1:2) {  dat[i,j,] <- rnorm(36)*0.5 + 3*cos(1:36*2*pi/12)}}
#' dat.anom <- standardize_Field(dat,tdim=3)
#' 
standardize_Field <- function(dat,tdim=3, dat.dates =NULL, anomPeriod=NULL){
  #Function stanardises a field
  n <- dim(dat)[tdim]
  
  if (!is.null(anomPeriod)) {
    anom.pos <- which(dat.dates >= paste(anomPeriod[1],"-12-01",sep="") & dat.dates <= paste(anomPeriod[2], "-01-01",sep=""))
  } else {
    anom.pos <- 1:n
  }
  
  d.mean <- dat
  
  if (tdim == 3) {
    for (i in 1:dim(dat)[1]) { 
      for (j in 1:dim(dat)[2]) { 
        if (sum(!is.na(dat[i,j, anom.pos]))>3){
          mn <- mean(dat[i,j, anom.pos],na.rm=TRUE)
          sd <- sd(dat[i,j, anom.pos],na.rm=TRUE)
          d.mean[i,j,] <- (dat[i,j,] - mn)/sd
        } else {
          d.mean[i,j,] <- rep(NA,n)
        }
      }}
  }
  
  if (tdim == 2) {
    for (i in 1:dim(dat)[1]) { 
      for (j in 1:dim(dat)[3]) { 
        if (sum(!is.na(dat[i,j, anom.pos]))>3){
          mn <- mean(dat[i, anom.pos,j],na.rm=TRUE) 
          sd <- sd(dat[i, anom.pos,j],na.rm=TRUE)
          d.mean[i,,j] <- (dat[i,,j]  - mn)/sd
        } else {
          d.mean[i,,j] <- rep(NA,n)
        }
      }}
  }
  
  if (tdim == 1){
    for (i in 1:dim(dat)[2]) { 
      for (j in 1:dim(dat)[3]) { 
        if (sum(!is.na(dat[i,j, anom.pos]))>3){
          mn <- mean(dat[anom.pos,i,j],na.rm=TRUE) 
          sd <- sd(dat[anom.pos,i,j],na.rm=TRUE)
          d.mean[,i,j] <- (dat[,i,j]  - mn)/sd
        } else {
          d.mean[,i,j] <- rep(NA,n)
        }
      }}
  }
  
  d.mean
}
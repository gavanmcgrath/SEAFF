#' mnthly_Anom  Calculate monthly anomalies of a 3D array 
#'
#' @param dat  the 3D numeric array
#' @param tdim the dimension of dat for time
#' @param anomPeriod  A charachter or numeric vector of length 2 containing the year range to calculate trends
#'
#' @return an Array of dim(dat) of detrended values
#'
#' @examples
#' dat <- array(NA, c(2,2,36))
#' for (i in 1:2){ for (j in 1:2) {  dat[i,j,] <- 3*cos(1:36*2*pi/12)}}
#' dat.anom <- detrend_Field(dat,tdim=3)
#' 
detrend_Field <- function(dat, anomPeriod = NULL,
                          trend.savecoeffs = FALSE,
                          trend.savename = NULL){
  require(parallel)
  #Function generates a monthly anomaly of a field
  tdim <- attributes(dat)$tdim
  dat.dates <- attributes(dat)$date
 #print(tdim)
 
  n <- dim(dat)[tdim]
  x2 <- 1:n
  marg <- setdiff(1:length(dim(dat)),tdim)
  
  if (!is.null(anomPeriod)) {
    anom.pos <- which(dat.dates >= paste(anomPeriod[1],"-12-01",sep="") & 
                        dat.dates <= paste(anomPeriod[2], "-01-01",sep=""))
  } else {
    anom.pos <- 1:n
  }
  print(anom.pos)
  
  cl <- makeCluster(3)
  clusterExport(cl = cl, varlist = c("x2","anom.pos"),envir=environment())
  
    d.mean.temp <- parApply(cl,dat,MARGIN=marg,FUN=
      function(x){
        y  <- x[anom.pos]
        if (sum(!is.na(y))>3){
          trnd <- lm(y~anom.pos) 
          x - coefficients(trnd)[2]*x2 - coefficients(trnd)[1] 
        } else {
          rep(NA,n)
        }
      })
    
    if (trend.savecoeffs){
      coeffs <- parApply(cl,dat,MARGIN=marg,FUN=
                              function(x){
                                y  <- x[anom.pos]
                                if (sum(!is.na(y))>3){
                                  trnd <- lm(y~anom.pos) 
                                  coefficients(trnd)[1:2] 
                                } else {
                                  c(NA,NA)
                                }
                              })
      saveRDS(coeffs,file=trend.savename)
    }
    
    if (length(dim(dat)) == 3) {
      if (tdim == 2){
        d.mean.temp <- aperm(d.mean.temp,c(2,1,3))
      }
      if (tdim == 3) {
        d.mean.temp <- aperm(d.mean.temp,c(2,3,1))
      }
    }
    
    if (length(dim(dat)) == 2) {
      if (tdim == 2){
        d.mean.temp <- t(d.mean.temp)
      }
    }
    
    stopCluster(cl)
  
    d.mean.temp <- transfer.attributes(dat,d.mean.temp)  
    return(d.mean.temp)
}

#'hilbert Hilbert Transform
#'
#' \code{hilbert} returns the Imaginary part of the Hilbert transform
#'
#'
#' @param wave A numeric vector or matrix. 
#' @param f Frequency. Default = 1.
#' @return  The imaginary part of the hilbert transform. If wave is a matrix a matrix is returned with.
#'  the transform returned on each column seprately. Original code from the package seewave.
#'
#' @examples
#' Standing Wave Basic EOF
#' \dontrun{
#' nx <- 20; ny <- 20; nt <- 40;
#' dat <- array(NA,dim=c(nx,ny,nt))
#' for (k in 1: nt) {  for (x in 1:nx) { for (y in 1:ny) {
#' dat[x,y,k] <- cos((2*x+y)*2*pi/20 + k/10*2*pi) 
#' }}}
#' par(mfrow=c(3,3),mar=c(0,0,0,0),oma=c(1,1,1,1))
#' for (i in 1:9) { image(dat[,,i*4],axes=FALSE,xlab="",ylab=""); box();}
#' newdat <- matrix(NA,nt,nx*ny); newdat[] <- c(dat)
#' eofs <- pca(dat)
#' }
#' 
#' Travelling wave Compelx EOF
#' \dontrun{
#'  eofs <- pca(dat,ceof=TRUE)
#'  }
#'  
hilbert <- function (wave, f=1) 
{
  if (is.matrix(wave)) {
    ht <- apply(wave,MARGIN=2,FUN=function(x) hilbert(x))
  } else {
    n <- n1 <- length(wave)
    log.n <- log2(n)
    if (log.n != floor(log.n)) {
      p <- ceiling(log.n)
      nzp <- 2^p - n
      wave <- c(rep(0, floor(nzp/2)), wave, rep(0, ceiling(nzp/2)))
      n <- length(wave)
    }
    h <- rep(0, n)
    if (n > 0 & 2 * floor(n/2) == n) {
      h[c(1, n/2 + 1)] <- 1
      h[2:(n/2)] <- 2
    }
    else {
      h[1] <- 1
      h[2:((n + 1)/2)] <- 2
    }
 
    ht <- fft(fft(wave) * h, inverse = TRUE)/n
    n2 <- length(ht)
    diffn <- n2 - n1
    if (diffn > 0) {
      ht <- ht[floor(diffn/2):(n1 + floor(diffn/2) - 1)]
    }
  }  
  return(as.matrix(ht))
}



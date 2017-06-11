#' Pepare a lagged matrix for extended eof analysis
#'
#' @param mat A 2d matrix
#' @param lag An integer < dim(mat)[1]
#'
#' @return A matrix of shape c(dim(mat)[1] - lag, dim(mat)[2]*(lag+1))
#' @export
#'
#' @examples
prep.eeof <- function(mat, lag) {
  window <- lag + 1
  nm = dim(mat)
  if (lag >= nm[1]) stop("Error: Lag same length or longer than dim(mat)[1]. Stopping.")
  if (lag < 0) stop("Error: Lag must be >= 0. Stopping.")
  nwin = nm[1] - window + 1
  new.shape = c(nwin, window*nm[2])
  newmat <- matrix(NA,nrow=new.shape[1],ncol=new.shape[2])
  for (i in 1:window){
    newmat[,((i-1)*nm[2]+1):(i*nm[2])] <- mat[i:(nwin+i-1),]
  }
  
  newmat <- transfer.attributes(mat,newmat)
  
  attr(newmat,"window") <- window
  attr(newmat,"date") <- attr(mat,"date")[max(1,lag+1):nm[1]]
  return(newmat)
}

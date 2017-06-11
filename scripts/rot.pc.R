#' Title rot.pc
#'
#' @param x.pca 
#' @param trunc 
#' @param normalize 
#'
#' @return
#' @export
#'
#' @examples
rot.pc <-
function(x.pca,trunc,normalize=TRUE) {

       # extract relevant items from pca object
       dat <- x.pca$dat
       center <- x.pca$center
       scale <- x.pca$scale
       loadings <- x.pca$eof[,1:trunc]

       # centred/scaled data matrix
       mu <- matrix(rep(center,dim(dat)[1]),nrow=dim(dat)[1],byrow=TRUE)
       sig <- matrix(rep(scale,dim(dat)[1]),nrow=dim(dat)[1],byrow=TRUE)
       datc <- ((dat-mu)/sig)

       # rotate 

       x.rot <- varimax(loadings,normalize=normalize)


       # calculate results
       rot.loadings <- loadings(x.rot)
       rot.scores <- as.matrix(datc) %*% rot.loadings  
               # project centered data matrix on rotated loadings
       rot.matrix <- x.rot$rotmat
       sdev <- apply(rot.scores,FUN=sd,MARGIN=c(2))

       res <- list(
          trunc = trunc,
          sdev = sdev,
          rot.loadings = rot.loadings,
          rot.scores = rot.scores,
          rot.matrix = rot.matrix)

       #transfer attributes to result
        res <- transfer.attributes(dat,res)
        
       return(res)
}


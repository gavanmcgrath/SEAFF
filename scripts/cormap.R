#'PCA Empirical Orthogonal Function Analysis
#'
#' \code{pca} returns the loadings, principal compnents, field correlations and variance explained.
#'
#'
#' @param data.mat A numeric matrix.
#' @param scores.mat Principal component scores from an eof of data.mat (time along rows)
#' @return  maps of correlations between data and PC scores
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
#' cors <- cormap(dat,eofs$scores)
#' }
#' 
cormap <- 
function(data.mat,scores.mat) {
# calculates correlation maps directly from data and scores matrices

    nt <- dim(data.mat)[1]
    nx <- dim(data.mat)[2]
    ns <- dim(scores.mat)[2]

    if(nt != dim(scores.mat)[1]) {
       stop("** ERROR ** dims in scores matrix and data matrix inconsistent")
    }

    # standardize data matrix
    mu <- apply(data.mat,FUN=mean,MARGIN=c(2))
    sdev <- apply(data.mat,FUN=sd,MARGIN=c(2))
    mu <- matrix(rep(mu,nt),nrow=nt,byrow=TRUE)
    sdev <- matrix(rep(sdev,nt),nrow=nt,byrow=TRUE)
    data.mat <- ((data.mat-mu)/sdev)
    rm(mu,sdev)

    # standardize scores matrix
    mu <- apply(scores.mat,FUN=mean,MARGIN=c(2))
    sdev <- apply(scores.mat,FUN=sd,MARGIN=c(2))
    mu <- matrix(rep(mu,nt),nrow=nt,byrow=TRUE)
    sdev <- matrix(rep(sdev,nt),nrow=nt,byrow=TRUE)
    scores.mat <- ((scores.mat-mu)/sdev)
    rm(mu,sdev)

    # calculate correlations by matrix product
    (t(data.mat) %*% scores.mat)/nt

}

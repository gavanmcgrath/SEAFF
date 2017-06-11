#'PCA Empirical Orthogonal Function Analysis
#'
#' \code{pca} returns the loadings, principal components, field correlations and variance explained.
#'
#'
#' @param dat A numeric or complex matrix.
#' @param corr A logical scalar. Should correlation matrix be used instead of the covariance matrix?
#'   Default = FALSE.
#' @param scale A numeric vector of data weights; of length 1; of length an integer multiple of ncol(dat) 
#'   ;or the same length as ncol(dat). Default = 1.
#' @param ceof A logical scalar to indicate Complex EOF should be performed on the data. Default = FALSE.  
#' @return  A list with:
#'  dat The original data matrix
#'  sdev The standard deviation of spatial locations.
#'  center The mean of spatial locations.
#'  scale The data scaling applied. From params.
#'  loadings The spatial pattern loading.
#'  scores The principal component scores.
#'  homcor Correlation of each PC with the data.
#'  n.obs Number of observation times.
#'  EVF Percentage of the explained variance of each PC. 
#'  CEVF Cumulative % explained variance.
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
pca <- function(dat,neof=50,weightbylat = FALSE, ceof=FALSE,
                nrot=NULL) {
  
  if (length(dim(dat))!=2) {
    stop("** ERROR ** only 2-dim data matrices allowed in pca")
  }
  
  # means
  center <- apply(dat,FUN=mean,MARGIN=c(2))
  ndef <- dim(dat)[1]
  ntot <- dim(dat)[2]
  mmin <- min(ndef,ntot,neof)
  neof <- mmin
  if (is.null(nrot)) nrot <- mmin
  
  # centred/scaled data matrix
  mu <- matrix(rep(center,each=dim(dat)[1]),nrow=dim(dat)[1])
  datc <- dat-mu #fix if complex?
  
  if (weightbylat) {
    lats <- attributes(dat)$lat[attributes(dat)$iys]
    weight <- sqrt(abs(cos(lats/180*pi)))
    weight <-rep(weight,each=dim(datc)[2]/length(weight))
  } else {
    weight <- attributes(dat)$lat[attributes(dat)$iys] * 0 + 1
    weight <-rep(weight,each=dim(datc)[2]/length(weight))
  }
  
  #If Complex EOF on a single data set
  if (ceof) datc <- hilbert(datc)
  
  # SVD decomposition
  if (is.complex(datc)) {
    dat.svd <- svd(Conj(t(datc)),nu=mmin,nv=mmin)
  } else {   
    dat.svd <- svd(t(datc),nu=mmin,nv=mmin)
  }    
  eof  = array(NA,dim=c(dim(datc)[2],mmin))
  eofi = array(NA,dim=c(dim(datc)[2],mmin))
  
  for ( n in 1:mmin ) {
    eof [,n] = dat.svd$u[,n]/weight*dat.svd$d[n]/sqrt(ntot-1)
    eofi[,n] = dat.svd$u[,n]*weight/dat.svd$d[n]*sqrt(ntot-1)
  }
  pc <- sqrt(ntot-1)*dat.svd$v[,(1:mmin)]
  
  # COMPUTE EXPLAINED VARIANCE
  vals <- dat.svd$d^2
  fexpvar <- dat.svd$d^2/sum(dat.svd$d^2)
  cum.fexpvar <- cumsum(fexpvar)
  
  # COMPUTE CONFIDENCE INTERVAL OF FEXPVAR
  alpha = 0.05
  stderr     <- qnorm(alpha/2,lower.tail=FALSE) * sqrt(2/ntot)
  fexpvar.ci <- cbind(fexpvar*(1-stderr),fexpvar*(1+stderr))
  
  if (is.complex(eof)) {
    amp.phs <- phaseAmp(eof,pc)
    spatial.phase <- amp.phs$spatial.phase
    spatial.amplitude <- amp.phs$spatial.amp
    temporal.phase <- amp.phs$temp.phase
    temporal.amplitude <- amp.phs$temp.amp
    rm(amp.phs)
    homcor <- NULL
  } else {
    spatial.phase <- NULL
    spatial.amplitude <- NULL
    temporal.phase <- NULL
    temporal.amplitude  <- NULL
    # homogeneous correlation maps
    homcor <- cormap(datc,pc)
  }
  
  res <- list(
    dat = dat,
    center = center,
    weight = weight,
    scale = center*0 + 1,
    sdev = sqrt(vals),
    eigen.vals = vals, 
    EVF = fexpvar,
    CEVF = cum.fexpvar,
    
    eof = eof,
    eofi = eofi,
    pc = pc,
    homcor = homcor,
    
    pcs.real = Re(pc),
    pcs.im = Im(pc),
    spatial.phase = spatial.phase,
    spatial.amplitude = spatial.amplitude,
    temporal.phase = temporal.phase,
    temporal.amplitude = temporal.amplitude
  )
  
  
  
  res <- transfer.attributes(dat,res)
  
  
  if (!is.complex(datc)){
    #Perform varimax rotation
    rotated.pc <- rot.pc(res,trunc=nrot,normalize=TRUE)
    res$rot.trunc <- rotated.pc$trunc
    res$rot.sdev <- rotated.pc$sdev
    res$rot.loadings <- rotated.pc$rot.loadings
    res$rot.scores <- rotated.pc$rot.scores
    res$rot.matrix <- rotated.pc$rot.matrix
  }
  return(res)
}


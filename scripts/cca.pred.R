cca.pred <- function(newdat,X,Y,tx,ty){
  
  #Demean
  x.mn <- colMeans(X)
  y.mn <- colMeans(Y)
  X2 <- X - rep(x.mn,each=nrow(X))
  Y2 <- Y - rep(y.mn,each=nrow(Y))

  lats.x <- attr(X,"lat")[attr(X,"iys")]
  lats.y <- attr(Y,"lat")[attr(Y,"iys")]
  
  X2 <- t(X2)
  Y2 <- t(Y2)
  
  #PCA
  x.eof <- eof.latlon(lats.x,X2, neof=tx)
  y.eof <- eof.latlon(lats.y,Y2, neof=ty)
  
  fx    <- x.eof$pc
  fy    <- y.eof$pc
  ex    <-  x.eof$eof
  ey    <- y.eof$eof
  
  #CCA
  cca.obj <- cca.normal(fx,fy,ex,ey,tx,ty) 
  
  #get "new" data
  x <- t(newdat)
  x <- x - x.mn
  
  #project data into a PC
  fx_new <-  t(x) %*%  x.eof$eofi[,1:tx]
  
  #calculate the regressor Bxy
  qx <- cca.obj$qx.tilde
  py <- cca.obj$py
  lam <- diag(cca.obj$can.cor)
  Bxy <- qx %*% lam[,1:min(tx,ty)] %*% t(py[,1:min(tx,ty)])
  
  #make the prediction
  fy_pred <- fx_new %*% Bxy
  
  #add back the climatological mean
  fy_pred <- fy_pred + rep(y.mn,each=ncol(x))
  
  fy_pred <- transfer.attributes(Y,fy_pred)
  
  fy_pred
}
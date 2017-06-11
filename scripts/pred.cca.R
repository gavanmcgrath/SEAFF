pred.cca <- function(newdat,cca.obj,
                     x.eof,y.eof,
                     x.mn=NULL,y.mn = NULL,
                     pred.what="y"){  
  
  if (is.null(x.mn)[1]) x.mn = x.eof$center
  if (is.null(y.mn)[1]) y.mn = y.eof$center
  
  tx <- cca.obj$tx
  ty <- cca.obj$ty
  #nxlbad <- !x.eof$lbad
  #nylbad <- !y.eof$lbad
  
  if (pred.what == "y"){
    x = t(newdat)
    x = x - x.mn

    #project data into a PC
    fx_new =  t(x) %*%  x.eof$eofi[,1:tx]

    #calculate the regressor Bxy
    qx <- cca.obj$qx.tilde
    print(paste("dim(qx)",dim(qx)))
    
    py <- cca.obj$py[,1:ty]
    lam <- diag(cca.obj$can.cor[1:min(tx,ty)])
    Bxy <- qx %*% lam %*% t(py)
    
   
    #make the prediction
    fy_pred <- fx_new %*% Bxy
    
    #add back the climatological mean
    fy_pred <- fy_pred + rep(y.mn,each=ncol(x))
    
    return(fy_pred)
    
  } else { 
    y = t(newdat)
    y <- y - y.mn
    
    #project data into a PC
    fy_new =  y %*%  y.eof$eofi[,1:ty]
    
    #calculate the regressor Bxy
    qy <- cca.obj$qy.tilde
    px <- cca.obj$px[,1:tx]
    lam <- diag(cca.obj$can.cor[1:min(tx,ty)])
    Byx <- qy %*% lam %*% t(px)
    
    
    #make the least squares estimate of the x anomaly
    fx_pred <- fy_new %*% Byx
    
    #add back the climatological mean
    fx_pred <- fx_pred + rep(x.mn,each=ncol(y))
    
    return(fx_pred)
  }
}



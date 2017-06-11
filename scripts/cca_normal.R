cca.normal = function(fx,fy,ex,ey,tx,ty) {
   ########################################################
   ## PERFORMS CANONICAL CORRELATION ANALYSIS ON X AND Y.
   ## X AND Y ARE ASSUMED TO BE REPRESENTED IN THE FOLLOWING FORMS:
   ## X = FX %*% EX^T ### AND ### Y = FY %*% EY^T
   ## FOR EXAMPLE: FROM PRINCIPAL COMPONENT ANALYSIS
   ## INPUT:
   # FX[NTOT,MX]: TIME SERIES FOR THE X-COMPONENTS
   # FY[NTOT,MY]: TIME SERIES FOR THE Y-COMPONENTS
   # EX[SX,MX]: SPATIAL STRUCTURES OF THE X-COMPONENTS
   # EY[SY,MY]: SPATIAL STRUCTURES OF THE Y-COMPONENTS
   # TX: TRUNCATION FOR X (TX <= MX)
   # TY: TRUNCATION FOR Y (TY <= MY)
   ## OUTPUT LIST:
   # CAN.COR[MIN(TX,TY)]: CANONICAL CORRELATIONS
   # RX[NTOT,MIN(TX,TY)]: CANONICAL VARIATES FOR X
   # RY[NTOT,MIN(TX,TY)]: CANONICAL VARIATES FOR Y
   # PX[SX ,MIN(TX,TY)]: CANONICAL LOADING VECTORS FOR X
   # PY[SY ,MIN(TX,TY)]: CANONICAL LOADING VECTORS FOR Y
   # QX.TILDE[TX,MIN(TX,TY)]: WEIGHTING VECTORS FOR X-FEATURES
   # QY.TILDE[TY,MIN(TX,TY)]: WEIGHTING VECTORS FOR Y-FEATURES
   # TX, TY: VALUES OF TX AND TY
   ########################################################
  
  ntot = dim(fx)[1]
  mx = dim(fx)[2]
  my = dim(fy)[2]
  if ( ntot != dim(fy)[1]) {
    stop('fx and fy have inconsistent time dimension')
  }
  if (tx <= mx) fx = fx[,1:tx] else stop('tx set too large')
  if (ty <= my) fy = fy[,1:ty] else stop('ty set too large')
  
  fx = t(t(fx) - colMeans(matrix(fx,ncol=tx)))
  fy = t(t(fy) - colMeans(matrix(fy,ncol=ty)))
  
  cov.xx = t(fx) %*% fx / (ntot-1)   #13.24
  cov.yy = t(fy) %*% fy / (ntot-1)
   cov.xy = t(fx) %*% fy / (ntot-1)   #13.23
   
   #chol.yy <- chol(cov.yy,pivot=TRUE)
   #pivot <- attr(chol.yy, "pivot")
   #cov.yy.inv = chol2inv(chol.yy[, order(pivot)])
   cov.yy.inv = chol2inv(chol(cov.yy))  #original
   
   #Eq. 13.10 in DelSole notes on CCA
   gev.list = gev(cov.xy %*% cov.yy.inv %*% t(cov.xy), cov.xx)
   qx.tilde = gev.list$q
   
   
   gev.list$lambda[gev.list$lambda<0] <- 0   #check if ok
   can.cor = sqrt(gev.list$lambda)
   #Eq. 13.11 in DelSole notes on CCA
   qy.tilde = cov.yy.inv %*% t(cov.xy) %*% qx.tilde
   
   for ( n in 1:min(tx,ty)) {
     qy.tilde[,n] =
       qy.tilde[,n] / sqrt(as.numeric(t(qy.tilde[,n]) %*% 
                                        cov.yy %*% qy.tilde[,n]))
   }
   rx = fx %*% qx.tilde  #Eq. 13:30
   ry = fy %*% qy.tilde  #Eq. 13.31
   #Check that 1/(N-1)*t(rx) %*% rx  = 1  Eq. 13.32
   px = ex[,1:tx] %*% cov.xx %*% qx.tilde   #Eq. 13.44
   py = ey[,1:ty] %*% cov.yy %*% qy.tilde   #Eq. 13.45
   
   list(can.cor=can.cor,  #Canonical correlations
        rx=rx,ry=ry,      #Canonical variates
        px=px,py=py,      #Canonical loading vectors
        qx.tilde=qx.tilde,qy.tilde=qy.tilde,  #normalised canonical projections
        tx=tx,ty=ty)
}

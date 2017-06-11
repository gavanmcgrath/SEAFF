cca2 <- function(x.pca,y.pca,
                 x.data,y.data,
                 tx=2,ty=2){
 # x.match <- which(attr(x.pca,"date") %in% attr(y.pca,"date"))
  #y.match <- which(attr(y.pca,"date") %in% attr(x.pca,"date"))
  
  
  center.x <- x.pca$center
  center.y <- y.pca$center
  
  ############## PROJECT EOFS ONTO WHOLE DATA SET ##########################
  x.pcs  = x.data %*% x.pca$eofi
  y.pcs = y.data %*% y.pca$eofi
  
  #x.pcs = x.pcs[x.match,]
  #y.pcs = y.pcs[y.match,]
  ############## DEFINE FX, FY, EX, FY MATRICES ##########################
  fx    =   x.pcs    ;
  fy    = y.pcs
  ex    =   x.pca$eof; 
  ey    = y.pca$eof
  
  ############## REMOVE CLIMATOLOGIES ##########################
  fx    = t(t(fx) - colMeans(fx))
  fy    = t(t(fy) - colMeans(fy))
  
  res.cca <- cca.normal(fx,fy,ex,ey,tx,ty)
  px <- res.cca$px
  py <- res.cca$py
  rx <- res.cca$rx
  ry <- res.cca$ry
  rho = res.cca$can.cor 
  #A test of CCA significance
  #require(CCP)
  #cca.sig  <- p.asym(rho, dim(dat.x)[1],dim(dat.x)[2], dim(dat.y)[2], tstat = "Pillai")
  #cca.sig <- NA
  
  # correlation maps
  #print("cca2.R: Calculating het and hom correlation maps")
  homcor.x <- NA #cormap(t(x.data),rx)
  homcor.y <- NA #cormap(t(y.data),ry)
  hetcor.x <- NA #cormap(t(x.data),ry)
  hetcor.y <- NA #cormap(t(y.data),rx)
  
  nyrs <- dim(x.data)[1]
  weight.x <- x.pca$weight
  weight.y <- y.pca$weight
  VX_tot <- 1/(nyrs-1)*sum(diag(x.data %*% diag(weight.x) %*% t(x.data)))
  VY_tot <- 1/(nyrs-1)*sum(diag(y.data %*% diag(weight.y) %*% t(y.data)))
  exp.var.x <- exp.var.y <- vector("numeric",length=length(rho))
  for (n in 1:length(rho)) {
    exp.var.x[n] <- t(px[,n]) %*% diag(weight.x) %*% px[,n] 
    exp.var.y[n] <- t(py[,n]) %*% diag(weight.y) %*% py[,n] 
  }
  fexp.var.x <- exp.var.x/VX_tot
  fexp.var.y <- exp.var.y/VY_tot
  cfexp.var.x <- cumsum(fexp.var.x)
  cfexp.var.y <- cumsum(fexp.var.y)
  
  center.x <- transfer.attributes(x.pca,center.x)
  center.y <- transfer.attributes(y.pca,center.y)
  
  list(
    center.left = center.x,
    center.right = center.y,
    left.singular.vectors = px,
    right.singular.vectors = py,
    coeff.left = rx,
    coeff.right = ry,
    left.homcor = homcor.x,
    right.homcor = homcor.y,
    left.hetcor = hetcor.x,
    right.hetcor = hetcor.y,
    SCF = rho,
    CSCF = cumsum(rho),
    EVF.left = fexp.var.x,
    EVF.right =fexp.var.y,
    CEVF.left = cfexp.var.x,
    CEVF.right = cfexp.var.y,
    rho = rho #,  #correlations?
    # stat = cca.sig$stat, #Pillai-Bartlett Trace
    # df1 = cca.sig$df1, #numerator degofreedom for the F-approx
    # df2 = cca.sig$df2, #denominator degoffreedom for the F-approx
    # approx = cca.sig$approx, #value of F-approxfor the statistic
    # p.value = cca.sig$p.value #p-value
  )
  
  
}
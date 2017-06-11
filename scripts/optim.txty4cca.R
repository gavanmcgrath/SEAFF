optim.txty4cca <- function(dat.x,dat.y,maxlag=0){
  
  mod.dir <- "./models/models.custom/temp/"
  #dat.x   N * Mx
  #dat.y   N * My
  #Assume it is already centered with attribute center
  #Assume end dates match, start dates can differ
  #Uses weights for X but not Y.
  
  ############## SET LAG FOR PREDICTION ##################################
  for (tau in 0:maxlag){
    #print(paste("lag = ",tau))
    
    #lag the predictor for complete data set
    n.y.compare <- (1+tau):(dim(dat.y)[1])  
    n.x.compare <- (dim(dat.x)[1]-length(n.y.compare)+1-tau):(dim(dat.x)[1]-tau)
  
    #index to verification set 
    ndxy.mid <- floor(dim(dat.y)[1]/2)
    n.verif.y <- (ndxy.mid+1+tau):dim(dat.y)[1]
    n.verif.x <- (dim(dat.x)[1]-length(n.verif.y)+1-tau):(dim(dat.x)[1]-tau)
    
    #index to the x eof development set
    n.eof.setup.x <- 1:(n.verif.x[1]-1)
    
    #index to the training set
    n.train.y <- (1+tau):(n.verif.y[1]-1)
    n.train.x <- rev(seq(n.verif.x[1]-1,by=-1,length.out=length(n.train.y)))
    index_2xtrain_in_Xeof <-  which(n.eof.setup.x  %in% n.train.x)
    
    X.train <- dat.x[n.train.x,]
    X.train <- transfer.attributes(dat.x,X.train)
    attr(X.train,"date") <- attr(X.train,"date")[n.train.x]
    attr(X.train,"time") <- attr(X.train,"time")[n.train.x,]
    
    Y.train <- dat.y[n.train.y,]
    Y.train <- transfer.attributes(dat.y,Y.train)
    attr(Y.train,"date") <- attr(Y.train,"date")[n.train.y]
    attr(Y.train,"time") <- attr(Y.train,"time")[n.train.y,]
    
    X.verif <- dat.x[n.verif.x,]
    Y.verif <- dat.y[n.verif.y,]
    
    nt       = length(n.verif.y)
    
    #Calculate x eof and PC
    X.eof.test <- transfer.attributes(dat.x,dat.x[n.eof.setup.x,])
    attr(X.eof.test,"date") <- attr(X.eof.test,"date")[n.eof.setup.x]
    attr(X.eof.test,"time") <- attr(X.eof.test,"time")[n.eof.setup.x,]
    
    
    # Calc. xeofs on extended training set
    X.eof <- pca(X.eof.test,weightbylat = TRUE)
    X.pcs  =  X.eof.test %*% X.eof$eofi
    
    # Calc. y eofs and pcs on training set
    Y.eof <- pca(Y.train,weightbylat = FALSE)
    Y.pcs  =  Y.train %*% Y.eof$eofi
    
    # loop over the set of possible tx and ty
    txty.set <- expand.grid(
      2:min(c(30,length(n.train.x), length(attr(dat.x,"ixs")))),
      2:min(c(10,length(n.train.y), length(attr(dat.y,"ixs")))))
    txty.set <- txty.set[sort(rowSums(txty.set),index.return=TRUE)$ix,]
    
    r.df <- c()
    for (j in 1:nrow(txty.set)){
      #assign tx and ty then pass to cca_norm
      tx <- txty.set[j,1] 
      ty <- txty.set[j,2]
      n.txty <- tx + ty
      ############## DEFINE FX, FY, EX, FY MATRICES ################
      fx    =   X.pcs[index_2xtrain_in_Xeof,];  fy   = Y.pcs
      ex    =   X.eof$eof ;                     ey   = Y.eof$eof
      exi   =   X.eof$eofi;                     eyi  = Y.eof$eofi
      
      ############## REMOVE CLIMATOLOGIES ##########################
      fx    = t(t(matrix(fx[,1:tx],ncol=tx)) - colMeans(matrix(fx[,1:tx],ncol=tx))) 
      #Should not be necessary on pcs 
      fy    = t(t(matrix(fy[,1:ty],ncol=ty)) - colMeans(matrix(fy[,1:ty],ncol=ty)))
      
      ####################### CCA ##################################
      cca.res <- cca.normal(fx,fy,ex,ey,tx,ty)
      
      #Compute the correlations between variates for training data
      rho_train <- cca.res$can.cor[1]
      
      #Project Verification data onto the inverse of the eofs
      Fx_verif <- X.verif %*% matrix(exi[,1:tx],ncol=tx)
      Fy_verif <- Y.verif %*% matrix(eyi[,1:ty],ncol=ty)
      
      #remove climatologies ?? Should not be necessary ??
      Fx_verif    = t(t(matrix(Fx_verif[,1:tx],ncol=tx)) - colMeans(matrix(Fx_verif[,1:tx],ncol=tx)))
      Fy_verif    = t(t(matrix(Fy_verif[,1:ty],ncol=ty)) - colMeans(matrix(Fy_verif[,1:ty],ncol=ty)))
      
      #Compute the correlations between the leading variate
      rx_verif = Fx_verif %*% cca.res$qx.tilde
      ry_verif = Fy_verif %*% cca.res$qy.tilde
      
      rho_verif <- cor(rx_verif[,1],ry_verif[,1])
      
      #Z Scores on correlations and confidence intervals on correlations
      #if (rho_train >= 1) print(paste("rho_train",rho_train,"tx",tx,"ty",ty))
      zy_t = 0.5*log((1+rho_train)/(1-rho_train))
      zy_v = 0.5*log((1+rho_verif)/(1-rho_verif))
      alpha = 0.1
      conf.int = qnorm(log(c(alpha/2,1-alpha/2)), 
                       mean = zy_v, 
                       sd = sqrt(1/(nrow(fx) - 3)),
                       log.p=TRUE)
      zy_v_up95 <- conf.int[2] 
      zy_v_lo95 <- conf.int[1]
      ry_v_lo95 <- (exp(2*zy_v_lo95)-1)/(exp(2*zy_v_lo95)+1)
      ry_v_up95 <- (exp(2*zy_v_up95)-1)/(exp(2*zy_v_up95)+1)
      
      #save the stats of all combinations of TX and TY
      corr.stats <- c(rho_train, rho_verif,
                      ry_v_up95, ry_v_lo95,
                      zy_t,      zy_v,          
                      zy_v_up95, zy_v_lo95,
                      n.txty,    tx,        ty)
      r.df <- rbind(r.df,corr.stats)
      # loop back to #3 for next combination
    }
    
    colnames(r.df) <- c("r_t",   "r_v", "r_v_up95", "r_v_lo95",
                        "z_t",   "z_v","z_v_up95","z_v_lo95",
                        "n.txty","tx",  "ty")
    
    saveRDS(r.df,file = paste(mod.dir,"rdf.rds",sep=""))
    
    #png(file=paste("./figs/cca/Z_Scores_atLag_",tau,".png",sep=""))
    #par(oma=c(4,4,0.1,4),mar=c(0,0,0,0))
    #plot(r.df[,"n.txty"],r.df[,"z_t"],ylim=c(0,3.5),
    #     ylab="Z-score",xlab="Tx+Ty")
    #points(r.df[,"n.txty"],r.df[,"z_v"],pch=20)
    #axis(side=4, at=0.5*log((1+c(seq(0,0.9,by=0.1)))/(1-seq(0,0.9,by=0.1))),labels = seq(0,0.9,by=0.1))
    #mtext(side=4,line=2,"Correlation")
    #print(r.df[,"r_t"])
    mx.r <- which(r.df[,"r_v"] == max(r.df[,"r_v"],na.rm=TRUE))[1]
    #arrows(x0 = r.df[mx.r,"n.txty"], x1 = r.df[mx.r,"n.txty"],
    #       y0 = r.df[mx.r,"z_v_lo95"],y1 = r.df[mx.r,"z_v_up95"],
    #       code=3,angle=90)
    #text(6,3,paste("lag = ",tau))
    
    #Following Hastie et al. (2009), select the simplest model whose
    # correlation is within one standard error of the best model's r
    z.max.lower <- r.df[mx.r,"z_v_lo95"] 
    if (length(mx.r) == 0) mx.r=1 
    simp.mod <- which(r.df[1:mx.r,"z_v"] >= z.max.lower)[1]
    #Fix to check that one exists, if not the select the next closest??
    tx <-  r.df[simp.mod,"tx"]
    ty <-  r.df[simp.mod,"ty"]
    #points(tx+ty,r.df[simp.mod,"z_v"],pch=17,cex=2,col="red")
    #text(tx+ty,r.df[simp.mod,"z_v"], 
    #     paste("tx = ",tx,", ty = ",ty,sep=""),pos=4,col="red")
    #dev.off()
    
    #Use these tx, ty to recompute the CCA for the full period
    fx_full =  dat.x[n.x.compare,] %*%  exi[,1:tx] 
    fy_full =  dat.y[n.y.compare,] %*%  eyi[,1:ty]
    cca.res <- cca.normal(fx_full,fy_full,ex,ey,tx,ty)
    
    px <- cca.res$px;   py <- cca.res$py;
    fx <- cca.res$fx;   fy <- cca.res$fy 
    ex <- cca.res$ex;   ey <- cca.res$ey  
    exi<- cca.res$exi;  eyi<- cca.res$eyi
    can.cor <- cca.res$can.cor
    
    #compute the significance that a component is different
    # from a random variable using MonteCarlo
    set.seed(1)
    ntrials = 10000
    cor.trials = array(NA,dim=c(min(tx,ty),ntrials))
    for (n in 1:ntrials) {
      x = array(rnorm(nt*tx),dim=c(tx,nt))
      y = array(rnorm(nt*ty),dim=c(ty,nt))
      x.svd = svd(x-rowMeans(x))
      y.svd = svd(y-rowMeans(y))
      cor.trials[,n] = svd(t(x.svd$v) %*% y.svd$v)$d
    }
    cor.crit = numeric(min(tx,ty))
    for ( n in 1:min(tx,ty)) {
      cor.crit[n] = quantile(cor.trials[n,],probs=0.99)
    }
    
    n.sig <- can.cor[1:min(tx,ty)] >  cor.crit
    mntxty <- 1:min(tx,ty)
    saveRDS(list(mntxty=mntxty,can.cor=can.cor,cor.crit=cor.crit),
            paste(mod.dir,"ccatxtysig.rds",sep=""))
    
    
    #png(file=paste("./figs/cca/SelctdVarCor_Lag_",tau,".png",sep=""))
    #plot(can.cor[mntxty],type="l",lwd=2,
    #     ylab="Correlation",xlab="Variate",
    #     ylim=c(0,1),xlim=c(0,max(mntxty)))
    #points(can.cor[mntxty],pch=20,cex=2)
    #lines(c(0:max(mntxty)),c(1,cor.crit),lty=2,lwd=2)
    #legend("topright",lty=1:2,legend = c("Optimised model","95% CI Noise"))
    #dev.off()
    
    weight.x <- X.eof$weight
    weight.y <- Y.eof$weight
    
    #Percent of variance explained by the canonical patterns
    VX_tot <- 1/(nt-1)*sum(diag( dat.x[n.x.compare,] %*% diag(weight.x) %*% t( dat.x[n.x.compare,])))
    VY_tot <- 1/(nt-1)*sum(diag( dat.y[n.y.compare,] %*% diag(weight.y) %*% t( dat.y[n.y.compare,])))
    exp.var.x <- exp.var.y <- vector("numeric",length=length(can.cor))
    for (n in 1:length(can.cor)) {
      exp.var.x[n] <- t(px[,n]) %*% diag(weight.x) %*% px[,n] 
      exp.var.y[n] <- t(py[,n]) %*% diag(weight.y) %*% py[,n] 
    }
    fexp.var.x <- exp.var.x/VX_tot
    fexp.var.y <- exp.var.y/VY_tot
    
    
    #save relevant models and associated results 
    
    results.list <- list(
      cca.mod = cca.res,
      fexp.var.x = fexp.var.x, 
      fexp.var.y = fexp.var.y,
      VX_tot =  VX_tot, 
      VY_tot =  VY_tot,
      n.sig = n.sig, 
      cor.crit = cor.crit,
      tx = tx,
      ty = ty,
      r.df = r.df,
      alpha = alpha,
      x.train.dates =  attr(X.train,"date"),
      y.train.dates =  attr(Y.train,"date"),
      x.verif.dates =  attr(X.verif,"date"),
      y.verif.dates =  attr(Y.verif,"date")
    )
    
    saveRDS(results.list,file=paste(mod.dir,"SelectedCCAModel_Lag_",tau,".rds",sep=""))
    
    #repeat from 0 for next lag
  }
  if (maxlag==0) results.list
}


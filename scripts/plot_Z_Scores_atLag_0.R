plot_Z_Scores_atLag_0 <- function(r.df){
  par(oma=c(4,4,0.1,4),mar=c(0,0,0,0))
  plot(x=r.df[,"n.txty"],y=r.df[,"z_t"],
      ylim=c(0,3.5),
      ylab="",xlab="")
 
  points(x=r.df[,"n.txty"],y=r.df[,"z_v"],pch=20)

  mx.r <- which(r.df[,"r_v"] == max(r.df[,"r_v"],na.rm=TRUE))[1]
  arrows(x0 = r.df[mx.r,"n.txty"], x1 = r.df[mx.r,"n.txty"],
       y0 = r.df[mx.r,"z_v_lo95"],y1 = r.df[mx.r,"z_v_up95"],
       code=3,angle=90)
  
  #Following Hastie et al. (2009), select the simplest model whose
  # correlation is within one standard error of the best model's r
  z.max.lower <- r.df[mx.r,"z_v_lo95"] 
  if (length(mx.r) == 0) mx.r=1 
  simp.mod <- which(r.df[1:mx.r,"z_v"] >= z.max.lower)[1]
  #Fix to check that one exists, if not the select the next closest??
  tx <-  r.df[simp.mod,"tx"]
  ty <-  r.df[simp.mod,"ty"]
  points(x=tx+ty,y=r.df[simp.mod,"z_v"],pch=17,cex=2,col="red")
  text(tx+ty,r.df[simp.mod,"z_v"], 
     paste("tx = ",tx,", ty = ",ty,sep=""),pos=4,col="red")
  
  axis(side=4, at=0.5*log((1+c(seq(0,0.9,by=0.1)))/(1-seq(0,0.9,by=0.1))),
       labels = seq(0,0.9,by=0.1))
  mtext(side=4,line=2,"Correlation")
  mtext(side=1,line=2,"Tx+Ty")
  mtext(side=2,line=2,"Z-score")
  
}
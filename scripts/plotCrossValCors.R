plotCrossValCors <- function(mod.dir){
  
  if (file.exists(paste(mod.dir,"rdf.rds",sep=""))){
    r.df <- readRDS(paste(mod.dir,"rdf.rds",sep=""))

    par(oma=c(4,4,0.1,4),mar=c(0,0,0,0))
    plot(r.df[,"n.txty"],r.df[,"z_t"],ylim=c(0,3.5),
       ylab="Z-score",xlab="Tx+Ty")
    points(r.df[,"n.txty"],r.df[,"z_v"],pch=20)
    axis(side=4, at=0.5*log((1+c(seq(0,0.9,by=0.1)))/(1-seq(0,0.9,by=0.1))),
         labels = seq(0,0.9,by=0.1))
    mtext(side=4,line=2,"Correlation")

    mx.r <- which(r.df[,"r_v"] == max(r.df[,"r_v"],na.rm=TRUE))[1]
    arrows(x0 = r.df[mx.r,"n.txty"], x1 = r.df[mx.r,"n.txty"],
         y0 = r.df[mx.r,"z_v_lo95"],y1 = r.df[mx.r,"z_v_up95"],
         code=3,angle=90)
    z.max.lower <- r.df[mx.r,"z_v_lo95"] 
    if (length(mx.r) == 0) mx.r=1 
    simp.mod <- which(r.df[1:mx.r,"z_v"] >= z.max.lower)[1]
  
    tx <-  r.df[simp.mod,"tx"]
    ty <-  r.df[simp.mod,"ty"]
    points(tx+ty,r.df[simp.mod,"z_v"],pch=17,cex=2,col="red")
    text(tx+ty,r.df[simp.mod,"z_v"], 
         paste("tx = ",tx,", ty = ",ty,sep=""),pos=4,col="red")
    
  } else {
    par(oma=c(4,4,0.1,4),mar=c(0,0,0,0))
    plot(NA,ylim=c(0,3.5),ylab="Z-score",xlab="Tx+Ty")
    axis(side=4, at=0.5*log((1+c(seq(0,0.9,by=0.1)))/(1-seq(0,0.9,by=0.1))),
         labels = seq(0,0.9,by=0.1))
    mtext(side=4,line=2,"Correlation")
    text(6,3,"No Data")
  }
  
}

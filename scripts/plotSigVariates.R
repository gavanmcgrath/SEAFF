plotSigVariates <- function(mod.dir){
  
  if (file.exists(paste(mod.dir,"ccatxtysig.rds",sep=""))) {
    
    cca.res <-  readRDS(paste(mod.dir,"ccatxtysig.rds",sep=""))
    can.cor <- cca.res$can.cor
    mntxty <- cca.res$mntxty
    cor.crit <- cca.res$cor.crit
  
    plot(can.cor[mntxty],type="l",lwd=2,
      ylab="Correlation",xlab="Variate",
      ylim=c(0,1),xlim=c(0,max(mntxty)))
    points(can.cor[mntxty],pch=20,cex=2)
    lines(c(0:max(mntxty)),c(1,cor.crit),lty=2,lwd=2)
    legend("topright",lty=1:2,legend = c("Optimised model","95% CI Noise"))
  } else {
    plot(NA,ylab="Correlation",xlab="Variate",
         ylim=c(0,1),xlim=c(0,2))
    text(1,0.5, "No Data")
  }
}



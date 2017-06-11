plot.ccavariate.sig <- function(mntxty,can.cor,cor.crit) {
  plot(can.cor[mntxty],type="l",lwd=2,
     ylab="Correlation",xlab="Canonical Variate #",
     ylim=c(0,1),xlim=c(0,max(mntxty)))
  points(can.cor[mntxty],pch=20,cex=2)
  lines(c(0:max(mntxty)),c(1,cor.crit),lty=2,lwd=2)
  legend("topright",lty=1:2,lwd=2,pch=c(20,NA),legend = c("Optimised model","95% CI Noise"))
}


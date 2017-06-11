#1. Plot regression of slopes of multiple linear regression of 
#2. Plot Australian map with regression coefficients
#3. Plot 2D Image of regression coefficients in SEA as a function of lag >= 3

#3 yr moving avergaed, logged SEA flows
#vs
#3 yr moving averaged, detrended, lagged (3 yr and 13 yr) ssts 
CorrPlots <- function(){
source("./scripts/lm.Q.SST12.R")
res <- lm.Q.SST12()

rbPal <- colorRampPalette(c('blue','red'))
rnames <- rownames(res)
cols <- rbPal(10)[as.numeric(cut(res["ar2",],breaks = 10))]

p1 <- 0.05
pos <- res["p1",]< p1 & res["p2",] < p1
par(mar=c(0,0,0,0),oma=c(4,4,0.5,0.5))
plot(res["m2",pos],
     res["m1",pos], col = cols[pos], 
     pch=20, 
     xlab = "" ,
     ylab = "",
     xlim = range(res["m2",],na.rm=TRUE),
     ylim = range(res["m1",],na.rm=TRUE))
mtext(side=1,line=3,expression(paste("Slope ",SSTa[2],sep="")))
mtext(side=2,line=2.5,expression(paste("Slope ",SSSTa[1],sep="")))
library(segmented)
lindsdf <- data.frame(x = res["m2",pos], y = res["m1",pos])
linds.lm <- lm(y ~ x, data = lindsdf)

approx.breakpoint <- c(0.1)
linds.seg <- segmented(linds.lm, 
                       seg.Z = ~ x, psi = list(x = approx.breakpoint))
x <- res["m2",pos]
x.brk <- linds.seg$psi[2]
x1 <- sort(x[x < x.brk ])
y1 <- intercept(linds.seg)$x[1] + slope(linds.seg)$x[1]*x1
x2 <- sort(x[x >= x.brk ])
y2 <- intercept(linds.seg)$x[2] + slope(linds.seg)$x[2]*x2


lines(c(x1,x2),c(y1,y2),lwd=2,lty=2,col="black")
abline(0,0)
lines(c(0,0),c(-999,999))
legend("bottomleft",legend = c(expression(paste(r^2,"<0.3",sep="")),
                               expression(paste("0.3<",r^2,"<0.5",sep="")),
                               expression(paste("0.5<",r^2,sep=""))),
       pch = 20,col=c(rbPal(10)[c(2,5,8)]),bty = "n")
summary(linds.seg)


par(oma=c(3,3.5,0.5,0.5),mar=c(0,0,0,0))
plot(NA,type="n",xlim=c(114,155),ylim=c(-45,-9),xlab="",ylab="",asp=34/43,
     xaxt="n",yaxt="n")
plot.land2()
points(attr(res,"lon"),attr(res,"lat"),col=cols,pch=20)
at <- c(120,130,140,150)
L <- parse(text = paste(at, "*degree ~ E", sep = ""))
axis(1, at = at, labels = L,las=1)
axis(3, at = at, labels = NA)
at <- c(-20,-30,-40)
L <- parse(text = paste(-at, "*degree ~ S", sep = ""))
axis(2, at = at, labels = L,las=1)
axis(4, at = at, labels = NA)
legend("bottomleft",legend = c(expression(paste(r^2,"<0.3",sep="")),
                               expression(paste("0.3<",r^2,"<0.5",sep="")),
                               expression(paste("0.5<",r^2,sep=""))),
       pch = 20,col=c(rbPal(10)[c(2,5,8)]),bty = "n")

n <- dim(sst)[3]
sst.sd <- apply(sst,MARGIN=c(1,2), FUN = function(x) {
  pos <- seq(from=1,to=n,by=12)
  x[pos] <- (x[pos] - mean(x[pos],na.rm=TRUE))
  pos <- seq(from=2,to=n,by=12)
  x[pos] <- (x[pos] - mean(x[pos],na.rm=TRUE))
  pos <- seq(from=3,to=n,by=12)
  x[pos] <- (x[pos] - mean(x[pos],na.rm=TRUE))
  pos <- seq(from=4,to=n,by=12)
  x[pos] <- (x[pos] - mean(x[pos],na.rm=TRUE))
  pos <- seq(from=5,to=n,by=12)
  x[pos] <- (x[pos] - mean(x[pos],na.rm=TRUE))
  pos <- seq(from=6,to=n,by=12)
  x[pos] <- (x[pos] - mean(x[pos],na.rm=TRUE))
  pos <- seq(from=7,to=n,by=12)
  x[pos] <- (x[pos] - mean(x[pos],na.rm=TRUE))
  pos <- seq(from=8,to=n,by=12)
  x[pos] <- (x[pos] - mean(x[pos],na.rm=TRUE))
  pos <- seq(from=9,to=n,by=12)
  x[pos] <- (x[pos] - mean(x[pos],na.rm=TRUE))
  pos <- seq(from=10,to=n,by=12)
  x[pos] <- (x[pos] - mean(x[pos],na.rm=TRUE))
  pos <- seq(from=11,to=n,by=12)
  x[pos] <- (x[pos] - mean(x[pos],na.rm=TRUE))
  pos <- seq(from=12,to=n,by=12)
  x[pos] <- (x[pos] - mean(x[pos],na.rm=TRUE))
  sd(x)
})

library(palr)
sstcols <- sstPal(palette = TRUE)
image(attr(sst,"lon"),rev(attr(sst,"lat")),sst.sd[,seq(89,1,by=-1)],xaxt="n",yaxt="n",
      ylim=c(-60,75),col=sstcols$cols)
plot.land2()
at <- seq(30,330,by=60)
L <- parse(text = paste(at, "*degree", sep = ""))
axis(1, at = at, labels = L,las=1)
axis(3, at = at, labels = NA)
at <- seq(-60,60,by=30)
L <- parse(text = paste(at, "*degree", sep = ""))
axis(2, at = at, labels = L,las=1)
axis(4, at = at, labels = NA)
rbind(c(202,-46),c(314,22))
points(202,-46,pch=3,cex=2,col="black",lwd=3)
points(314,22,pch=3,cex=2,col="black",lwd=3)
text(216,-46,"1",cex=2,col="black",lwd=3)
text(328,22,"2",cex=2,col="black",lwd=3)


temps <- rev(c(0, 0.2,0.4,0.6,0.8,1,1.2,1.4,1.6))
op <- par(xpd = NA)
plot(1:20,type="n",axes=FALSE,xlab="",ylab="")
legend("bottom", horiz = TRUE,legend = rev(temps), fill = sstcols$cols[round(seq(2,length(sstcols$cols),length.out=length(temps)))],cex=0.8)

index2SEA <- which(attr(res,"lon")<155 & attr(res,"lon")>145 & 
                     attr(res,"lat") < -34 &  attr(res,"lat") > -40)
flows2 <- flows[, index2SEA]
attr(flows2,"lat") <- attr(flows,"lat")[index2SEA]
attr(flows2,"lon") <- attr(flows,"lon")[index2SEA]
attr(flows2,"time") <- attr(flows,"time")
flows <- flows2
lags <- (3*12):237 # (dim(seaResults)[2]+3*36)
n <- length(lags)
seaResults <- matrix(NA,nrow=n,ncol=n)
all.lags <- expand.grid(lags,lags)
for (i in 1:length(lags)) {
  for (j in 1:length(lags)){
    seaResults[i,j] <- mean(lm.Q.SST12(lags[i],lags[j]))
    print(c(lags[i],lags[j]))
  }
}

flows <- readRDS("./data/processed/flows.rds")

png(filename ="./figs/SST12CorrsMultiLags.png",width=362,height=362)
par(mar=c(0,0,0,0),oma=c(4,4,0.5,0.5))
image(lags,lags,seaResults ) #,col=sstcols$cols,xlab="",ylab="",asp=1)
mtext(side=1,line=3,expression(paste(SSTa[1], " Lag (months)",sep="")))
mtext(side=2,line=2.5,expression(paste(SSTa[2], " Lag (months)",sep="")))
dev.off()


temps <- c(0,0.02,0.04,0.06,0.08,0.10,0.12,0.14,0.16,0.18,0.2)
op <- par(xpd = NA)
plot(1:20,type="n",axes=FALSE,xlab="",ylab="")
legend("bottom", horiz = FALSE,legend = rev(temps), fill = rev(sstcols$cols[round(seq(2,length(sstcols$cols),length.out=length(temps)))]),cex=0.7)



png(filename ="./figs/SEA_Cor_lag110_150.png",width=250,height=250)
res <- lm.Q.SST12(lag1 = 168, lag2 = 72)
cols <- rbPal(10)[as.numeric(cut(res["ar2",],breaks = c(seq(0,0.5,length.out=9),1)))]
par(oma=c(3,3.5,0.5,0.5),mar=c(0,0,0,0))
plot(NA,type="n",xlim=c(145,152),ylim=c(-40,-30),xlab="",ylab="",asp=15/16,
     xaxt="n",yaxt="n")
plot.land2()
points(attr(res,"lon"),attr(res,"lat"),col=cols,pch=20,cex=1.5)
at <- c(145,150)
L <- parse(text = paste(at, "*degree ~ E", sep = ""))
axis(1, at = at, labels = L,las=1)
axis(3, at = at, labels = NA)
at <- c(-30,-35,-40)
L <- parse(text = paste(-at, "*degree ~ S", sep = ""))
axis(2, at = at, labels = L,las=1)
axis(4, at = at, labels = NA)
legend("bottomright",legend = c(expression(paste(r^2,"<0.3",sep="")),
                               expression(paste("0.3<",r^2,"<0.5",sep="")),
                               expression(paste("0.5<",r^2,sep=""))),
       pch = 20,col=c(rbPal(10)[c(2,5,8)]),bty = "n")
dev.off()

png(filename ="./figs/SEA_Cor_lag70_170.png",width=250,height=250)
res <- lm.Q.SST12(lag1 = 50, lag2 = 140)
cols <- rbPal(10)[as.numeric(cut(res["ar2",],breaks = c(seq(0,0.5,length.out=9),1)))]
par(oma=c(3,3.5,0.5,0.5),mar=c(0,0,0,0))
plot(NA,type="n",xlim=c(145,152),ylim=c(-40,-30),xlab="",ylab="",asp=15/16,
     xaxt="n",yaxt="n")
plot.land2()
points(attr(res,"lon"),attr(res,"lat"),col=cols,pch=20,cex=1.5)
at <- c(145,150)
L <- parse(text = paste(at, "*degree ~ E", sep = ""))
axis(1, at = at, labels = L,las=1)
axis(3, at = at, labels = NA)
at <- c(-30,-35,-40)
L <- parse(text = paste(-at, "*degree ~ S", sep = ""))
axis(2, at = at, labels = L,las=1)
axis(4, at = at, labels = NA)
legend("bottomright",legend = c(expression(paste(r^2,"<0.3",sep="")),
                               expression(paste("0.3<",r^2,"<0.5",sep="")),
                               expression(paste("0.5<",r^2,sep=""))),
       pch = 20,col=c(rbPal(10)[c(2,5,8)]),bty = "n")
dev.off()

png(filename ="./figs/SEA_Cor_lag70_48.png",width=250,height=250)
res <- lm.Q.SST12(lag1 = 48, lag2 = 70)
cols <- rbPal(10)[as.numeric(cut(res["ar2",],breaks = c(seq(0,0.5,length.out=9),1)))]
par(oma=c(3,3.5,0.5,0.5),mar=c(0,0,0,0))
plot(NA,type="n",xlim=c(145,152),ylim=c(-40,-30),xlab="",ylab="",asp=15/16,
     xaxt="n",yaxt="n")
plot.land2()
points(attr(res,"lon"),attr(res,"lat"),col=cols,pch=20)
at <- c(145,150)
L <- parse(text = paste(at, "*degree ~ E", sep = ""))
axis(1, at = at, labels = L,las=1)
axis(3, at = at, labels = NA)
at <- c(-30,-35,-40)
L <- parse(text = paste(-at, "*degree ~ S", sep = ""))
axis(2, at = at, labels = L,las=1)
axis(4, at = at, labels = NA)
legend("bottomright",legend = c(expression(paste(r^2,"<0.3",sep="")),
                               expression(paste("0.3<",r^2,"<0.5",sep="")),
                               expression(paste("0.5<",r^2,sep=""))),
       pch = 20,col=c(rbPal(10)[c(2,5,8)]),bty = "n")
dev.off()


png(filename ="./figs/SEA_Cor_lag200_200.png",width=250,height=250)
res <- lm.Q.SST12(lag1 = 200, lag2 = 200)
cols <- rbPal(10)[as.numeric(cut(res["ar2",],breaks = c(seq(0,0.5,length.out=9),1)))]
par(oma=c(3,3.5,0.5,0.5),mar=c(0,0,0,0))
plot(NA,type="n",xlim=c(145,152),ylim=c(-40,-30),xlab="",ylab="",asp=15/16,
     xaxt="n",yaxt="n")
plot.land2()
points(attr(res,"lon"),attr(res,"lat"),col=cols,pch=20,cex=1.5)
at <- c(145,150)
L <- parse(text = paste(at, "*degree ~ E", sep = ""))
axis(1, at = at, labels = L,las=1)
axis(3, at = at, labels = NA)
at <- c(-30,-35,-40)
L <- parse(text = paste(-at, "*degree ~ S", sep = ""))
axis(2, at = at, labels = L,las=1)
axis(4, at = at, labels = NA)
legend("bottomright",legend = c(expression(paste(r^2,"<0.3",sep="")),
                               expression(paste("0.3<",r^2,"<0.5",sep="")),
                               expression(paste("0.5<",r^2,sep=""))),
       pch = 20,col=c(rbPal(10)[c(2,5,8)]),bty = "n")
dev.off()


png(filename ="./figs/SEA_Cor_lag36_156.png",width=250,height=250)
res <- lm.Q.SST12(lag1 = 156, lag2 = 36)
cols <- rbPal(10)[as.numeric(cut(res["ar2",],breaks = c(seq(0,0.5,length.out=9),1)))]
par(oma=c(3,3.5,0.5,0.5),mar=c(0,0,0,0))
plot(NA,type="n",xlim=c(145,152),ylim=c(-40,-30),xlab="",ylab="",asp=15/16,
     xaxt="n",yaxt="n")
plot.land2()
points(attr(res,"lon"),attr(res,"lat"),col=cols,pch=20,cex=1.5)
at <- c(145,150)
L <- parse(text = paste(at, "*degree ~ E", sep = ""))
axis(1, at = at, labels = L,las=1)
axis(3, at = at, labels = NA)
at <- c(-30,-35,-40)
L <- parse(text = paste(-at, "*degree ~ S", sep = ""))
axis(2, at = at, labels = L,las=1)
axis(4, at = at, labels = NA)
legend("bottomright",legend = c(expression(paste(r^2,"<0.3",sep="")),
                               expression(paste("0.3<",r^2,"<0.5",sep="")),
                               expression(paste("0.5<",r^2,sep=""))),
       pch = 20,col=c(rbPal(10)[c(2,5,8)]),bty = "n")
dev.off()

}

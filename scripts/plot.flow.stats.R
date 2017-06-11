plot.flow.stats <- function(site.id = 
          c("TotalSnowyScheme",  "Lake_Hume_DamWall", "Hume_Dam_Natural",  "Hume_Unregulated",  
            "Dartmouth_DamWall", "SnowyMurray",       "UpperTumut"    ,    "BL_Natural")){
  
  require(RColorBrewer)
  pal <- colorRampPalette(c(brewer.pal(4,"RdBu")))(8)
  
  stats <- readRDS("./data/processed/flow.stats.rds")
  probs <- attr(stats,"probs")
  times <- attr(stats,"t.plus")
  pos <- which(attr(stats,"ids") == site.id )
  if(sum(is.na(stats[[5]])) == length(stats[[5]])) {skip.5 <- TRUE} else {skip.5 <- FALSE}
  times.sort <- sort(times,index.return=TRUE)$ix
  times <- sort(times)
  labels <- attr(stats,"what")
  labels <- labels[times.sort]
  
  ss.p <- c()
  for (i in 1:length(stats)){
    ss.p <- c(ss.p,stats[[i]][,pos])
  }
  ss.p <- matrix(ss.p,ncol=length(stats),nrow=length(probs))
  ss.p <- t(ss.p)
  
  ss.p <- ss.p[times.sort,]
  if(skip.5) ss.p <- ss.p[-which(times.sort == 5),]
  if (skip.5) times <- times[-which(times.sort == 5)]
  if (skip.5) labels <- labels[-which(times.sort == 5)]
  
  ylab <- "Cumulative Flow (GL)"
  xlab <- ""
  ylim <- c(0,max(c(ss.p,na.rm=TRUE)))
  xlim <- c(0.5,length(times)+0.5)
  par(oma=c(5,5,2,2),mar=c(0,0,0,0))
  plot(NA,NA,xlim=xlim,ylim=ylim,xlab="",ylab="",
       yaxs="i",xaxs="i",axes=FALSE,
       main = "",type="n")
  axis(side=1,at=1:xlim[2],labels= labels,las=2)
  axis(side=2,las=2)
  box()
  mtext(side=2,text=ylab,line=4)
  mtext(side=3,text="Historical flows",line=4)
  
  poly.x <- c(1:xlim[2],rev(1:xlim[2]))
  polygon(poly.x,c(ss.p[1,],rev(ss.p[2,])),col=pal[1],border=NA)
  polygon(poly.x,c(ss.p[2,],rev(ss.p[3,])),col=pal[2],border=NA)
  polygon(poly.x,c(ss.p[3,],rev(ss.p[4,])),col=pal[3],border=NA)
  polygon(poly.x,c(ss.p[4,],rev(ss.p[5,])),col=pal[4],border=NA)
  polygon(poly.x,c(ss.p[5,],rev(ss.p[6,])),col=pal[5],border=NA)
  polygon(poly.x,c(ss.p[6,],rev(ss.p[7,])),col=pal[6],border=NA)
  polygon(poly.x,c(ss.p[7,],rev(ss.p[8,])),col=pal[7],border=NA)
  polygon(poly.x,c(ss.p[8,],rev(ss.p[9,])),col=pal[8],border=NA)
  lines(1:xlim[2],ss.p[5,],col="black",lwd=3)
  lines(1:xlim[2],ss.p[2,],col="black",lwd=3,lty=2)
  lines(1:xlim[2],ss.p[8,],col="black",lwd=3,lty=2)
  
  #Scale bar
  ys <- seq(mean(ylim)+ 0.1*diff(range(ylim)),mean(ylim)+ 0.45*diff(range(ylim)),length.out=9)
  xs <- c(1,1.5)
  x.p <- c(xs[1],xs[2],xs[2],xs[1],xs[1])
  for (p1 in 1:8){polygon(x.p,c(ys[p1],ys[p1],ys[p1+1],ys[p1+1],ys[p1]),col=pal[p1])}
  text(1.5,ys[8],"95%",pos=4)
  text(1.5,ys[6],"75%",pos=4)
  text(1.5,ys[4],"25%",pos=4)
  text(1.5,ys[2]," 5%",pos=4)
  
  #return(labels)
}

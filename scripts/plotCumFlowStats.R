#plots cumulative flow historical and forecast
#requires s.1 - s.5
plotCumFlowStats <- function(site.id,forecast=NULL){

  require(RColorBrewer)
  flow.stats <- readRDS("./data/processed/flow.stats.rds")
  
  probs <- attr(flow.stats,"probs")
  q.times <- attr(flow.stats,"t.plus")
  t.sort <- sort(q.times,index.return=TRUE)$ix
  pos.id <- which(attr(flow.stats,"ids") == site.id)
  
	ylab <- "Cumulative Flow (GL)"
	xlab <- "Flow Year"
  ylim <- range(unlist(lapply(flow.stats,FUN = function(x) x[,pos.id])),na.rm=TRUE)
  xlim <- range(q.times,na.rm=TRUE)
  labels <- attr(flow.stats,"what")
	pal <- colorRampPalette(c(brewer.pal(4,"RdBu")))(8) 
	par(mar=c(0,0,0,0),oma=c(4,4,3,2))
	plot(NA,NA,xlim=xlim,ylim=ylim,xlab="",ylab="",
	     yaxs="i",xaxs="i",axes=FALSE
	     #,	   main = "Forecast Inflows"
	)
	axis(side=1,at=q.times[t.sort],labels=labels[t.sort])
	axis(side=2)	
	axis(side=4,labels = FALSE)
	mtext(side=1,line=3,xlab)
	mtext(side=2,line=2.5,ylab)
	mtext(side=3,line=1,site.id)
	box()
	for (i in 1:(length(probs)-1)){
	  qp1 <- unlist(lapply(flow.stats, FUN = function(x) x[i,pos.id]))[t.sort]
	  qp2 <- unlist(lapply(flow.stats, FUN = function(x) x[i+1,pos.id]))[t.sort]   
	 	polygon(c(q.times[t.sort],rev(q.times[t.sort]),q.times[t.sort][1]),
	 	        c(qp1,rev(qp2),qp1[1]),col=pal[i],border=NA)
  }
	lines(q.times[t.sort],unlist(lapply(flow.stats, FUN = function(x) x[which(probs == 0.5),pos.id]))[t.sort],col="black",lwd=2)
	#	lines(0:5,ss.p[2,],col="black",lwd=3,lty=2)
	#	lines(0:5,ss.p[8,],col="black",lwd=3,lty=2)
	
	#plot forecasts
	#points(1,Q1.p,cex=1.5,pch=20); 
	#	error.bar(1, Q1.p,  1.96*Q1.se)	
	#points(2,Q2.p,cex=1.5,pch=20); 
  #		error.bar(2, Q2.p,  1.96*Q2.se)
	#points(3,Q3.p,cex=1.5,pch=20); 
	#	error.bar(3, Q3.p,  1.96*Q3.se)
	#points(4,Q4.p,cex=1.5,pch=20); 
	#	error.bar(4, Q4.p,  1.96*Q4.se)
	#points(5,Q5.p,cex=1.5,pch=20); 
	#	error.bar(5, Q5.p,  1.96*Q5.se)			
	#lines(0:5,c(0,Q1.p,Q2.p,Q3.p,Q4.p,Q5.p),lwd=2,lty=3)
	#Scale bar
	ys <- seq(mean(ylim)+ 0.1*diff(range(ylim)),
	          mean(ylim)+ 0.45*diff(range(ylim)),length.out=9)
	xs <- min(q.times) + c(0.05,0.1)*diff(range(q.times))
	x.p <- c(xs[1],xs[2],xs[2],xs[1],xs[1])
	for (p1 in 1:8){
	  polygon(x.p,c(ys[p1],ys[p1],ys[p1+1],ys[p1+1],ys[p1]),col=pal[p1])
	}
	xt <- xs[2]+ 0.05*diff(range(q.times))
	text(xt,ys[8],"95%")
	text(xt,ys[6],"75%")
	text(xt,ys[4],"25%")
	text(xt,ys[2]," 5%")
}

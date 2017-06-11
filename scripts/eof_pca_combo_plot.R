eof_pca_combo_plot <- function(pca.obj,type=c("grid","points"),num=1,
                               what1 = "eof",
                               what2 = "pc",
                               nlevels = 15,breaks,zero.center=TRUE,
                               legend.pos=NULL,
                               plotscale=FALSE,nwin=NULL,flip.sign=1,...){
  
 # what1 can take ,"loadings","spatial.phase","spatial.amplitude"
 # what2 can take ,"scores","temp.phase","temp.amplitude"  
  
  if (missing(breaks)) {
    if (zero.center) {
      maxx <- max(abs(pca.obj[[what1]][,num]))
      minx <- -maxx
      breaks <- c(pretty(c(minx, +maxx), nlevels))
    }
    else {
      maxx <- max(abs(pca.obj[[what1]][,num]))
      minx <- min(abs(pca.obj[[what1]][,num]))
      breaks <- c(pretty(range(abs(pca.obj[[what1]][,num]), finite = TRUE), 
                         nlevels))
    }
  }
  
  #iF it is an eeof object then allow for multiple images to be plotted at various lags as specified in nwin
  if (!is.null(nwin)){
    #nwin specifies the lagged pattern(s) of the num'th eof to plot
    nw <- length(nwin)
  } else {
    nw <- 1
    nwin <- 1
  }  
  
  fig.eof <- matrix(NA,nrow=nw,ncol=4)
  for (j in 1:nw){
    fig.eof[j,] <- c(0.3,0.7,1-1.2*j/(nw+1),1-(j-1)/(nw+1))
  }
  fig.pc <- c(0,0.9,0,0.6*1/(nw+1))
  fig.scale <- c(0.9,0.95,0.5,0.9) 
  
  #dev.new(width=11,height=8, noRStudioGD = TRUE)
  par(mar=c(1,0,0,0),oma=c(4,4,1,4) ,fig =c(0,1,0,1))
  plot.new()
  
  #Color scale
  par(fig = fig.scale,new = TRUE)
  plot.new()
  image_scale(flip.sign*pca.obj[[what1]][,num], 
              zlim = c(minx,maxx), 
              horiz=FALSE,las=2,xlab="",ylab="",
              nlevels=nlevels)
  
  #PC Time Series
  par(fig = fig.pc, new = TRUE)
  plot.new()
  if (length(what2) == 1){
    ylab <- switch(what2, 
                   "pc" = "PC",
                   "temp.phase"="Temporal Phase",
                   "temp.amplitude" = "Temporal Amplitude")
    plot(attributes(pca.obj)$date, flip.sign*pca.obj[[what2]][,num], 
         xlab="", ylab="",type="l")
    mtext(side=2,line=2.5,ylab)
    mtext(side=1,line=2.5,"Year")
  }
  if (length(what2) == 2){
    ylab <- switch(what2[1], 
                   "pc" = "PC",
                   "temp.phase"="Temporal Phase",
                   "temp.amplitude" = "Temporal Amplitude")
    plot(attributes(pca.obj)$date,flip.sign*pca.obj[[what2[1]]][,num],
         xlab="",ylab="",type="l")
    mtext(side=2,ylab,line=2.5)
    mtext(side=1,line=2.5,"Year")
    
    par(fig=fig.pc,new=TRUE)
    plot.new()
    ylab <- switch(what2[1], 
                   "pc" = "PC",
                   "temp.phase"="Temporal Phase",
                   "temp.amplitude" = "Temporal Amplitude")
    plot(attributes(pca.obj)$date,flip.sign*pca.obj[[what2[2]]][,num],
         axes=FALSE,xlab="",ylab="",type="l",lty=2)
    mtext(side=4,ylab,line=2.5)
    axis(side=4)
  }
  
  #EOF Pattern(s)
  for (j in 1:nw){
    par(fig = fig.eof[j,],new = TRUE)
    plot.new()
    plot.pca.vec2(pca.obj,what=what1,num=num,projectn = "mercator",
                  type=type,plotscale=plotscale,
                  legend.pos=legend.pos,nlevels=nlevels,win.id=nwin[j],flip.sign=flip.sign,...)
  }
  
}  


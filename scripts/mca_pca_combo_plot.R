mca_pca_combo_plot <- function(mca.obj,type=c("grid","points"),num=1,
                               what = "singular.vectors",
                               nlevels = 15,legend.pos=NULL,
                               xlim.l=NULL,ylim.l=NULL,
                               xlim.r=NULL,ylim.r=NULL,
                               zero.center=TRUE,nwin=NULL,savefig=FALSE,....){
  
  # what1 can take ,"loadings","spatial.phase","spatial.amplitude"
  # what2 can take ,"scors","temp.phase","temp.amplitude"                               
  left.what <- paste("left.",what,sep="")
  right.what <- paste("right.",what,sep="")
  
  
    if (zero.center) {
      maxx.l <- max(abs(mca.obj[[left.what]][,num]))
      minx.l <- -maxx.l
      breaks.l <- c(pretty(c(minx.l, +maxx.l), nlevels))
      
      maxx.r <- max(abs(mca.obj[[right.what]][,num]))
      minx.r <- -maxx.r
      breaks.r <- c(pretty(c(minx.r, +maxx.r), nlevels))
    }
  else {
    maxx.l <- max(mca.obj[[left.what]][,num])
    minx.l <- min(mca.obj[[left.what]][,num])
    breaks.l <- c(pretty(c(minx.l,maxx.l), nlevels))
    
    maxx.r <- max(mca.obj[[right.what]][,num])
    minx.r <- min(mca.obj[[right.what]][,num])
    breaks.r <- c(pretty(c(minx.r,maxx.r), nlevels))
  }
  
  
  #If EEOF can plot one of the windowed elements via nwin
  #if (is.null(nwin.r)) nwin.r <- 1
  #if (is.null(nwin.l)) nwin.l <- 1
  
  if (!is.null(nwin)){
    #nwin specifies the lagged pattern(s) of the num'th eof to plot
    nw <- length(nwin)
  } else {
    nw <- 1
    nwin <- 1
  }  
  fig.mca.right <- fig.mca.left <- matrix(NA,nrow=nw,ncol=4)
  for (j in 1:nw){
    fig.mca.left[j,] <- c(0,0.4,1-1.2*j/(nw+1),1-(j-1)/(nw+1))
    fig.mca.right[j,] <- c(0.5,0.9,1-1.2*j/(nw+1),1-(j-1)/(nw+1))
  }
  fig.pc <- c(0,0.9,0,0.6*1/(nw+1))
  fig.scale.left <- c(0.4,0.45,0.5,0.9)
  fig.scale.right <- c(0.9,0.95,0.5,0.9) 
  
  
  
  
  #if (!savefig) dev.new(width=11,height=8, noRStudioGD = TRUE)
  par(mar=c(1,0,0,0),oma=c(4,4,1,4) ,fig =c(0,1,0,1))
  plot.new()
  
  #Right mca
  for (j in 1:nw){
    par(new = TRUE, fig = fig.mca.right[j,])
    plot.new()
    plot.mca.vec(mca.obj,what=right.what,num=num,
                 type=type[2],plotscale= FALSE,breaks=breaks.r,
                 xlim=xlim.r,ylim=ylim.r,zero.center=zero.center,win.id=nwin[j])
  }
  
  
  #Right colour scale
  par(new = TRUE, fig = fig.scale.right)
  plot.new()
  image_scale(mca.obj[[right.what]][,num], 
              zlim = c(minx.r,maxx.r),
              horiz=FALSE,las=1,xlab="",ylab="",
              nlevels=nlevels)
  
 
  #Left mca
  for (j in 1:nw){
    par(new = TRUE, fig = fig.mca.left[j,])
    plot.new()
    plot.mca.vec(mca.obj,what=left.what,num=num,
               type=type[1],plotscale=FALSE,breaks=breaks.l,
               xlim=xlim.l,ylim=ylim.l,zero.center=zero.center,win.id=nwin[j])
  }
  
 
  #Left colour scale
  par(new = TRUE, fig = fig.scale.left)
  plot.new()
  image_scale(mca.obj[[left.what]][,num], 
              zlim = c(minx.l,maxx.l),
              horiz=FALSE,las=1,xlab="",ylab="",
              nlevels=nlevels)
  
  
  
  #Left and Right coefficients
  par(new = TRUE, fig = fig.pc)
  plot.new()
      ylab <- "Left Coefficients"
      plot(attributes(mca.obj$center.left)$date,mca.obj[["coeff.left"]][,num],
       axes=TRUE,xlab="",ylab="",type="l",lty=1)
      mtext(side=2,ylab,line=2.5)
      mtext(side=1,line=2.5,"Year")
  
      par(new = TRUE, fig = fig.pc)
      plot.new()
      ylab <- "Right Coefficients"
      plot(attributes(mca.obj$center.right)$date,mca.obj[["coeff.right"]][,num],
       axes=FALSE,xlab="",ylab="",type="l",lty=2)
      mtext(side=4,ylab,line=2.5)
      axis(side=4)

}


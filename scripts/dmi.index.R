dmi.index <- function(dmi, xlim = NULL, index.name = "Dipole Mode Index") {
   #Plot Dipole Mode Index
  
    yrmon <- dmi$dates
    yrs <- as.numeric(as.character(format(yrmon,"%Y")))
    doy <- as.numeric(as.character(format(yrmon,"%j")))
    yrmon <- yrs + doy/365
    dmi <- dmi$index
    
    index.positive <-  dmi > 0
    index.negative <-  dmi <= 0
    in.red <- in.blue <-  dmi
    in.red[which(index.negative)] <- 0
    in.blue[which(index.positive)] <- 0
    par(mar=c(4,4,0.5,0.5),oma=c(0,0,0,0))
    plot(yrmon,dmi,type="n",axes=FALSE,xlab="",ylab="",
         ylim=c(-max(abs(dmi),na.rm=TRUE),max(abs(dmi),na.rm=TRUE)),
         xlim=xlim)
    polygon(c(yrmon,yrmon[length(yrmon)], yrmon[1]),c(in.red,0,0),col="red",border=NA)
    polygon(c(yrmon,yrmon[length(yrmon)], yrmon[1]),c(in.blue,0,0),col="blue",border=NA)
    lines(yrmon,dmi,lwd=1.2)
    abline(0,0)
    box()
    axis(side=1)
    axis(side=2)
    mtext(side = 2, index.name,line=2.5)
  }

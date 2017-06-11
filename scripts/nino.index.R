#TECHNICAL NOTES
#Nino X Index computation: (a) Compute area averaged total SST from Niño X region; (b) Compute monthly climatology (e.g., 1950-1979) for area averaged total SST from Niño X region, and subtract climatology from area averaged total SST time series to obtain anomalies; (c) Smooth the anomalies with a 5-month running mean; (d) Normalize the smoothed values by its standard deviation over the climatological period.

#Niño 1+2 (0-10S, 90W-80W):  The Niño 1+2 region is the smallest and eastern-most of the Niño SST regions, and corresponds with the region of coastal South America where El Niño was first recognized by the local populations.  This index tends to have the largest variance of the Niño SST indices.

#Niño 3 (5N-5S, 150W-90W):  This region was once the primary focus for monitoring and predicting El Niño, but researchers later learned that the key region for coupled ocean-atmosphere interactions for ENSO lies further west (Trenberth, 1997).  Hence, the Niño 3.4 and ONI became favored for defining El Niño and La Niña events.

#Niño 3.4 (5N-5S, 170W-120W):  The  Niño 3.4 anomalies may be thought of as representing the average equatorial SSTs across the Pacific from about the dateline to the South American coast.  The Niño 3.4 index typically uses a 5-month running mean, and El Niño or La  Niña events are defined when the  Niño 3.4 SSTs exceed +/- 0.4C for a period of six months or more.

#ONI (5N-5S, 170W-120W): The ONI uses the same region as the Niño 3.4 index.  The ONI uses a 3-month running mean, and to be classified as a full-fledged El Niño or La Niña, the anomalies must exceed +0.5C or -0.5C for at least five consecutive months.  This is the operational definition used by NOAA.

#Niño 4 (5N-5S, 160E-150W): The  Niño 4 index captures SST anomalies in the central equatorial Pacific.  This region tends to have less variance than the other Niño regions

#TNI computation: (a) Compute area averaged total SST from Niño 1+2 region; (b) Compute area averaged total SST from Niño 4 region; (c) Compute monthly climatologies (e.g., 1950-1979) for area averaged total SST from Niño 1+2 region, and Niño 4 region, and subtract climatologies from area averaged total SST time series to obtain anomalies; (d) Normalize each time series of anomalies by their respective standard deviations over the climatological period; (e) Define the raw TNI as Niño 1+2 normalized anomalies minus Niño 4 normalized anomalies; (f) Smooth the raw TNI with a 5-month running mean; (g) Normalize the smoothed TNI by its standard deviation over the climatological period.

#Trenberth, Kevin & National Center for Atmospheric Research Staff (Eds). Last modified 02 Feb 2016. "The Climate Data Guide: Nino SST Indices (Nino 1+2, 3, 3.4, 4; ONI and TNI)." Retrieved from https://climatedataguide.ucar.edu/climate-data/nino-sst-indices-nino-12-3-34-4-oni-and-tni.

nino.index <- function(ssts, what="nino3.4",clim.period = c(1971,2000),plt=TRUE,xlim = NULL) {
  #clim.period defined corresponding with ftp://ftp.cpc.ncep.noaa.gov/wd52dg/data/indices/Readme.index.htm
  #Note they use an Optimum Interpolation (OI) scheme, which replaced the blended analysis - presumably this means a moving average
  #This still uses a moving average
  
  
  lat <- attr(ssts,"lat")
  lon <- attr(ssts,"lon")
 
  sst.dates <- attr(ssts,"date")
  pos.clim <- which(sst.dates >= as.Date(paste(clim.period[1],"-01-01",sep="")) & sst.dates <= as.Date(paste(clim.period[2],"-12-01",sep="")))
  yr <- as.numeric(as.character(format(sst.dates,format="%Y")))
  mon <- as.numeric(as.character(format(sst.dates,format="%m")))
  yrmon <- yr+mon/12
  
  if (what != "tni") {
    region <- switch(what,
                     nino3 = rbind(c(-5,210),c(5,270)), 
                     nino12 = rbind(c(-10,270),c(0,280)),
                     nino3.4 = rbind(c(-5,190),c(5,240)),
                     oni = rbind(c(-5,190),c(5,240)),
                     nino4 = rbind(c(-5,160),c(5,210))
    )
    
    averaging.window <- switch(what,
                               nino3 = rep(1,5)/5, 
                               nino12 = rep(1,5)/5,
                               nino3.4 = rep(1,5)/5,
                               oni = rep(1,3)/3,
                               nino4 = rep(1,5)/5
    )
    
    class.window <- switch(what,
                           nino3 = NULL, 
                           nino12 = NULL,
                           nino3.4 = 6,
                           oni = 5,
                           nino4 = NULL
    )
    
    class.threshold <- switch(what,
                              nino3 = 0, 
                              nino12 = 0,
                              nino3.4 = 0.4,
                              oni = 0.5,
                              nino4 =0
    )
    
    pos.lat <- which(lat >= region[1,1] & lat <= region[2,1])
    pos.lon <-  which(lon >= region[1,2] & lon <= region[2,2]) 
    
    index.data <- ssts[pos.lon,pos.lat,]
    mean.region.data <- apply(index.data,MARGIN=3,FUN=function(x) mean(c(x),na.rm=TRUE))
    
    climatology <- vector("numeric",length=12)
    for (i in 1:12)   {
      pos <- seq(from=pos.clim[i],to = pos.clim[length(pos.clim)],by=12)
      climatology[i] <- mean(mean.region.data[pos])
      pos <- seq(from=i,to = dim(ssts)[3],by=12)
      mean.region.data[pos] <- mean.region.data[pos] - climatology[i]
    }
    
    index <- as.numeric(stats::filter(mean.region.data,sides=1,filter=averaging.window))
    index.raw <- mean.region.data/sd(index[pos.clim],na.rm=TRUE)
    index[1:(length(averaging.window)-1)] <- 0
    index.sd <- sd(index[pos.clim],na.rm=TRUE)
    index.norm <- index/index.sd
    
    nino.class <- vector("character",length=length(index))
    if (what %in% c("nino3.4","oni")) {
      index.class.pos <- index > class.threshold
      index.class.neg <-index < -class.threshold
      nino.class[1:(class.window-1)] <- "Neutral"
      for (i in class.window:length(index)){
        #print(i)
        if(sum(index.class.pos[(i-class.window+1):i])==class.window)  nino.class[i] <- "ElNino"
        if(sum(index.class.neg[(i-class.window+1):i])==class.window)  nino.class[i] <- "LaNina"
        if(sum(index.class.pos[(i-class.window+1):i])<class.window & 
           sum(index.class.neg[(i-class.window+1):i])<class.window)  nino.class[i] <- "Neutral"
      }
    }
    
    attr(index.norm,"temp") <- index
    attr(index.norm,"raw") <- index.raw
    attr(index.norm,"classification") <-  nino.class
    attr(index.norm,"climatology") <- clim.period
    attr(index.norm,"type") <- what
    attr(index.norm,"date") <- sst.dates
    
  } else {
    #TNI computation: (a) Compute area averaged total SST from Niño 1+2 region; (b) Compute area averaged total SST from Niño 4 region; (c) Compute monthly climatologies (e.g., 1950-1979) for area averaged total SST from Niño 1+2 region, and Niño 4 region, and subtract climatologies from area averaged total SST time series to obtain anomalies; (d) Normalize each time series of anomalies by their respective standard deviations over the climatological period; (e) Define the raw TNI as Niño 1+2 normalized anomalies minus Niño 4 normalized anomalies; (f) Smooth the raw TNI with a 5-month running mean; (g) Normalize the smoothed TNI by its standard deviation over the climatological period.
    
    nino12 <- nino.index(ssts, what="nino12",clim.period = c(1971,2000),plt=FALSE)
    nino4 <- nino.index(ssts, what="nino4",clim.period = c(1971,2000),plt=FALSE)
    
    tn.raw <- attr(nino12,"raw") - attr(nino4,"raw")
    tn.smooth <- stats::filter(tn.raw,sides=1,filter=rep(1,5)/5)
    tn.sd <- sd(tn.smooth[pos.clim])
    index.norm <- tn.smooth/tn.sd
    
    attr(index.norm,"temp") <- tn.raw
    attr(index.norm,"raw") <- tn.raw
    attr(index.norm,"classification") <-  rep("",length(index.norm))
    attr(index.norm,"climatology") <- clim.period
    attr(index.norm,"type") <- what
    attr(index.norm,"date") <- sst.dates
    
    class.threshold <- 0
    index.sd <- 1
    
  }
  
  
  if (plt) {
    index.positive <-  index.norm > class.threshold/index.sd
    index.neutral  <-  index.norm <=  class.threshold/index.sd & index.norm >=  -class.threshold/index.sd
    index.negative <-  index.norm< -class.threshold/index.sd
    in.red <- in.blue <- in.neut <- index.norm;
    in.red[index.neutral | index.negative] <- 0
    in.neut[index.positive | index.negative] <- 0
    in.blue[index.neutral | index.positive] <- 0
    par(mar=c(4,4,0.5,0.5),oma=c(0,0,0,0))
    plot(yrmon,index.norm,type="n",axes=FALSE,xlab="",ylab="",
         ylim=c(-max(abs(index.norm),na.rm=TRUE),max(abs(index.norm),na.rm=TRUE)),
         xlim=xlim)
    polygon(c(yrmon,yrmon[length(yrmon)], yrmon[1]),c(in.red,0,0),col="red",border=NA)
    polygon(c(yrmon,yrmon[length(yrmon)], yrmon[1]),c(in.blue,0,0),col="blue",border=NA)
    polygon(c(yrmon[1],yrmon[length(yrmon)], yrmon[length(yrmon)], yrmon[1],yrmon[1]),
            c(class.threshold/index.sd,class.threshold/index.sd,-class.threshold/index.sd,-class.threshold/index.sd,0),col="white",
            border=NA)
    lines(yrmon,index.norm,lwd=4)
    abline(0,0)
    box()
    axis(side=1)
    axis(side=2)
    mtext(side = 2, paste(what, "(standard deviations)"),line=2.5)
  }
  
  return(index.norm)
  
}

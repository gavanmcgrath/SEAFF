plot.pca.eeof <-
  function(x.pca,what="loadings",num=1,amplitude=+1,
           zero.center="AUTO",type="grid",plot.windows=NULL,...) {
    
   
    # extract relevant vector from pca object according to "what"
    vvv <- switch(what,
                  "center" = x.pca$center,
                  "scale" = x.pca$scale,
                  "loadings" = x.pca$loadings[,num],
                  "homcor" = x.pca$homcor[,num],
                  "center+loading" = x.pca$center+
                    (amplitude*x.pca$sdev[num]*
                       x.pca$scale*
                       x.pca$loadings[,num]),
                  "rot.loadings" = x.pca$rot.loadings[,num],
                  stop("ERROR: invalid value for what!")
    )
    
    # Automatic treatment of zero.center 
    if (zero.center == "AUTO") {
      zero.center <- switch(what,
                            "center" = FALSE,
                            "scale" = FALSE,
                            "loadings" = TRUE,
                            "homcor" = TRUE,
                            "rot.loadings" = TRUE,
                            "center+loading" = FALSE,
                            stop("ERROR: invalid value for what!"))
    }
    
    # transfer attributes from pca object (e.g. grid structure)
    # note that additional attributes are stored with the center item
   
    
    # call the plot function
    window <- attributes(x.pca)$window
    if (is.null(window)) {stop("No window attribute. Stopping.")}
    ns <- length(attributes(x.pca)$ixs)
    if (is.null(plot.windows)) plot.windows <- 1
    
    par(mfrow=c(length(plot.windows),1),mar=c(0.01,0.1,0.01,0.1),oma=c(0,0,0,0))
    
   
    for (i in plot.windows){
      aaa <- vvv[((i-1)*ns+1):(i*ns)]
      aaa <- transfer.attributes(x.pca,aaa)
	  attr(aaa,"crs") <- "+proj=longlat +datum=WGS84"
      
      fld <- vector2field(aaa)
      
      if (type == "grid"){
        fld <- vector2field(aaa)
        plot.projected(fld,...)
      }
      if (type == "points"){
        plot.pca.points(aaa,...)
      } 
    }
  }


plot.pca.vec2 <-
function(x.pca,what="eof",num=1,amplitude=+1,
         zero.center="AUTO",type="grid",win.id=NULL,flip.sign=1,...) {

     # extract relevant vector from pca object according to "what"
     vvv <- switch(what,
               "data" = x.pca[,num],     
               "center" = x.pca$center,
               "pc" = x.pca$pc,
               "eof" = x.pca$eof[,num],
               "homcor" = x.pca$homcor[,num],
               "center+loading" = x.pca$center+
                             (amplitude*x.pca$sdev[num]*
                             x.pca$scale*
                             x.pca$eof[,num]),
               "rot.loadings" = x.pca$rot.loadings[,num],
               "spatial.phase" = x.pca$spatial.phase[,num],
               "spatial.amplitude" = x.pca$spatial.amplitude[,num],
               "temporal.phase" = x.pca$temporal.phase[,num],
               "temporal.amplitude" = x.pca$temporal.amplitude[,num],
               "ceof.real" = Re(x.pca$eof[,num]),
               "ceof.im" = Im(x.pca$eof[,num]),
               stop("ERROR: invalid value for what!")
     )
     
     # Automatic treatment of zero.center 
     if (zero.center == "AUTO") {
       zero.center <- switch(what,
                             "data" = FALSE,                 
                             "center" = FALSE,
                             "scale" = FALSE,
                             "eof" = TRUE,
                             "homcor" = TRUE,
                             "rot.loadings" = TRUE,
                             "center+loading" = FALSE,
                             "spatial.phase" = FALSE,
                             "spatial.amplitude" = FALSE,
                             "temporal.phase" = FALSE,
                             "temporal.amplitude" = FALSE,
                             "ceof.real" = FALSE,
                             "ceof.im" = FALSE,
                             stop("ERROR: invalid value for what!"))
     }
     ns <- length(attributes(x.pca)$ixs)
     if (!is.null(win.id)) {
       vvv <- vvv[(1+(win.id-1)*ns):(win.id*ns)]
     }
     
     vvv <- flip.sign*vvv
     # transfer attributes from pca object (e.g. grid structure)
     # note that additional attributes are stored with the center item
     
	   vvv <- transfer.attributes(x.pca,vvv)
	   attr(vvv,"crs") <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
	 
     # call the appropriate plot function
     if (type == "grid"){
       fld <- vector2field(vvv)
       plot.projected(fld,...)
     }
     if (type == "points"){
       plot.pca.points(vvv, ...)
     }
     
}



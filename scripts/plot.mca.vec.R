plot.mca.vec <-
function(x.mca,what="left.singular.vectors",num=1,amplitude=+1,
         zero.center="AUTO",type="grid",breaks,win.id=NULL,...) {

     # extract relevant vector from mca object according to "what"
     vvv <- switch(what,
               "center.left" = x.mca$center.left,
               "center.right" = x.mca$center.right,
               "scale.left" = x.mca$scale.left,
               "scale.right" = x.mca$scale.right,
               "left.singular.vectors" = x.mca$left.singular.vectors[,num],
               "right.singular.vectors" = x.mca$right.singular.vectors[,num],
               "left.homcor" = x.mca$left.homcor[,num],
               "right.homcor" = x.mca$right.homcor[,num],
               "left.hetcor" = x.mca$left.hetcor[,num],
               "right.hetcor" = x.mca$right.hetcor[,num],
               "left.center+singular.vector" = x.mca$center.left+
                             amplitude*sd(x.mca$coeff.left[,num])*
                             x.mca$scale.left*
                             x.mca$left.singular.vectors[,num],
               "right.center+singular.vector" = x.mca$center.right+
                             amplitude*sd(x.mca$coeff.right[,num])*
                             x.mca$scale.right*
                             x.mca$right.singular.vectors[,num],
               stop("ERROR: invalid value for what!")
            )

     # Automatic treatment of zero.center 
     if (zero.center == "AUTO") {
         zero.center <- switch(what,
               "center.left" = FALSE,
               "center.right" = FALSE,
               "scale.left" = FALSE,
               "scale.right" = FALSE,
               "left.singular.vectors" = TRUE,
               "right.singular.vectors" = TRUE,
               "left.homcor" = TRUE,
               "right.homcor" = TRUE,
               "left.hetcor" = TRUE,
               "right.hetcor" = TRUE,
               "left.center+singular.vector" = FALSE,
               "right.center+singular.vector" = FALSE,
               stop("ERROR: invalid value for what!"))
     }

    

     
     # transfer attributes from mca object (e.g. grid structure)
     # note that additional attributes are stored with the center item
     left <- ("left" %in% strsplit(what,split=".",fixed=TRUE)[[1]])
     right <- ("right" %in% strsplit(what,split=".",fixed=TRUE)[[1]])
     if (right+left != 1) {stop("ERROR: can't decide for right or left!")}
     if(right) {
        cent <- x.mca$center.right
     } else {
        cent <- x.mca$center.left
     }
     
     if (!is.null(win.id)) {
       vvv <- vvv[(1+(win.id-1)*length(attributes(cent)$ixs)):(win.id*length(attributes(cent)$ixs))]
     }
     
	 vvv <- transfer.attributes(cent,vvv)
     attr(vvv,"crs") <- "+proj=longlat +datum=WGS84"
    
     # call the appropriate plot function
     if (type == "grid"){
       fld <- vector2field(vvv)
       plot.projected(fld, ...)
     } 
     if (type == "points"){
       plot.pca.points(vvv, ...)
     }
}


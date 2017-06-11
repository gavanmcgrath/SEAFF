vector2field <- function(vec) {

        # get field attributes from vector
        nx <- length(attr(vec,"lon"))
        ny <- length(attr(vec,"lat"))
        ixs <- attr(vec,"ixs")
        iys <- attr(vec,"iys")

        # convert to 2D field
        field <- matrix(NA,ncol=ny,nrow=nx)
        for (k in 1:length(vec)) {
           field[ixs[k],iys[k]] <- vec[k]
        }

        # append field attributes to field
        all.atts <- names(attributes(vec))
        trans.atts <- all.atts[!(all.atts %in% 
                                   c("dim","dimnames","names","row.names","class"))]
        for (a in trans.atts) {
          attr(field,a) <- attr(vec,a)
        }
        
        # return result
        field
}


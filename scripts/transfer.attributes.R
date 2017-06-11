transfer.attributes <- function(X,Y){
  #Generic function to transfer attributes from object X to object Y
  all.atts <- names(attributes(X))
  trans.atts <- all.atts[!(all.atts %in% 
                             c("dim","dimnames", "names","class"))]
  for (a in trans.atts) {
    attr(Y,a) <- attr(X,a)
  }
  Y
}
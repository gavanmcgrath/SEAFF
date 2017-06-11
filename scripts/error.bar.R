error.bar <- function(x, y, upper, lower=upper, length=0.1,horiz= FALSE,...){
  if(horiz == FALSE){
    arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
  } else {
    arrows(lower,y, upper, y, angle=90, code=3, length=length, ...)	
  }	
}

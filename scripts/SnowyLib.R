#Script to load all functions for later reuse
sourceDir <- function(path="./scripts", trace = TRUE, ...) {
  lof <- list.files(path, pattern = "\\.[Rr]$")
  lof <- lof[!(lof  == "SnowyLib.R")]  #prevent infinite loop
  for (nm in lof) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

sourceDir("./scripts",trace=FALSE)
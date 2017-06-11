list.of.packages <- c("curl",
                      "devtools",
                      "ggplot2", "Grid2Polygons",
                      "lubridate",
                      "mapdata", "maps", 
                      "ncdf4",
                      "parallel", 
                      "raster","RColorBrewer","rgdal","rgeos", "R.utils","rworldmap", 
                      "shiny","signal", "sp")

install.required.packages <- function(update.all.packages = FALSE,
                                      list.of.packages = list.of.packages){

  if (update.all.packages) { 
    install.packages(list.of.packages)
  } else {
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)>0) install.packages(new.packages)
  }
}

install.required.packages(list.of.packages = list.of.packages)
lapply(list.of.packages,FUN=require,character.only = TRUE)

#' plot_map  Plots a geophysical field data frame 
#'
#' @param data A dataframe with the data to be imaged in the column $layer, latitude and longitude coordinates of the data in the columns $lat and $lon respectively.  Missing data allowed as specified by NA.
#' @param orientation Projection orientation. A three element vector, c(latitude, longitude, rotation) specifiying the Pole of the projection. See mapproj for more detail.
#' @param projectn One of the projections spcified by mapproject in the package mapproj.
#' @param col Vector of colours specified as strings. Uses default color scheme if NULL.
#' @param breaks Numeric Vector of breaks of length(col) + 1 specifying the color partitions. Uses a pretty specification on the range of values if NULL (default).
#' @param fast If TRUE (default) uses coord_quickmap to plot the projected data, else uses the slower but more accurate coord_map to render the projected data
#' @param xlab X-axis label. Defaults to NULL.
#' @param ylab Y-axis label. Defaults to NULL.
#' @param xticks If TRUE (default) plots default x-axis ticks and text.
#' @param yticks If TRUE (default) plots default y-axis ticks and text.
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' data <- data.frame(layer=runif(90*180), 
#'    lat = rep(seq(-89,89,by=2),180),
#'    lon = rep(seq(-179,179,by=2),each=90))
#' plot_map(data)
#' plot_map(data,orientation=c(-75,0,0),projectn="ortho")
#' \dontrun{plot_map(data,orientation=c(-75,0,0),projectn="ortho",fast=FALSE) }
plot_map <- function(data,orientation = c(90, 0, 180),projectn="ortho", col=NULL,
                     breaks = NULL,zlim=NULL,parameters=NULL){
  require(rworldmap)
  require(ggplot2)
  require(maps)
  require(mapdata)
  worldMap <- getMap()
  #worldMap <- plot.map2("world",center=180)
  world.points <- fortify(worldMap)
  world.points$region <- world.points$id
  world.df <- world.points[,c("long","lat","group", "region")]
 # world.df$long[world.df$long < 0 ] <- 180 - world.df$long[world.df$long < 0 ]

  
  #colors 
  if (is.null(col)) col <- c("#8600FF", "#3F94FE", "#77CAFD", "#99EEFF", "#D9FFD9", "#FFFFFF",
                            "#FFFF4C", "#FFCC00", "#FF7E00", "#FF0000", "#5E0000")
  if (is.null(breaks)) breaks <- pretty(data$layer,n=length(col)+1,min.n=length(col))
  if (!is.null(zlim)) breaks <- seq(min(zlim),max(zlim),length.out=length(col)+1)
  
  cols <- cbind(
    val = levels(cut(range(breaks), breaks = breaks)),
    col = col
  )
  cols <- data.frame(cols, stringsAsFactors = FALSE)
  colnames(cols) <- list("val", "col")
  cols$col <- paste(cols$col,"FF", sep = "")
  
  data$interval <- cut(data$layer, breaks = breaks)
  if (is.null(zlim)) zlim <- range(data$layer)
  
  #circles.long <- data.frame( lat = rep(seq(-90,90,by=1),length(seq(-180,180,by=60))),
  #                            lon = rep(seq(-180,180,by=60),each=length(seq(-90,90,by=1))))
  #circles.lat <- data.frame( lon = rep(seq(-180,180,by=1),length(seq(-90,90,by=30))),
  #                           lat = rep(seq(-90,90,by=30),each=length(seq(-180,180,by=1))))
  
  #image(data,)
  gg <- ggplot() + 
    geom_tile(data = data, aes(x = lon, y = lat, fill = interval), alpha = 1) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual("interval", breaks = cols$val, values = cols$col) +
    geom_path(data = world.df, aes(x = long, y = lat, group = group)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_text(),
          axis.ticks.x=element_line(),
          axis.title.y=element_blank(),
          axis.text.y=element_text(),
          axis.ticks.y=element_line(),
          title = element_blank())
  #geom_path(data = circles.long, 
  #          aes(x = lon, y = lat, group = lon)) +
  #  geom_path(data = circles.lat, 
  #            aes(x = lon, y = lat, group = lat)) +
  if (!is.null(parameters)){
    gg <- gg + coord_map(projectn,orientation=orientation,parameters=parameters)
  } else {
    gg <- gg + coord_map(projectn,orientation=orientation)
  }  
  
  gg
}  

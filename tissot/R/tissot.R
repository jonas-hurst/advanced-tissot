#' This function generates Tissot's indicatrix
#'
#' This function returns equally-sized circles (radius = 300km) as sf-polygons
#' to use as Tissot's indicatrix. The circles are located on a regular grid in
#' lon/lat every 20 degrees.
#' @return circles as objects of class sf
#' @import sf
#' @param geom input geometry for which tissot should be generated
#' @param circles_den Specifies the number of indicatrix circles that are generated.
#' Default is automatic. Manually, number of circles in x and y direction can be
#' specified through an array, e.g. c(20, 10).
#' @export
# Constructor
tissot <- function (geom, circles_den="auto") {
  geom = sf::st_geometry(geom)
  circ = sf::st_geometry(make_indicatrix(geom, circles_den = circles_den))

  value <- list(geometry = geom, circles = circ)
  class(value) = "tissot"
  value
}

#' This function returns the original geometries, from which Tissot indicatrix
#' was generated
#' @return Original geometry as objects of class sf
#' @param obj tissot object
#' @export
get_geometry.tissot <- function(obj){
  return(obj$geometry)
}

#' This function returns the Tissot indicatrix circles that were generated
#' @return Tissot indicatrix as polygon objects of class sf
#' @param obj tissot object
#' @export
get_indicatrix.tissot <- function(obj){
  return(obj$circles)
}

plot.tissot <- function(obj, srid){}
print.tissot <- function(){}
summarize.tissot <- function(){}

make_indicatrix = function(geom, circles_den="auto"){

  geom_bbox <- sf::st_bbox(geom)
  x_min <- geom_bbox$xmin
  y_min <- geom_bbox$ymin
  x_max <- geom_bbox$xmax
  y_max <- geom_bbox$ymax

  x_ext <- abs((x_max - x_min))
  y_ext <- abs((y_max - y_min))

  print(x_ext)
  print(y_ext)

  geom_srid <- sf::st_crs(geom)

  if(circles_den=="auto"){
    rel <- x_ext/y_ext
    if(rel > 1){
      circles_x = 15
      circles_y = circles_x / rel
    }else{
      circles_y = 15
      circles_x = circles_y * rel
    }
  }else{
    circles_x = circles_den[1]-1
    circles_y = circles_den[2]-1
  }

  # x <- seq(x_min+10, x_max-10, by=x_ext/circles_x)
  # y <- seq(y_min+20, y_max-8, by=y_ext/circles_y)

  x <- seq(x_min, x_max, by=x_ext/circles_x)
  y <- seq(y_min, y_max, by=y_ext/circles_y)

  x <- x + (x[2] - x[1]) / 2
  y <- y + (y[2] - y[1]) / 2

  x <- x[1:length(x)-1]
  y <- y[1:length(y)-1]

  coords <- expand.grid(x, y)

  pnts <- list()

  for(row in 1:nrow(coords)){
    xy <- c(coords[row, 1], coords[row, 2])
    pnt <- sf::st_point(xy)
    pnts[[row]] <- pnt
  }

  sfc <- sf::st_sfc(pnts, crs=geom_srid)
  sf <- sf::st_sf(geom=sfc)
  sf::st_buffer(sf$geom, dist=30000)
}

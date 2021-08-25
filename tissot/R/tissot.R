#' This function generates Tissot's indicatrix
#'
#' This function returns equally-sized circles (radius = 300km) as sf-polygons
#' to use as Tissot's indicatrix. The circles are located on a regular grid in
#' lon/lat every 20 degrees.
#' @return circles as objects of class sf
#' @import sf
#' @param geom input geometry for which tissot should be generated
#' @export
# Constructor
tissot <- function (geom) {
  geom = sf::st_geometry(geom)
  circ = sf::st_geometry(make_indicatrix(geom))

  value <- list(geometry = geom, circles = circ)
  class(value) = "tissot"
  value
}

# Methods go here
get_geometry.tissot <- function(obj){
  return(obj$geometry)
}


get_indicatrix.tissot <- function(obj){
  return(obj$circles)
}

plot.tissot <- function(obj, srid){}
print.tissot <- function(){}
summarize.tissot <- function(){}

make_indicatrix = function(geom){

  geom_bbox <- sf::st_bbox(geom)
  x_min <- geom_bbox$xmin
  y_min <- geom_bbox$ymin
  x_max <- geom_bbox$xmax
  y_max <- geom_bbox$ymax

  geom_srid <- sf::st_crs(geom)

  x <- seq(x_min, x_max, by=abs((x_max - x_min))/19)
  y <- seq(y_min, y_max, by=abs((y_max - y_min))/19)

  coords <- expand.grid(x, y)

  pnts <- list()

  for(row in 1:nrow(coords)){
    xy <- c(coords[row, 1], coords[row, 2])
    pnt <- sf::st_point(xy)
    pnts[[row]] <- pnt
  }

  sfc <- sf::st_sfc(pnts, crs=geom_srid)
  sf <- sf::st_sf(geom=sfc)
  sf::st_buffer(sf$geom, dist=3000)
}

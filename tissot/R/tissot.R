#' This function generates Tissot's indicatrix
#'
#' This function returns equally-sized circles (radius = 300km) as sf-polygons
#' to use as Tissot's indicatrix. The circles are located on a regular grid in
#' lon/lat every 20 degrees.
#' @return circles as objects of class sf
#' @import sf
#' @export
# Constructor
tissot <- function (geom) {
  geom = sf::st_geometry(geom)
  value <- list(geometry = geom, circles = NA)
  class(value) = "tissot"
  value
}

# Methods go here
get_geometry.tissot <- function(obj){
  return(obj$geometry)
}


get_indicatrix.tissot <- function(){}
plot.tissot <- function(){}
print.tissot <- function(){}
summarize.tissot <- function(){}

tissot = function(){

  lat <- seq(-80, 80, by=20)
  lon <- seq(-160, 160, by=20)

  coords <- expand.grid(lon, lat)

  pnts <- list()

  for(row in 1:nrow(coords)){
    xy <- c(coords[row, 1], coords[row, 2])
    pnt <- sf::st_point(xy)
    pnts[[row]] <- pnt
  }

  sfc <- sf::st_sfc(pnts, crs=4326)
  sf <- sf::st_sf(geom=sfc)
  sf::st_buffer(sf$geom, dist=300000)
}

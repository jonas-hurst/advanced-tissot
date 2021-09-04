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
#' @param circle_size Specifies the size of each indicatrix circle. Default it is
#' automatic. Manually, size in Meters should be specified.
#' @export
# Constructor
tissot <- function (geom, circles_den="auto", circle_size = "auto") {
  srs = sf::st_crs(geom)
  if(st_crs(geom) != st_crs(4326)){
    geom = sf::st_transform(sf::st_geometry(geom), 4326)
  }

  l = suppressWarnings(make_indicatrix(geom,
                                       circles_den = circles_den,
                                       circle_size = circle_size))
  circ = l[[1]]
  circle_size = l[[2]]

  value <- list(crs = srs,
                geometry = geom,
                circles = circ,
                circlesize = circle_size)

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

#' This function plots the sf geometry and Tissot indicatrix circles that were generated
#' @param obj tissot object
#' @param crs target coordinate reference system: object of class 'crs', or input string for st_crs,
#' default is automatic
#' @import ggplot2
#' @export
plot.tissot <- function(obj, crs="auto"){

  if(crs!="auto"){
    plot_geom = sf::st_transform(obj$geometry, crs)
    plot_circles = sf::st_transform(obj$circles, crs)
  }else{
    plot_geom = sf::st_transform(obj$geometry, obj$crs)
    plot_circles = sf::st_transform(obj$circles, obj$crs)
  }

  plot_circles <- calc_areachange(plot_circles, obj$circlesize)

  ggplot2::ggplot(data = plot_geom) +
    ggplot2::geom_sf(color = "black") +
    ggplot2::geom_sf(data = plot_circles,
                     ggplot2::aes(fill = as.double(areachange))) +
    ggplot2::scale_fill_viridis_c(option = "plasma", trans = "sqrt", name = "Areachange")
}

#' This function prints Tissot indicatrix circles that were generated
#' @param obj tissot object
#' @export
print.tissot <- function(obj){
  print("This is a list of the generated Tissot indicatrix circles: ")
  print(obj$circles)
}

#' This function summarizes Tissot indicatrix circles details
#' @param obj tissot object
#' @export
summary.tissot <- function(obj){
  print("Summary of the sf geometry and Tissot Indicatrix circles.")
  geom_bbox <- sf::st_bbox(obj$geometry)
  print("The bounnding box of the input geometry is: ")
  print(geom_bbox)
  print("The generated indicatrix circles count is: ")
  circle_count <- nrow(obj$circles)
  print(circle_count)

}

make_indicatrix = function(geom, circles_den="auto", circle_size = "auto"){

  geom_bbox <- sf::st_bbox(geom)
  x_min <- geom_bbox$xmin
  y_min <- geom_bbox$ymin
  x_max <- geom_bbox$xmax
  y_max <- geom_bbox$ymax

  x_ext <- abs((x_max - x_min))
  y_ext <- abs((y_max - y_min))

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
    circles_x = circles_den[1]
    circles_y = circles_den[2]
  }

  #x <- seq(x_min, x_max-10, by=x_ext/circles_x)
  #y <- seq(y_min, y_max-8, by=y_ext/circles_y)

  # distribute circles symmetrically in norty-south direction
  if(y_min < 0 & y_max > 0){
    #create uneven number of circles so that circles are on equator
    if(circles_y %% 2 == 0 & circles_den == "auto"){
      circles_y = circles_y + 1
    }
    step = y_ext / (circles_y-1)
    end = step * (circles_y / 2)
    y <- seq(end * (-1), end , by=step)
    y = y[y>-75 & y<75]
  }else{
    y <- seq(y_min, y_max, by=y_ext/circles_y)
    y <- y + (y[2] - y[1]) / 2
    y <- y[1:length(y)-1]
  }

  x <- seq(x_min, x_max, by=x_ext/circles_x)
  x <- x + (x[2] - x[1]) / 2
  x <- x[1:length(x)-1]

  coords <- expand.grid(x, y)

  if(nrow(coords) == 0 | ncol(coords)<2){
    stop("Could not determine circle density automatically.
  Please specify manually through parameter circle_den")
  }


  pnts_df<- as.data.frame(coords)
  colnames(pnts_df) <- c("x","y")

  sf  <- sf::st_as_sf(pnts_df,coords=c("x","y"), crs= geom_srid)


  # calculate circle size
  if(circle_size == "auto"){
    dist = sf::st_distance(sf)
    circle_size = min(dist[as.integer(dist)>0]) * 0.4
  }

  circles <- sf::st_buffer(sf$geom, dist=circle_size)
  circles = sf::st_sf(geom=circles)
  return(list(circles, circle_size))

}

calc_areachange <- function(circles, circle_size) {
  true_area <- (3.14 * (circle_size)**2)

  circles$area <- sf::st_area(circles)

  circles$areachange <- (circles$area / true_area) * 100
  return(circles)
}

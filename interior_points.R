#' Interior points
#'
#' Takes a simple polygon and adds interior points
#' @param polygon A data frame containing the ordered (x, y) coordinates of the polygon
#' @param n The number of points to insert
#' @export
#' @examples
#' interior_points()

interior_points <- function(polygon, n) {
  # Set up a data frame for the interior points
  points <- data.frame(x = numeric(n), y = numeric(n))
  
  # Main loop
  for(i in 1:n) {
    inside <- FALSE
    while(!inside) {
      # Generate (x, y) coordinates at random
      x <- runif(1, min(polygon$x), max(polygon$x))
      y <- runif(1, min(polygon$y), max(polygon$y))
      # Check if the generated point is in the interior of the polygon
      if(point.in.polygon(x, y, polygon$x, polygon$y) == 1) {
        points[i, ] <- c(x, y)
        inside <- TRUE
      }
    }
  }
  points
}

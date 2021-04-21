#' Exterior points
#'
#' Takes a simple polygon and adds exterior points in the interior of its convex hull
#' @param polygon A data frame containing the ordered (x, y) coordinates of the polygon
#' @param n The number of points to insert
#' @export
#' @examples
#' exterior_points()

exterior_points <- function(polygon, n) {
  # Set up a data frame for the interior points
  points <- data.frame(x = numeric(n), y = numeric(n))
  hull <- polygon[chull(polygon), ]
  if(nrow(polygon) == nrow(hull) | n <= 0) {
    return(points)
  }
  
  # Main loop
  for(i in 1:n) {
    valid <- FALSE
    while(!valid) {
      # Generate (x, y) coordinates at random
      x <- runif(1, min(polygon$x), max(polygon$x))
      y <- runif(1, min(polygon$y), max(polygon$y))
      # Check if the generated point is in the interior of the polygon
      if(point.in.polygon(x, y, polygon$x, polygon$y) == 0 & point.in.polygon(x, y, hull$x, hull$y) == 1) {
        points[i, ] <- c(x, y)
        valid <- TRUE
      }
    }
  }
  points
}

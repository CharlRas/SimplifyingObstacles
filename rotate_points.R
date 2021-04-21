#' Rotate points
#'
#' Rotate a set of points by a given angle
#' @param points A data frame with x and y co-ordinates of the points to rotate
#' @param angle The rotation angle in radians
#' @export 
#' @examples
#' rotate_points()

rotate_points <- function(points, angle) {
  df <- points %>% mutate(x_dash = x * cos(angle) - y * sin(angle),
                          y_dash = x * sin(angle) + y * cos(angle)) %>%
    select(x_dash, y_dash) %>%
    rename(x = x_dash, y = y_dash)
  df
}

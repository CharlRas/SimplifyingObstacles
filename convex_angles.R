#' Convex angles
#'
#' Identifies convex angles in a simple polygon
#' @param polygon A data frame with the ordered (x, y) coordiantes defining the polygon
#' @export
#' @examples
#' convex_angles()

convex_angles <- function(polygon) {
  points <- polygon %>% mutate(id = 1:nrow(.))
  c_hull <- points[points %>% select(x, y, id) %>% chull(), ]
  points <- points %>% mutate(hull = ifelse(id %in% c_hull$id, TRUE, FALSE))
  sort_at_end <- FALSE
  
  # If convex angles make a right turn, reverse the id numbers
  temp <- c_hull %>% select(id) %>%
    mutate(delta = id - lag(id, default = .$id[nrow(.)]),
           flag = ifelse(delta < 0 , 1, 0))
  if(sum(temp$flag) == 1) {
    points <- points %>% mutate(id = nrow(.) + 1 - id) %>% arrange(id)
    polygon <- polygon %>% mutate(id = seq(nrow(.), 1)) %>% arrange(id) %>% select(-id)
    c_hull <- points[points %>% select(x, y, id) %>% chull(), ]
    sort_at_end <- TRUE
  }
  
  # Create data frame with terminals and polygon vertices combined
  points <- points %>% select(x, y, id) %>% mutate(convex = FALSE)
  
  # Main loop
  i <- nrow(points)
  j <- 1
  k <- 2
  while(j <= nrow(points)) {
    v1 <- points[i, ]
    v2 <- points[j, ]
    v3 <- points[k, ]
    # Main loop
    # Compute the angle at v2
    vector1 <- points[points$id == v1$id[1], c("x", "y")] - points[points$id == v2$id[1], c("x", "y")]
    vector2 <- points[points$id == v3$id[1], c("x", "y")] - points[points$id == v2$id[1], c("x", "y")]
    vectors <- rbind(vector1, vector2)
    angle <- pi/2 - atan2(vector2$y, vector2$x)
    rotate <- rotate_points(vectors, angle)
    if(rotate$y[2] > 0 & rotate$x[1] < 0 | rotate$y[2] < 0 & rotate$x[1] > 0) {
      # Angle is > 180 deg. Mark as non-convex.
      points$convex[j] <- TRUE
    } else {
      # Angle is < 180 deg. Mark as convex.
      points$convex[j] <- FALSE
    }
    i <- ifelse(i == nrow(points), 1, i + 1)
    j <- j + 1
    k <- ifelse(k == nrow(points), 1, k + 1)
  }
  if(sort_at_end) {
    points <- points %>% mutate(id = nrow(.) + 1 - id) %>% arrange(id)
  }
  points
}

#' Trim obstacle
#'
#' Trims a given an obstacle in a given fixed-orientation metric
#' @param polygon A data frame containing the ordered (x, y) coordinates defining the obstacle
#' @param directions Vector of angles in [0, pi)], in radians, defining fixed orientation directions measured from the positive x-direction
#' @param left_turn If true, obstacle traversal is in the direction for which convex angles make a left turn
#' @export
#' @examples
#' trim_obstacle()

trim_obstacle <- function(polygon, directions, left_turn = TRUE) {
  # Ensure that convex angles turn in the direction (left / right) specified
  temp <- data.frame(id = chull(polygon)) %>%
    mutate(delta = id - lag(id, default = .$id[nrow(.)]), flag = ifelse(delta < 0, 1, 0))
  if(sum(temp$flag) == 1) {
    # Convex angles make a right turn
    if(left_turn) {
      polygon <- polygon %>%
        mutate(id = seq(nrow(.), 1)) %>%
        arrange(id) %>%
        select(-id)
    }
  } else {
    # Convex angles make a left turn
    if(!left_turn) {
      polygon <- polygon %>%
        mutate(id = seq(nrow(.), 1)) %>%
        arrange(id) %>%
        select(-id)
    }
  }
  
  # Initialise indexes
  n <- nrow(polygon)
  
  i <- n
  j <- 1
  k <- 2
  
  angles <- convex_angles(polygon)
  
  polygon <- polygon %>% mutate(id = 1:n, invisible = FALSE)
  
    while(j <= n) {
    # Update points
    v_i <- as.numeric(polygon[i, ])
    v_j <- as.numeric(polygon[j, ])
    v_k <- as.numeric(polygon[k, ])
    
    # 1. Check if the line segment between v_i and v_k lies entirely in the obstacle
    
    # (a) Check if the interior angle at v_j is convex
    # Recompute the angles
    angles2 <- convex_angles(polygon %>% filter(!invisible)) %>%
      select(-id) %>%
      left_join(polygon %>% select(x, y, id), by = c("x", "y")) %>%
      select(x, y, convex, id)
    
    angles <- angles2
    
    
    if(!angles[angles$id == j, "convex"]) {
      i <- j
      j <- ifelse(nrow(polygon %>% filter(!invisible, id > j)) == 0, n + 1, k)
      temp <- polygon %>% filter(!invisible)
      k <- ifelse(k == n, temp[1, "id"], k + 1)
      next
    }
    
    # (b) Check if an obstacle vertex is inside the triangle v_iv_jv_k
    triangle_x <- c(v_i[1], v_j[1], v_k[1])
    triangle_y <- c(v_i[2], v_j[2], v_k[2])
    polygon_vis <- polygon %>% filter(!invisible)
    check <- point.in.polygon(polygon_vis$x, polygon_vis$y, triangle_x, triangle_y)
    if(sum(check > 0) > 3) {
      # There is at least one obstacle vertex in the triangle
      i <- j
      j <- ifelse(nrow(polygon %>% filter(!invisible, id > j)) == 0, n + 1, k)
      temp <- polygon %>% filter(!invisible)
      k <- ifelse(k == n, temp[1, "id"], k + 1)
      next
    }
    
    # 2. Check if the path vivjvk lies in the edge parallelogram of vivk (for the given fixed orientation metric)
    # (equivalent to saying that the vectors vivj and vjvk lie on or between two adjacent legal orientations.)
    vivj <- v_j - v_i
    vjvk <- v_k - v_j
    theta_ij <- atan2(vivj[2], vivj[1])
    theta_jk <- atan2(vjvk[2], vjvk[1])
    test_ij <- theta_ij > directions
    test_jk <- theta_jk > directions
    if(identical(test_ij, test_jk)) {
      polygon$invisible[j] <- TRUE
      if(nrow(polygon %>% filter(!invisible, id < j)) == 0) {
        i <- i
        j <- k
        k <- k + 1
      } else {
        j <- i
        temp <- polygon %>% filter(!invisible)
        temp2 <- temp %>% filter(id < i)
        i <- ifelse(nrow(temp2) == 0, temp[nrow(temp), "id"], temp2[nrow(temp2), "id"])
        k <- k
      }
      next
    }
    i <- j
    j <- ifelse(nrow(polygon %>% filter(!invisible, id > j)) == 0, n + 1, k)
    temp <- polygon %>% filter(!invisible)
    k <- ifelse(k == n, temp[1, "id"], k + 1)
  }
  polygon %>% select(x, y, invisible)
}

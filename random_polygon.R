#' Generate random polygon
#'
#' Generates a simple random polygon. Returns a data frame with the ordered vertex co-ordinates and data for animating the algorithm
#' @param points A data frame with the (x, y) coordinates of the points (if left blank, points are generated at random)
#' @param n If points not given, the number of polygon vertices to be randomly-generated
#' @param X The maximum x-coordinate of the polygon
#' @param Y The maximum y-coordinate of the polygon
#' @export
#' @examples
#' random_polygon()

random_polygon <- function(points = NA, n = 0, X = 10000, Y = 10000) {
  i <- 1
  animate <- vector("list", n^4)

  # Generate n polygon vertices at random
  if(sum(is.na(points)) > 0) {
    points <- data.frame(x = runif(n, 0, X), y = runif(n, 0, Y)) %>%
      mutate(pt = 1:n) %>%
      select(pt, x, y)
  } else {
    n <- nrow(points)
    points <- points %>% mutate(pt = 1:n)
  }

  # Store edges in a data frame
  edges <- points %>%
    select(-pt) %>%
    mutate(pt1 = 1:n, pt2 = c(2:n, 1),
           xend = lead(x, default = .$x[1]), yend = lead(y, default = .$y[1]), edge = 1:n) %>%
    select(edge, pt1, pt2, x, y, xend, yend)

  # Function for computing edge intersections
  find_intersections <- function(edges) {
    intersections <- data.frame(e1 = rep(1:n, times = n)) %>%
      arrange(e1) %>%
      mutate(e2 = rep(1:n, times = n), remove = ifelse(e2 <= e1, TRUE, FALSE)) %>%
      filter(!remove) %>%
      select(e1, e2) %>%
      left_join(edges, by = c("e1" = "edge")) %>%
      rename(pt1_1 = pt1, pt2_1 = pt2, x1 = x, y1 = y, xend1 = xend, yend1 = yend) %>%
      left_join(edges, by = c("e2" = "edge")) %>%
      rename(pt1_2 = pt1, pt2_2 = pt2, x2 = x, y2 = y, xend2 = xend, yend2 = yend) %>%
      rowwise() %>%
      mutate(intersect = does_intersect(c(x1, y1), c(xend1, yend1), c(x2, y2), c(xend2, yend2))) %>%
      select(e1, pt1_1, pt2_1, x1, y1, xend1, yend1, e2, pt1_2, pt2_2, x2, y2, xend2, yend2, intersect)
  }

  # Iterative procedure
  intersections <- find_intersections(edges)
  n_intersections <- nrow(intersections %>% filter(intersect))

  while(n_intersections > 0) {
    # Choose a pair of intersecting edges at random
    rand_cross <- sample_n(intersections %>% filter(intersect), 1)

    ei <- rand_cross$e1[1]
    ej <- rand_cross$e2[1]

    # Determine vi and v_{i+1}
    if(rand_cross$pt1_1[1] == 1 & rand_cross$pt2_1[1] == n) {
      vi   <- rand_cross$pt2_1[1]
      vip1 <- rand_cross$pt1_1[1]
    } else if(rand_cross$pt1_1[1] == n & rand_cross$pt2_1[1] == 1) {
      vi   <- rand_cross$pt1_1[1]
      vip1 <- rand_cross$pt2_1[1]
    } else if(rand_cross$pt1_1[1] < rand_cross$pt2_1[1]) {
      vi   <- rand_cross$pt1_1[1]
      vip1 <- rand_cross$pt2_1[1]
    } else {
      vi   <- rand_cross$pt2_1[1]
      vip1 <- rand_cross$pt1_1[1]
    }

    # Determine vj and v_{j+1}
    if(rand_cross$pt1_2[1] == 1 & rand_cross$pt2_2[1] == n) {
      vj  <- rand_cross$pt2_2[1]
      vjp1 <- rand_cross$pt1_2[1]
    } else if(rand_cross$pt1_2[1] == n & rand_cross$pt2_2[1] == 1) {
      vj   <- rand_cross$pt1_2[1]
      vjp1 <- rand_cross$pt2_2[1]
    } else if(rand_cross$pt1_2[1] < rand_cross$pt2_2[1]) {
      vj   <- rand_cross$pt1_2[1]
      vjp1 <- rand_cross$pt2_2[1]
    } else {
      vj   <- rand_cross$pt2_2[1]
      vjp1 <- rand_cross$pt1_2[1]
    }

    # Get coordinates of vip1
    if(rand_cross[1, "pt1_1"] == vip1) {
      vip1_xy <- c(rand_cross$x1[1], rand_cross$y1[1])
    } else {
      vip1_xy <- c(rand_cross$xend1[1], rand_cross$yend1[1])
    }

    # Get coordinates of vj
    if(rand_cross[1, "pt1_2"] == vj) {
      vj_xy <- c(rand_cross$x2[1], rand_cross$y2[1])
    } else {
      vj_xy <- c(rand_cross$xend2[1], rand_cross$yend2[1])
    }

    # Swap vip1 and vj
    if(edges[edges$edge == ei, "pt1"] == vip1) {
      edges[edges$edge == ei, c("x", "y")] <- vj_xy
      edges[edges$edge == ei, "pt1"] <- vj
    } else {
      edges[edges$edge == ei, c("xend", "yend")] <- vj_xy
      edges[edges$edge == ei, "pt2"] <- vj
    }

    # Swap vj and vip1
    if(edges[edges$edge == ej, "pt1"] == vj) {
      edges[edges$edge == ej, c("x", "y")] <- vip1_xy
      edges[edges$edge == ej, "pt1"] <- vip1
    } else {
      edges[edges$edge == ej, c("xend", "yend")] <- vip1_xy
      edges[edges$edge == ej, "pt2"] <- vip1
    }

    # Reverse all the vertex indexes between i_min and i_max
    v_min <- min(vi, vj)
    v_max <- max(vi, vj)
    temp <- data.frame(before = 1:n) %>%
      filter(before > v_min, before <= v_max)
    after <- temp %>% arrange(-before) %>% rename(after = before)
    lookup1 <- cbind(temp, after)
    lookup2 <- data.frame(before = 1:n) %>%
      filter(before <= v_min | before > v_max) %>%
      mutate(after = before)
    lookup <- rbind(lookup1, lookup2)

    edges <- edges %>%
      left_join(lookup, by = c("pt1" = "before")) %>%
      select(-pt1) %>%
      rename(pt1 = after) %>%
      left_join(lookup, by = c("pt2" = "before")) %>%
      select(-pt2) %>%
      rename(pt2 = after)

    intersections <- find_intersections(edges)
    n_intersections <- nrow(intersections %>% filter(intersect))
    temp <- edges %>% mutate(frame = i)
    animate[[i]] <- temp
    i <- i + 1
    # print(paste("iteration:", i, sep = " "))
  }

  animate <- animate[1:(i-1)]

  # Create data for animation
  if(length(animate) < 2) {
    tf <- animate[[1]]
  } else {
    tf <- tween_states(animate, tweenlength = 8, statelength = 1, ease = "linear", nframes = 200)
  }

  # Store the polygon in a data frame
  polygon <- data.frame(x = numeric(n), y = numeric(n))
  temp <- edges %>% mutate(used = FALSE)
  polygon[1, ] <- edges[1, c("x", "y")]
  polygon[2, ] <- edges[1, c("xend", "yend")]
  temp[1, "used"] <- TRUE
  pt <- temp[1, "pt2"]
  k <- 3
  while(k <= n) {
    edge <- temp %>% filter(!used, pt1 == pt | pt2 == pt)
    polygon[k, c("x")] <- ifelse(edge$pt1[1] == pt, edge$xend[1], edge$x[1])
    polygon[k, c("y")] <- ifelse(edge$pt1[1] == pt, edge$yend[1], edge$y[1])
    temp[temp$used == FALSE & (temp$pt1 == pt | temp$pt2 == pt), "used"] <- TRUE
    pt <- ifelse(edge$pt1[1] == pt, edge$pt2[1], edge$pt1[1])
    k <- k + 1
  }

  list(polygon = polygon, animation = tf)
}

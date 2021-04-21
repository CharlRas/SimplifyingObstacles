#' Backfill polygon
#'
#' Backfill a polygon
#' @param polygon A data frame with polygon (x, y) coordinates
#' @param terminals A data frame with terminal (x, y) coordinates
#' @param pad If TRUE, applies padding sub-routine
#' @param pad_vertex If TRUE, applies padding around obstacle vertices
#' @return A list containing data frames for polygon, backfill, padding, boundary and data
#' @export
#' @examples
#' backfill_polygon()

backfill_polygon <- function(polygon, terminals, pad = TRUE, pad_vertex = FALSE) {
  # Determine which points are on the boundary of the convex hull
  points <- polygon %>% mutate(id = 1:nrow(.))
  c_hull <- points[points %>% select(x, y, id) %>% chull(), ]
  points <- points %>% mutate(hull = ifelse(id %in% c_hull$id, TRUE, FALSE))
  
  # If angles at convex vertices make a right turn, reverse the id numbers
  temp <- c_hull %>% select(id) %>%
    mutate(delta = id - lag(id, default = .$id[nrow(.)]),
           flag = ifelse(delta < 0 , 1, 0))
  if(sum(temp$flag) == 1) {
    points <- points %>% mutate(id = nrow(.) + 1 - id) %>% arrange(id)
    polygon <- polygon %>% mutate(id = seq(nrow(.), 1)) %>% arrange(id) %>% select(-id)
    c_hull <- points[points %>% select(x, y, id) %>% chull(), ]
  }
  
  # Compute covers
  covers <- points %>%
    mutate(id_next = lead(id, default = .$id[1]), hull_next = lead(hull, default = .$hull[1])) %>%
    select(x, y, id, id_next, hull, hull_next) %>%
    filter(hull) %>%
    mutate(id2 = lead(id, default = .$id[1])) %>%
    filter(!hull_next) %>%
    select(id, id2)
  
  # Create data frame with terminals and polygon vertices combined
  all_points <- rbind(points %>% select(x, y), terminals %>% select(x, y))
  
  # Function for filling cavity
  fill_cavity <- function(ids) {
    start <- which(points$id == ids$id[1])
    end <- which(points$id == ids$id2[1])
    transparent <- points %>% mutate(transparent = FALSE, boundary = TRUE)
    if(start > end) {
      path <- c(points$id[start:(nrow(points))], points$id[1:end])
    } else {
      path <- points$id[start:end]
    }
    path_df <- data.frame(id = path, visible = TRUE) %>% left_join(points %>% select(id, x, y), by = "id")
    path_df <- path_df[!duplicated(path_df), ]
    boundary <- path_df %>% select(x, y) %>% mutate(occupant = NA, type = "corner", i = 1:nrow(.))
    
    backfill <- data.frame(x = numeric(0), y = numeric(0), frame = integer(0))
    padding <- data.frame(x = numeric(0), y = numeric(0), frame = integer(0))
    
    # Initialise index values
    a <- as.numeric(path_df[1, c("x", "y")])
    a_last <- a
    a_history <- data.frame(x = numeric(0), y = numeric(0))
    a_history[1, ] <- a_last
    b <- as.numeric(path_df[2, c("x", "y")])
    c <- as.numeric(path_df[3, c("x", "y")])
    terminate <- FALSE
    k <- nrow(path_df)
    v1 <- as.numeric(path_df[1, c("x", "y")])
    frame <- 1
    boundary$occupant[1] <- "a"
    boundary$occupant[2] <- "b"
    boundary$occupant[3] <- "c"
    
    # Main loop
    while(!terminate) {
      # Compute the angle at v2
      vector1 <- a - b
      vector2 <- c - b
      vectors <- as.data.frame(rbind(vector1, vector2)) %>% rename(x = V1, y = V2)
      angle <- pi/2 - atan2(vector2[2], vector2[1])
      rotate <- rotate_points(vectors, angle)
      a_row <- which(boundary$occupant == "a")
      b_row <- which(boundary$occupant == "b")
      c_row <- which(boundary$occupant == "c")
      if((rotate$y[2] > 0 & rotate$x[1] < 0 | rotate$y[2] < 0 & rotate$x[1] > 0) |
         (boundary[b_row, "type"] == "terminal")) {
        # Angle is > 180 deg OR b is a terminal. Don't backfill. Move a,b,c forward.
        if(c_row == nrow(boundary)) {
          terminate <- TRUE
        } else {
          boundary$occupant[boundary$occupant == "a"] <- NA
          boundary$occupant[boundary$occupant == "b"] <- "a"
          boundary$occupant[boundary$occupant == "c"] <- "b"
          a_history <- rbind(a_history, a)
          a <- b
          b <- c
          c <- as.numeric(boundary[c_row+1, c("x", "y")])
          boundary$occupant[c_row+1] <- "c"
        }
      } else {
        # Angle is < 180 deg. Check if triangle contains a terminal or obstacle vertex
        point_in <- FALSE
        df <- data.frame(x = c(a[1], b[1], c[1]),
                         y = c(a[2], b[2], c[2]), frame = frame)
        all_points <- rbind(path_df %>% select(x, y), terminals %>% select(x, y))
        temp <- all_points %>%
          mutate(in_triangle = point.in.polygon(.$x, .$y, df$x, df$y),
                 in_triangle = (in_triangle == 1)) %>%
          filter(in_triangle) %>%
          mutate(angle = Inf)
        if(nrow(temp) > 0) {
          point_in <- TRUE
        }
        if(!point_in & boundary[b_row, "type"] == "corner") {
          # No terminal inside the triangle AND b is a corner. Flag b as being transparent. Do backfill.
          backfill <- rbind(backfill, df)
          path_df[path_df$x == b[1] & path_df$y == b[2], "visible"] <- FALSE
          frame <- frame + 1
          transparent[transparent$x == b[1] & transparent$y == b[2], "transparent"] <- TRUE
          transparent[transparent$x == b[1] & transparent$x == b[2], "boundary"] <- FALSE
          if(identical(a, v1)) {
            # If a is at v1, move b and c forward
            if(c_row == nrow(boundary)) {
              boundary <- boundary[-b_row, ] # Remove b from boundary
              terminate <- TRUE
            } else {
              b <- c
              boundary$occupant[boundary$occupant == "c"] <- "b"
              c <- as.numeric(boundary[c_row+1, c("x", "y")])
              boundary$occupant[c_row+1] <- "c"
              boundary <- boundary[-b_row, ] # Remove b from boundary
            }
          } else {
            # Otherwise, backtrack a and b
            b <- a
            boundary$occupant[boundary$occupant == "b"] <- NA
            boundary$occupant[boundary$occupant == "a"] <- "b"
            a <- as.numeric(boundary[a_row-1, c("x", "y")])
            boundary$occupant[a_row-1] <- "a"
            if(nrow(a_history) == 1) {
              a_history <- data.frame(x = numeric(0), y = numeric(0))
            } else {
              a_history <- a_history[seq(1, nrow(a_history)-1), ]
            }
            boundary <- boundary[-b_row, ] # Remove b from boundary
          }
        } else {
          # There is a point inside the triangle. Pad the triangle. Flag v_j as being transparent.
          # Move a forward to terminal location.
          if(pad) {
            # Padding on - apply padding around terminal
            # Compute the angles bap for all points p in the triangle
            ab <- sqrt(sum((a - b)^2))
            temp <- temp %>%
              rowwise() %>%
              mutate(ap = sqrt(sum((a - c(x, y))^2)), bp = sqrt(sum((b - c(x, y))^2)),
                     angle = acos((ap^2 + ab^2 - bp^2) / (2 * ap * ab))) %>%
              select(-ap, -bp) %>%
              arrange(angle)
            # If the point with the smallest angle is an obstacle vertex, move a, b, c forward
            if(nrow(polygon %>% select(x, y) %>% filter(x == temp$x[1], y == temp$y[1])) > 0 & !pad_vertex) {
              transparent[transparent$x == b[1] & transparent$y == b[2], "transparent"] <- TRUE
              a_history <- rbind(a_history, a)
              if(c_row == nrow(boundary)) {
                terminate <- TRUE
              } else {
                boundary$occupant[boundary$occupant == "a"] <- NA
                a <- b
                boundary$occupant[boundary$occupant == "b"] <- "a"
                b <- c
                boundary$occupant[boundary$occupant == "c"] <- "b"
                c <- as.numeric(boundary[c_row+1, c("x", "y")])
                boundary$occupant[c_row+1] <- "c"
              }
            } else {
              # Pad the region
              transparent[transparent$x == b[1] & transparent$y == b[2], "transparent"] <- TRUE
              df2 <- data.frame(x = c(a[1], b[1], temp$x[1]),
                                y = c(a[2], b[2], temp$y[1]), frame = frame)
              padding <- rbind(padding, df2)
              a_history <- a_history %>% rbind(temp[1, ] %>% select(x, y))
              if(identical(a, v1) | boundary[a_row, "type"] == "terminal") {
                # Insert a row for p into the boundary data frame (in between current a and b)
                boundary$occupant[boundary$occupant == "a"] <- NA
                previous_ai <- boundary$i[a_row]
                previous_bi <- boundary$i[b_row]
                boundary <- boundary %>%
                  rbind(data.frame(x = as.numeric(temp[1, "x"]), y = as.numeric(temp[1, "y"]),
                                   occupant = "a", type = "terminal",
                                   i = mean(c(previous_ai, previous_bi)))) %>%
                  arrange(i)
                # Move a to p
                a <- as.numeric(temp[1, c("x", "y")])
              } else {
                # Move a to a_last, b to a, c to p
                boundary$occupant[c_row] <- NA
                boundary$occupant[b_row] <- NA
                boundary$occupant[a_row] <- "b"
                boundary$occupant[a_row-1] <- "a"
                b <- a
                a <- as.numeric(boundary[a_row-1, c("x", "y")])
                c <- as.numeric(temp[1, c("x", "y")])
                previous_ai <- boundary$i[a_row]
                previous_bi <- boundary$i[b_row]
                boundary <- boundary %>%
                  rbind(data.frame(x = as.numeric(temp[1, "x"]), y = as.numeric(temp[1, "y"]),
                                   occupant = "c", type = "terminal",
                                   i = mean(c(previous_ai, previous_bi)))) %>%
                  arrange(i)
              }
              frame <- frame + 1
            }
          } else {
            # Padding switched off. Flag b as being transparent. Move a,b,c forward by one.
            transparent[transparent$x == b[1] & transparent$y == b[2], "transparent"] <- TRUE
            a_history <- rbind(a_history, a)
            if(c_row == nrow(boundary)) {
              terminate <- TRUE
            } else {
              boundary$occupant[boundary$occupant == "a"] <- NA
              a <- b
              boundary$occupant[boundary$occupant == "b"] <- "a"
              b <- c
              boundary$occupant[boundary$occupant == "c"] <- "b"
              c <- as.numeric(boundary[c_row+1, c("x", "y")])
              boundary$occupant[c_row+1] <- "c"
            }
          }
        }
      }
    }
    list(backfill = backfill, padding = padding, transparent = transparent, boundary = boundary)
  }
  
  # Fill cavity for all covers
  result <- split(covers, seq(nrow(covers))) %>%
    map(~fill_cavity(.))
  
  backfill <- result %>%
    map_df("backfill", .id = "cavity") %>%
    mutate(frame = frame + as.integer(cavity) * 1000)
  
  padding <- result %>%
    map_df("padding", .id = "cavity") %>%
    mutate(frame = frame + as.integer(cavity) * 1000)
  
  transparent <- result %>%
    map_df("transparent", .id = "cavity") %>%
    select(id, transparent, boundary) %>%
    group_by(id) %>%
    summarise(n_transparent = sum(transparent), n_boundary = sum(boundary)) %>%
    mutate(transparent = (n_transparent > 0), boundary = (n_boundary > 0)) %>%
    select(id, transparent, boundary)
  
  boundary <- result %>%
    map_df("boundary", .id = "cavity")
  
  polygon <- cbind(polygon, transparent %>% select(transparent, boundary))
  data <- c(vertices = nrow(polygon), terminals = nrow(terminals), pad = pad,
            surviving = nrow(polygon %>% filter(!transparent)),
            edges = length(chull(polygon)) + nrow(boundary) - 2 * nrow(boundary %>% group_by(cavity) %>% summarise()))
  list(polygon = polygon, backfill = backfill, padding = padding, boundary = boundary, data = data)
}

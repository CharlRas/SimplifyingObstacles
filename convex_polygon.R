convex_polygon <- function(n) {
  df <- data.frame(x = runif(3, 0, 10000), y = runif(3, 0, 10000))
  ch <- df[chull(df), ]
  animate <- vector("list", n-2)
  animate[[1]] <- ch
  
  k <- 3
  
  while(k < n) {
    valid <- FALSE
    while(!valid) {
      # Generate a random point
      pt <- runif(2, 0, 10000)
      # Compute the convex hull of df and the new point
      temp <- rbind(df, pt)
      temp <- temp[chull(temp), ]
      if(nrow(temp) == k+1) {
        df <- temp
        ch <- df[chull(df), ]
        valid <- TRUE
      }
    }
    animate[[k-2]] <- ch %>% mutate(frame = k-2)
    k <- k + 1
  }
  # Create data for animation
  tf <- do.call(rbind.data.frame, animate)
  # Return a list with vertex co-ordinates and animation data
  list(polygon = ch, animation = tf)
}
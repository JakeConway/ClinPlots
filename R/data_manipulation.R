addYposition <- function(data) {
  data$y <- 1:nrow(data)
  return(data)
}

flattenToX <- function(data) {
  mins <- data$min
  maxs <- data$max
  x_positions <- c(mins, maxs)
  y_positions <- rep(data$y, 2)
  return(data.frame(x = x_positions, y = y_positions))
}

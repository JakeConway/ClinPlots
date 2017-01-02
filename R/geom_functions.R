addLines <- function(plot, line_data) {
  plot <- plot + geom_line(data = line_data, aes(x = x, y = y, group = y))
  return(plot)
}

addPoints <- function(plot, data){
  plot <- (plot + geom_point(data = data, aes(x = avg, y = disorder))
  + scale_y_discrete())
  return(plot)
}

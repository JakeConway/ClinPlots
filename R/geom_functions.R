addLines <- function(plot, line_data) {
  plot <- plot + geom_line(data = line_data, aes(x = x, y = y, group = y))
  return(plot)
}

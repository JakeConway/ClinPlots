addLines <- function(plot, line_data) {
  plot <-
    plot + geom_line(data = line_data, aes(x = x, y = y, group = y))
  return(plot)
}

addPoints <- function(plot, data) {
  plot <- (plot + geom_point(data = data, aes(x = avg, y = disorder, shape=17))
           + scale_y_discrete(position = 'right') + scale_shape_identity())
  return(plot)
}

addCountText <- function(plot, data) {
  data$x <- 1
  plot <-(plot + geom_text(data = data, aes(x = x, y = disorder, label = n))
      + ggtitle('n')
      + theme(
        panel.background = element_rect(fill = 'white', colour = 'white'),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
    )
  return(plot)
}

addHeaderLine <- function(plot, data) {
  plot <- plot + annotate('segment', x = 0, xend = 2, y = nrow(data)+1, yend = nrow(data)+1)
  return(plot)
}

addLines <- function(plot, line_data) {
  plot <- plot + geom_line(data = line_data, aes(x = x, y = y, group = y))
  return(plot)
}

addPoints <- function(plot, data) {
  plot <- (plot + geom_point(data = data, aes(x = avg, y = disorder, shape = 17))
    + scale_y_discrete(position = 'right') + scale_shape_identity()
    + ggtitle('Disorder')
    + xlab('Clinical risk (%)')
    + theme(
      # A lower hjust moves to the left, and a higher hjust to the right
      plot.background = element_rect(fill = 'white', colour = 'white'),
      panel.background = element_rect(fill = 'white', colour = 'white'),
      plot.title = element_text(hjust = 1.27, size = 10),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 9),
      axis.line.x = element_line(colour = 'black', size = 0.4),
      axis.line.y = element_line(colour = 'black', size = 0.4),
      axis.text.x = element_text(colour = 'black', size = 8),
      axis.text.y = element_text(colour = 'black', size = 8)
    )
  )
  return(plot)
}

addText <- function(plot, data, y, title, size) {
  data$x <- 1
  plot <-(plot + geom_text(data = data, aes_string(x = "x", y = y, label = "n"), size = size)
      + theme(
        panel.background = element_rect(fill = 'white', colour = 'white'),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
      + scale_y_discrete()
    )
  return(plot)
}

addHeaderLine <- function(plot, data) {
  plot <- plot + annotate('segment', x = 0, xend = 2, y = nrow(data)+1, yend = nrow(data)+1)
  return(plot)
}

addLines <- function(plot, line_data) {
  plot <- plot + geom_line(data = line_data, aes(x = x, y = y, group = group))
  return(plot)
}

addPoints <- function(plot, data, title, x, y,  x_label, x_is_y) {
  plot <- (plot + geom_point(data = data, aes_string(x = x, y = y),
                             shape = data$shape,
                             colour = data$color,
                             size = 3)
           + scale_shape_identity()
           )
    if(x_is_y){
      plot <- (plot
               +scale_x_discrete()
               + scale_y_continuous(labels = percent, breaks = pretty_breaks(n = 3))
               + labs(title = title, y = x_label)
      )
    } else {
      plot <- (plot
               +scale_x_continuous(labels = percent)
               + scale_y_discrete(position = 'right')
               + labs(title = title, x = x_label)
      )
    }
  return(plot)
}

addDisorderCohortRiskTheme <- function(plot) {
  plot <- (plot
           # A lower hjust moves to the left, and a higher hjust to the right
           + theme(
             plot.background = element_rect(fill = 'white', colour = 'white'),
             panel.background = element_rect(fill = 'white', colour = 'white'),
             plot.title = element_text(hjust = 1, vjust = 0.5, size = 10),
             axis.title.x = element_text(size = 9),
             axis.title.y = element_blank(),
             axis.line.x = element_line(colour = 'black', size = 0.4),
             axis.line.y = element_line(colour = 'black', size = 0.4),
             axis.text.x = element_text(colour = 'black', size = 8),
             axis.text.y = element_text(colour = 'black', size = 8)
    )
  )
  return(plot)
}

addGenomicRiskSummaryTheme <- function(plot) {
  plot <- (plot
           +theme(
             plot.background = element_rect(fill = 'white', colour = 'white'),
             panel.background = element_rect(fill = 'white', colour = 'white'),
             plot.title = element_text(colour = 'white', size = 10),
             axis.title.x = element_text(size = 9),
             axis.title.y = element_blank(),
             axis.line.x = element_line(colour = 'black', size = 0.4),
             axis.line.y = element_blank(),
             axis.text.x = element_text(colour = 'black', size = 8),
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank()
           )
           + coord_flip()
  )
}

addText <- function(plot, data, y, title, size) {
  data$x <- 1
  plot <-(plot + geom_text(data = data, aes_string(x = "x", y = y, label = title), size = size)
      + ggtitle(title)
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

addHeaderLine <- function(plot, extend, x_is_y) {
  x_limits <- xScaleLimits(plot)
  min <- 0
  max <- x_limits[2]*100000
  if(!extend || is.null(x_limits)) max <- Inf
  plot <- plot + annotation_custom(segmentsGrob(), xmin = min, xmax = max, ymin = 8.6, ymax = 8.6)
  return(plot)
}

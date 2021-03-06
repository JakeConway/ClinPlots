addLines <- function(plot, line.data) {
  plot <- (plot + geom_line(data = line.data, aes(x = x, y = y, group = group, colour = line.color))
           + scale_colour_identity())
  return(plot)
}

addPoints <- function(plot, data, title, x, y,  x.label, x.is.y) {
  plot <- (plot + geom_point(data = data, aes_string(x = x, y = y),
                             shape = data$shape,
                             colour = data$color,
                             size = 3)
           + scale_shape_identity()
           )
    if(x.is.y){
      plot <- (plot
               +scale_x_discrete()
               + scale_y_continuous(labels = percent, breaks = pretty_breaks(n = 3))
               + labs(title = title, y = x.label)
      )
    } else {
      plot <- (plot
               +scale_x_continuous(labels = percent)
               + scale_y_discrete(position = 'right')
               + labs(title = title, x = x.label)
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

removeMargin <- function(plot, top, right, bottom, left) {
  plot <- plot + theme(plot.margin = unit(c(top, right, bottom, left), "cm"))
  return(plot)
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

addHeaderLine <- function(plot, n.rows, extend, x.is.y) {
  if(!x.is.y){
    x.limits <- xScaleLimits(plot)
    min.x.position <- 0
    max.x.position <- x.limits[2]*2
    if(!extend || is.null(x.limits)) max.x.position <- Inf
    min.y.position <- n.rows*1.075
    max.y.position <- min.y.position
  } else {
    y.limits <- yScaleLimits(plot)
    min.y.position <- 0
    max.y.position <- y.limits[2]*2
    if(!extend || is.null(y.limits)) max.y.position <- Inf
    min.x.position <- n.rows*1.075
    max.x.position <- min.x.position
  }
  plot <- plot + annotation_custom(segmentsGrob(), xmin = min.x.position, xmax = max.x.position,
                                   ymin = min.y.position, ymax = max.y.position)
  return(plot)
}

addDisorderTitle <- function(plot, n.rows) {
  x.limits <- xScaleLimits(plot)
  max <- x.limits[2]*1.15
  y.position <- n.rows*1.105
  plot <- plot + annotation_custom(textGrob(label = 'Disorder', gp = gpar(cex = 0.85)),
                                   xmin = max, xmax = max,
                                   ymin = y.position, ymax = y.position)
  return(plot)
}

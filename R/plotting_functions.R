xScaleLimits <- function(plot) {
  return(layer_scales(plot)$x$range$range)
}

yScaleLimits <- function(plot) {
  return(layer_scales(plot)$y$range$range)
}

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

toGrobs <- function(plot.list) {
  grobs <- lapply(plot.list, function(x){
    x <- ggplot_gtable(ggplot_build(x))
    x$layout$clip[x$layout$name == 'panel'] <- 'off'
    return(x)
  })
  return(grobs)
}

alignHeights <- function(plot.list, model.index) {
  grobs <- toGrobs(plot.list)
  heights <- grobs[[model.index]]$heights
  grobs <- lapply(grobs, function(x){
    x$heights <- heights
    return(x)
  })
  return(grobs)
}

alignGenomicHeights <- function(grobs, model.index) {
  heights <- grobs[[model.index]]$plot$heights
  grobs <- lapply(grobs, function(x){
    x$plot$heights <- heights
    return(x)
  })
  return(grobs)
}

initGgplots <- function(data, columns, n.rows) {
  columns <- as.list(columns)
  data.list <- lapply(columns, function(x){
    data.vec <- unlist(data[x])
    data.vec[which(is.na(data.vec))] <- " "
    col.data <- as.data.frame(data.vec)
    names(col.data) <- x
    col.data$y <- nrow(col.data):1
    extend <- TRUE
    index <- which(columns == x)
    if(index == ncol(data)) extend <- FALSE
    plot <- addText(ggplot(), col.data, 'y', x, 2.5)
    plot <- addHeaderLine(plot, n.rows, extend, FALSE)
    plot <- removeMargin(plot, 0.5, 0, 0.5, 0)
    return(list(plot = ggplot_gtable(ggplot_build(plot)),
           index = index, updatable = TRUE))
  })
  return(data.list)
}

generateGenomicLinePlot <- function(line.data) {
  line.data <- orderByY(line.data)
  line.data <- addShape(line.data, 15)
  line.data <- editShape(line.data, 16, 1)
  line.data <- addLineColor(line.data, 'black')
  line.data <- addColor(line.data, genomicRiskColorPicker(line.data))
  plot <- ggplot()
  plot <- addPoints(plot, line.data, '   ', 'x', 'y','Risk (%)', TRUE)
  plot <- addLines(plot, line.data)
  plot <- addHeaderLine(plot, nrow(line.data), TRUE, TRUE)
  plot <- addGenomicRiskSummaryTheme(plot)
  plot <- removeMargin(plot, 0.5, 0, 0.5, -0.05)
  return(list(plot = ggplot_gtable(ggplot_build(plot)),
              index = 4:5, updatable = FALSE))
}

addLineData <- function(plot.list, line.plot) {
  index <- line.plot$index[1]
  n.plots <- length(plot.list)
  before <- index - 1
  before <- plot.list[1:before]
  after <- plot.list[index:n.plots]
  after <- c(list(line.plot), after)
  updated.after <- lapply(after, function(x){
    if(x$updatable) {
      x$index <- x$index + 2
      return(x)
    } else {
      return(x)
    }
  })
  return(c(before, updated.after))
}

drawGenomicRiskSummary <- function(data.list) {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, length(data.list)+1)))
  invisible(lapply(data.list, function(x) {
    vp = vplayout(1, x$index)
    pushViewport(vp)
    grid.draw(arrangeGrob(x$plot))
    popViewport()
  }))
}

formatDisorderData <- function(data) {
  colnames(data) <- tolower(colnames(data))
  data$disorder <- as.character(data$disorder)
  data <- toPercentages(data)
  data <- data[order(data$disorder), ]
  data <- addShape(data, disorderCohortShapePicker(data))
  data <- addColor(data, 'black')
  return(data)
}

generateDisorderLinePlot <- function(data, line.data) {
  n.rows <- nrow(data)
  plot <- ggplot()
  plot <- addLines(plot, line.data)
  plot <- addPoints(plot, data, '   ', 'avg', 'disorder', 'Clinical risk (%)', FALSE)
  plot <- addDisorderCohortRiskTheme(plot)
  plot <- addHeaderLine(plot, n.rows, TRUE, FALSE)
  plot <- addDisorderTitle(plot, n.rows)
  return(plot)
}

generateDisorderTextPlot <- function(data) {
  text.plot <- ggplot()
  text.plot <- addText(text.plot, data, 'disorder', 'n', 2.5)
  text.plot <- addHeaderLine(text.plot, nrow(data), FALSE, FALSE)
  return(text.plot)
}

drawDisorderCohortRiskPlot <- function(plot, text.plot) {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 100)))
  vp = vplayout(1, 1:92)
  pushViewport(vp)
  grid.draw(arrangeGrob(plot))
  popViewport()
  vp = vplayout(1, 93:100)
  pushViewport(vp)
  grid.draw(arrangeGrob(text.plot))
  popViewport()
}

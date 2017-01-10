xScaleLimits <- function(plot) {
  return(layer_scales(plot)$x$range$range)
}

yScaleLimits <- function(plot) {
  return(layer_scales(plot)$y$range$range)
}

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

toGrobs <- function(plot_list) {
  grobs <- lapply(plot_list, function(x){
    x <- ggplot_gtable(ggplot_build(x))
    x$layout$clip[x$layout$name == 'panel'] <- 'off'
    return(x)
  })
  return(grobs)
}

alignHeights <- function(plot_list, model_index) {
  grobs <- toGrobs(plot_list)
  heights <- grobs[[model_index]]$heights
  grobs <- lapply(grobs, function(x){
    x$heights <- heights
    return(x)
  })
  return(grobs)
}

alignGenomicHeights <- function(grobs, model_index) {
  heights <- grobs[[model_index]]$plot$heights
  grobs <- lapply(grobs, function(x){
    x$plot$heights <- heights
    return(x)
  })
  return(grobs)
}

initGgplots <- function(data, columns, n_rows) {
  columns <- as.list(columns)
  data_list <- lapply(columns, function(x){
    data_vec <- unlist(data[x])
    data_vec[which(is.na(data_vec))] <- " "
    col_data <- as.data.frame(data_vec)
    names(col_data) <- x
    col_data$y <- nrow(col_data):1
    extend <- TRUE
    index <- which(columns == x)
    if(index == ncol(data)) extend <- FALSE
    plot <- addText(ggplot(), col_data, 'y', x, 2.5)
    plot <- addHeaderLine(plot, n_rows, extend, FALSE)
    plot <- removeMargin(plot, 0.5, 0, 0.5, 0)
    return(list(plot = ggplot_gtable(ggplot_build(plot)),
           index = index, updatable = TRUE))
  })
  return(data_list)
}

generateGenomicLinePlot <- function(line_data) {
  line_data <- orderByY(line_data)
  line_data <- addShape(line_data, 15)
  line_data <- editShape(line_data, 16, 1)
  line_data <- addLineColor(line_data, 'black')
  line_data <- addColor(line_data, genomicRiskColorPicker(line_data))
  plot <- ggplot()
  plot <- addPoints(plot, line_data, '   ', 'x', 'y','Risk (%)', TRUE)
  plot <- addLines(plot, line_data)
  plot <- addHeaderLine(plot, nrow(line_data), TRUE, TRUE)
  plot <- addGenomicRiskSummaryTheme(plot)
  plot <- removeMargin(plot, 0.5, 0, 0.5, -0.05)
  return(list(plot = ggplot_gtable(ggplot_build(plot)),
              index = 4, updatable = FALSE))
}

addLineData <- function(plot_list, line_plot) {
  index <- line_plot$index
  n_plots <- length(plot_list)
  before <- index - 1
  before <- plot_list[1:before]
  after <- plot_list[index:n_plots]
  after <- c(list(line_plot), after)
  updated_after <- lapply(after, function(x){
    if(x$updatable) {
      x$index <- x$index + 1
      return(x)
    } else {
      return(x)
    }
  })
  return(c(before, updated_after))
}

drawGenomicRiskSummary <- function(data_list) {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, length(data_list))))
  invisible(lapply(data_list, function(x) {
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

generateDisorderLinePlot <- function(data, line_data) {
  n_rows <- nrow(data)
  plot <- ggplot()
  plot <- addLines(plot, line_data)
  plot <- addPoints(plot, data, '   ', 'avg', 'disorder', 'Clinical risk (%)', FALSE)
  plot <- addDisorderCohortRiskTheme(plot)
  plot <- addHeaderLine(plot, n_rows, TRUE, FALSE)
  plot <- addDisorderTitle(plot, n_rows)
  return(plot)
}

generateDisorderTextPlot <- function(data) {
  text_plot <- ggplot()
  text_plot <- addText(text_plot, data, 'disorder', 'n', 2.5)
  text_plot <- addHeaderLine(text_plot, nrow(data), FALSE, FALSE)
  return(text_plot)
}

drawDisorderCohortRiskPlot <- function(plot, text_plot) {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 100)))
  vp = vplayout(1, 1:92)
  pushViewport(vp)
  grid.draw(arrangeGrob(plot))
  popViewport()
  vp = vplayout(1, 93:100)
  pushViewport(vp)
  grid.draw(arrangeGrob(text_plot))
  popViewport()
}

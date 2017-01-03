xScaleLimits <- function(plot) {
  return(layer_scales(plot)$x$range$range)
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

alignHeights <- function(plot_list){
  grobs <- toGrobs(plot_list)
  heights <- grobs[[1]]$heights
  grobs <- lapply(grobs, function(x){
    x$heights <- heights
    return(x)
  })
  return(grobs)
}

initGgplots <- function(data, columns) {
  columns <- as.list(columns)
  data_list <- lapply(columns, function(x){
    col_data <- as.data.frame(unlist(data[x]))
    names(col_data) <- x
    col_data$y <- nrow(col_data):1
    index <- which(columns == x)
    col_data <- na.omit(col_data)
    return(list(plot = ggplot_gtable(ggplot_build(addText(ggplot(), col_data, 'y', x, 2.5))),
           index = index))
  })
  return(data_list)
}

drawGenomicRiskSummary <- function(data_list) {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 7)))
  invisible(lapply(data_list, function(x) {
    vp = vplayout(1, x$index)
    pushViewport(vp)
    grid.draw(arrangeGrob(x$plot))
    popViewport()
  }))
}

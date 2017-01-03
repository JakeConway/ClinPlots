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

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

toGrobs <- function(plot_list) {
  grobs <- lapply(plot_list, function(x){
    return(ggplot_gtable(ggplot_build(x)))
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

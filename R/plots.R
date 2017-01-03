DisorderCohortRiskPlot <- function(data){
  data$disorder <- as.character(data$disorder)
  data <- data[order(data$disorder), ]
  line_data <- flattenToX(data)
  plot <- ggplot()
  plot <- addLines(plot, line_data)
  plot <- addPoints(plot, data)

  text_plot <- ggplot()
  text_plot <- addText(text_plot, data, 'disorder', 'n', 2.5)
  text_plot <- addHeaderLine(text_plot, data)

  plot_list <- list(plot, text_plot)
  grobs <- alignHeights(plot_list)

  plot <- grobs[[1]]
  text_plot <- grobs[[2]]

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

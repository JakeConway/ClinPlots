disorderCohortRiskPlot <- function(data){
  data <- formatDisorderData(data)
  line_data <- disorderFlattenToX(data)
  plot <- generateDisorderLinePlot(data, line_data)
  text_plot <- generateDisorderTextPlot(data)
  plot_list <- list(plot, text_plot)
  grobs <- alignHeights(plot_list, 1)
  plot <- grobs[[1]]
  text_plot <- grobs[[2]]
  drawDisorderCohortRiskPlot(plot, text_plot)
}

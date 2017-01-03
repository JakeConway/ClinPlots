DisorderCohortRiskPlot <- function(data){
  data$disorder <- as.character(data$disorder)
  data <- data[order(data$disorder), ]
  line_data <- flattenToX(data)
  plot <- ggplot()
  plot <- addLines(plot, line_data)
  plot <- addPoints(plot, data)
  print(plot)

  text_plot <- ggplot()
  text_plot <- addCountText(text_plot, data)
  text_plot <- addHeaderLine(text_plot, data)
  print(text_plot)
}

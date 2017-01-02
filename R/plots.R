DisorderCohortRiskPlot <- function(data){
  data <- addYposition(data)
  line_data <- flattenToX(data)
  plot <- ggplot()
  plot <- addLines(plot, line_data)
  plot <- addPoints(plot, data)
  plot
}

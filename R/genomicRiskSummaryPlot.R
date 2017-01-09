genomicRiskSummaryPlot <- function(data, column) {
  data <- colsToCharacter(data)
  columns <- colnames(data)
  data <- adjustGeneLabels(data, columns)
  line_data <- genomicFlattenToX(data, column)
  line_plot <- generateGenomicLinePlot(line_data)
  data <- initGgplots(data, columns, nrow(data))
  data <- addLineData(data, line_plot)
  grobs <- alignGenomicHeights(data, 4)
  drawGenomicRiskSummary(grobs)
}

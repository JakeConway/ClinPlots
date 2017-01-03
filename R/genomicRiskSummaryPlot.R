genomicRiskSummaryPlot <- function(data) {
  data <- colsToCharacter(data)
  columns <- colnames(data)
  data <- adjustGeneLabels(data, columns)
  print(data$studies)
  data <- initGgplots(data, columns)
  drawGenomicRiskSummary(data)
}

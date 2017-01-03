genomicRiskSummaryPlot <- function(data) {
  data <- colsToCharacter(data)
  columns <- colnames(data)
  data <- adjustGeneLabels(data, columns)
  data <- initGgplots(data, columns)
  drawGenomicRiskSummary(data)
}

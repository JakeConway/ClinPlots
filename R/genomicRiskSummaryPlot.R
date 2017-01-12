#' Function to generate the genomic risk summary plot, as seen in the README.
#'
#' @description A function to generate the genomic risk summary plot, which is a series of text plots accompanied
#' by a line plot displaying the values from one of the columns, usually post-test probability.
#' @param data A data.frame of the variables to use in the plots. The column that is used for the line plot must
#'        have one additional value than the other columns. The names of the columns in the data.frame will be
#'        used for the names of the columns in the plot.
#' @param column The name or index of the column to be used for the line plot.
#' @export
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

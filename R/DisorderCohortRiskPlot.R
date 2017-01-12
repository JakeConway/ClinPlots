#' Function to generate the disorder risk cohort plot of post-test probabilities, as seen in the README.
#'
#' @description A function to generate the disorder risk cohort plot, which summarizes the the post-test
#'              probabilities of each disorder cohort. The disorder risk cohort plot is comprised of a line plot
#'              that displays the distribution of post-test probabilities for each disorder, along with the
#'              direction of change in probability. Each line in the plot is aligned with the name of the disorder,
#'              as well as the number of individuals in the disorder cohort.
#' @param data A data.frame that contains the columns disorder, n, min, max, avg, and increased. The disorder column
#'        contains the name of the disorder (character), the n column contains the number of individuals in the disorder
#'        cohort (integer), the min column contains the minimum post-test probability (numeric), the max columns
#'        contains the maximum post-test probability (numeric), the avg column contains the average post-test
#'        probability (numeric), and increased is a column that states if the post-test probability increased (logical).
#' @examples
#' clinical_risk <- read.csv(system.file("extdata", "clinical_risk.txt", package = "ClinPlots"), sep='\t')
#' disorderCohortRiskPlot(clinical_risk)
#' @export
disorderCohortRiskPlot <- function(data){
  data <- formatDisorderData(data)
  line_data <- disorderFlattenToX(data)
  line_data <- addLineColor(line_data, disorderCohortColorPicker(line_data))
  plot <- generateDisorderLinePlot(data, line_data)
  text_plot <- generateDisorderTextPlot(data)
  plot_list <- list(plot, text_plot)
  grobs <- alignHeights(plot_list, 1)
  plot <- grobs[[1]]
  text_plot <- grobs[[2]]
  drawDisorderCohortRiskPlot(plot, text_plot)
}

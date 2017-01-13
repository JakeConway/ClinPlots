#' Function to generate the disorder risk cohort plot of post-test probabilities, as seen in the README.
#'
#' @description A function to generate the disorder risk cohort plot, which summarizes the the post-test
#'              probabilities of each disorder cohort. The disorder risk cohort plot is comprised of a line plot
#'              that displays the distribution of post-test probabilities for each disorder, along with the
#'              direction of change in probability. Each line in the plot is aligned with the name of the disorder,
#'              as well as the number of individuals in the disorder cohort.
#' @param data A data.frame that contains the columns disorder, n, min, max, avg, and increased. Details about
#'             these parameters can be seen in the format section below.
#' @details The data for this parameter requires 6 columns: disorder, n, min, max, avg, and increased. \cr
#' \cr
#'   \bold{disorder:} the name of the disorder (character) \cr
#'   \bold{n:} the number of individuals in the disorder cohort (integer) \cr
#'   \bold{min:} the minimum post-test probability (numeric) \cr
#'   \bold{max:} the maximum post-test probability (numeric) \cr
#'   \bold{avg:} the average post-test probability (numeric) \cr
#'   \bold{increased:} whether or not the post-test probability increased (logical) \cr
#'
#' @examples
#' require(ggplot2); require(grid); require(gridExtra); require(scales);
#'
#' cr_df <- read.csv(system.file("extdata", "clinical_risk.txt", package = "ClinPlots"), sep='\t')
#' disorderCohortRiskPlot(cr_df)
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

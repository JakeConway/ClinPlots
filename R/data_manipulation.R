disorderFlattenToX <- function(data) {
  mins <- data$min
  maxs <- data$max
  x.positions <- c(mins, maxs)
  y.positions <- rep(data$disorder, 2)
  increased <- rep(data$increased, 2)
  return(data.frame(x = x.positions, y = y.positions, group = y.positions, increased = increased))
}

toPercentages <- function(data) {
  data$avg <- as.numeric(data$avg)/100
  data$min <- as.numeric(data$min)/100
  data$max <- as.numeric(data$max)/100
  return(data)
}

colsToCharacter <- function(data) {
  data[] <- lapply(data, as.character)
  return(data)
}

removeDuplicateGenes <- function(data, index) {
  data[which(duplicated(unlist(data[index]))), ][index] <- " "
  return(data)
}

adjustGeneLabels <- function(data, columns) {
  if ('gene' %in% tolower(columns)) {
    return(removeDuplicateGenes(data, which(tolower(columns) == 'gene')))
  } else {
    return(data)
  }
}

genomicFlattenToX <- function(data, column) {
  y.positions <- as.numeric(unname(unlist(data[column])))/100
  x.data <- length(y.positions):1
  group <- rep(1, nrow(data))
  studies <- data$studies

  return(data.frame(x = x.data, y = y.positions, group = group, studies = studies))
}

orderByY <- function(line.data) {
  line.data <- line.data[order(line.data$y), ]
  return(line.data)
}

addShape <- function(data, shape) {
  data$shape <- shape
  return(data)
}

editShape <- function(data, shape, index) {
  data$shape[index] <- shape
  return(data)
}

addLineColor <- function(data, colors) {
  data$line.color <- colors
  return(data)
}

addColor <-function(data, colors) {
  data$color <- colors
  return(data)
}

genomicRiskColorPicker <- function(data) {
  studies <- unique(data$studies)
  studies <- studies[!is.na(studies)]
  n.studies <- length(studies)
  colors <- grey.colors(n.studies, start = 0.15, end = 0.75, gamma = 2.2, alpha = NULL)
  colors <- rev(colors)
  colors <- c('blue', colors[data$studies[2:nrow(data)]])
  return(colors)
}

disorderCohortColorPicker <- function(data) {
  increased <- as.logical(data$increased)
  increased[which(increased)] <- '#F8766D'
  increased[which(increased == "FALSE")] <- '#00BFC4'
  return(increased)
}

disorderCohortShapePicker <- function(data) {
  increased <- data$increased
  increased[which(increased)] <- '\u25BA'
  increased[which(increased == "FALSE")] <- '\u25C4'
  return(increased)
}

chooseLeftRightMargins <- function(index, n.rows, plot.index) {
  left <- -0.5
  right <- -0.5
  if(index == 1) {
    left <- 0
  }
  if(index == n.rows) {
    left <- 0.5
    right <- 0
  }
  if(index == plot.index) {
    left <- 0
    right <- 0
  }
  return(c(left, right))
}

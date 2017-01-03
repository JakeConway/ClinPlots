flattenToX <- function(data) {
  mins <- data$min
  maxs <- data$max
  x_positions <- c(mins, maxs)
  y_positions <- rep(data$disorder, 2)
  return(data.frame(x = x_positions, y = y_positions))
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

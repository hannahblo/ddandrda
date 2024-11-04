# Function to compute the weights of the fc
get_weighted_representation <- function(x, y = rep(1, dim(x)[1])) {
  ## computes weighted representation of a data matrix x with duplicated rows,
  ##  returns unique(x) together with counts: how often appears the column,
  # mean_y: mean of y in the set of the duplicated columns
  xd <- data.frame(cbind(x, y))
  names(xd)[1] <- "v1"
  v1 <- "v1"
  p <- dim(x)[2]
  result <- as.matrix(plyr::ddply(xd, names(xd[(1:p)]), dplyr::summarise, count = length(v1), mean.y = mean(y), sum.y = sum(y)))
  x_weighted <- result[, (1:p)]
  colnames(x_weighted) <- colnames(x)
  return(list(x_weighted = x_weighted, y_weighted = result[, p + 3], mean_y = result[, p + 2], counts = result[, p + 1]))
}

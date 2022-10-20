compute_tukeys_outlyingness <- function(context, 
                                        weights = rep(1, nrow(context))) {
  n <- nrow(context)
  m <- ncol(context)
  a <- rep(0, m)
  for (k in (1:m)) {
    a[k] <- mean(context[, k] * weights)
  }
  l <- rep(0, n)

  for (k in (1:n)) {
    temp <- context[k, ]
    l[k] <- max(a[which(temp == 0)])
  }
  return(l)
}

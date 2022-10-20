compute_tukeys_outlyingness <- function(intent,
                                        context,
                                        row_weights = rep(1, nrow(context)),
                                        col_weights = rep(1, ncol(context))) {
  normed_row_weights <- row_weights / sum(row_weights)
  weighted_context <- ((normed_row_weights) %*% t(col_weights)) * context
  weighted_column_means <- t(weighted_context) %*% (rep(1, nrow(context)))
  if (is.vector(intent)) {
    return(max(weighted_column_means[which(intent == 0)]))
  }
}

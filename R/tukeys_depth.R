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
  if (is.matrix(intent)) {
    return(sapply(as.list(as.data.frame(t(intent))),
      compute_tukeys_outlyingness,
      context = context, simplify = TRUE
    ))
  }
}

compute_tukeys_depth <- function(intent,
                                 context,
                                 row_weights = rep(1, nrow(context)),
                                 col_weights = rep(1, ncol(context))) {
  return(1 - compute_tukeys_outlyingness(
    intent,
    context,
    row_weights,
    col_weights
  ))
}

compute_tukeys_median_order <- function(orders, startorder = orders[[1]] * 0) {
  # name eigtl. compute_tukeys_true_median_order
  # coputes that partial order in the space of ALL partial orders that has the
  # maximal tukeys depth wr.t. the given data cloud representet by th given
  # contetxt (given in the form of a list of posets, where every etry of the
  # list is an incidence relation apposited with its negation
  # (In terms of conceptual scaling we use here the complemented scaling

  m <- length(orders)
  q <- nrow(orders[[1]])
  w <- Reduce("+", orders)
  ans_old <- ans_new <- startorder

  while (TRUE) {
    ww <- max(w[which(ans_old == 0)])
    i <- which(ans_old == 0 & W == w)
    i <- sample(rep(i, 2), size = 1)
    ans_new <- ans_old
    ans_new[i] <- 1
    if (!is_extendable_to_partial_order(ans_new)) {
      return(cbind(ans_old[, (1:q)], 1 - ans_old[, (1:q)]))
    }
    m1 <- ans_new[, (1:q)]
    diag(m1) <- 1
    m1 <- relation_incidence(transitive_closure(as.relation(m1)))
    m2 <- ans_new[, -(1:q)]
    ans_old <- cbind(m1, m2)
  }
}

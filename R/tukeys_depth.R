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

compute_tukeys_median_order <- function(corders,
                                        startorder = corders[[1]] * 0) {
  # name eigtl. compute_tukeys_true_median_order
  # computes that partial order in the space of ALL partial orders that has the
  # maximal tukeys depth w.r.t. the given data cloud representet by th given
  # context (given in the form of a list of posets, where every entry of the
  # list is an incidence relation apposited with its negation
  # (In terms of conceptual scaling we use here the complemented scaling)

  q <- nrow(corders[[1]])
  sum_corder <- Reduce("+", corders)
  ans_old <- ans_new <- startorder

  while (TRUE) {
    max_worder <- max(sum_corder[which(ans_old == 0)])
    i <- which(ans_old == 0 & sum_corder == max_corder)
    i <- sample(rep(i, 2), size = 1)
    ans_new <- ans_old
    ans_new[i] <- 1
    if (!is_extendable_to_porder(ans_new)) {
      return(cbind(ans_old[, (1:q)], 1 - ans_old[, (1:q)]))
    }
    m_leq <- ans_new[, (1:q)]
    diag(m_leq) <- 1
    m_leq <- relations::relation_incidence(
      relations::transitive_closure(relations::as.relation(m_leq))
    )
    m_nleq <- ans_new[, -(1:q)]
    ans_old <- cbind(m_leq, m_nleq)
  }
}

is_extendable_to_porder <- function(corder) {
  q <- dim(corder)[1]
  m_leq <- relations::relation_incidence(relations::transitive_closure(
    relations::as.relation(corder[, (1:q)])
  ))
  diag(m_leq) <- 1
  m_nleq <- corder[, -(1:q)]
  if (any(m_leq == 1 & m_nleq == 1)) {
    return(FALSE)
  }
  if (!relations::relation_is_acyclic(relations::as.relation(m_leq))) {
    return(FALSE)
  }
  return(TRUE)
}

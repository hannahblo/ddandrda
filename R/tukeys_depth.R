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
    max_corder <- max(sum_corder[which(ans_old == 0)])
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







# properties of depth functions
is_quasiconcave <- function(depths, context) {
  m <- nrow(context)
  for (k in (1:m)) {
    i <- which(depths > depths[k])
    extent <- rep(0, m)
    extent[i] <- 1
    if (operator_closure_obj_input(extent, context)[k] == 1) {
      return(FALSE)
    }
  }
  return(TRUE)
}


is_strictly_quasiconcave <- function(depths, context) {
  m <- nrow(context)
  for (k in (1:m)) {
    i <- which(depths >= depths[k])
    i[k] <- 0
    extent <- rep(0, m)
    extent[i] <- 1
    if (operator_closure_obj_input(extent, context)[k] == 1) {
      return(FALSE)
    }
  }
  return(TRUE)
}

strictly_quasiconcave_phull <- function(depths, context) {
  # eigtl. name strictly_quasiconcave_pseudohull
  m <- nrow(context)
  ans <- depths
  for (k in (1:m)) {
    i <- which(depths >= depths[k])
    i[k] <- 0
    extent <- rep(0, m)
    extent[i] <- 1
    if (operator_closure_obj_input(extent, context)[k] == 1) {
      ans[k] <- ans[k] + 1 / 100 / m
    }
  }
  return(ans)
}


operator_closure_obj_input <- function(subset_object, context) {
  # Defines the closure operator for computing all extends (objects)

  # Input: subset_object (array): set of objects
  #         context (matrix): formal context which is used to calculate the extent

  # Output: subset (array): to smallest closure in the FCA based on
  #                         subset_object and context
  calculate_phi(calculate_psi(subset_object, context), context)
}


calculate_phi <- function(subset_attributes, context) {
  # Calculates for a subset of attributes the minimal extent based on the
  # given context

  # Input: subset_attributes (array): set of attributes
  #         context (matrix): formal context which is used to calculate the
  # extent

  # Output: subset (array): the smallest extent (set of objects) in the FCA
  #                         based on subset_attributes and the formalc context

  # Determines and subsets the attributes which are selected
  index_attribute <- which(subset_attributes == 1)
  selected_attributes <- as.matrix(context[, index_attribute])
  dim(selected_attributes) <- c(dim(context)[1], length(index_attribute))

  # Counting for each object how many selected attributes hold and choosing the
  # one where all attributes are true
  count_objects_attribute_hold <- rowSums(selected_attributes)
  index_obejct <- which(count_objects_attribute_hold == length(index_attribute))

  # returning a list which represents which objects correspond to the considered
  # attribute set
  extend <- rep(0, dim(context)[1])
  extend[index_obejct] <- 1

  return(extend)
}



calculate_psi <- function(subset_objects, context) {
  # Calculates for a subset of objects the minimal intent based on the given
  # context

  # Input: subset_objects (array): set of objects
  #         context (matrix): formal context which is used to calculate the
  # intent

  # Output: subset (array): to smallest intent (set of attributes) in the FCA
  #                         based on subset_objects and context

  # Determines and sub-setting the objects which are selected
  index_object <- which(subset_objects == 1)
  selected_objects <- as.matrix(context[index_object, ])
  dim(selected_objects) <- c(length(index_object), dim(context)[2])

  # Counting for each attribute how many selected objects are related and chose
  # the ones where all objects are related
  count_atts_object_related <- colSums(selected_objects)
  index_attribute <- which(count_atts_object_related ==
                             length(index_object))

  # returning an array which represents the attributes which correspond to the
  # considered object set
  intent <- rep(0, dim(context)[2])
  intent[index_attribute] <- 1
  return(intent)
}

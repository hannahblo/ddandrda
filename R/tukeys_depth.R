#' Plot a partial order
#' @description 'plot_order' plots a partial order by drawing the
#' Hasse diagramm of its Dedekind–MacNeille completion
#'
#' @param incidence The incidence relation of the partial order
#'
#' @return The drawing of the partial order in the form of the Hasse diagramm of
#' the Dedekind-MacNeille completion of the given partial order
#' @export
plot_order <- function(incidence) {
  fc <- fcaR::FormalContext$new(incidence[, (1:nrow(incidence))])
  fc$find_concepts()
  fc$concepts$plot()
}

context_to_list <- function(context, complemented = FALSE, colnames = NULL,
                            rownames = NULL) {
  m <- nrow(context)
  if (complemented) {
    q <- sqrt(ncol(context) / 2)
  } else {
    q <- sqrt(ncol(context))
  }
  q2 <- q
  if (complemented) {
    q2 <- 2 * q
  }
  list <- list()
  for (k in (1:m)) {
    temp <- context[k, ]
    dim(temp) <- c(q, q2)
    colnames(temp) <- colnames
    rownames(temp) <- rownames
    list[[k]] <- temp
  }

  return(list)
}






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

#' Computes Tukeys depth of an intent w.r.t. a given formal context
#'
#' @description 'compute_tukeys_depth' returns the depth value of an object
#' represented by its intent (given as a 0-1 vector) w.r.t. a data cloud
#' represented by a formal context.
#'
#' @param intent represents the envisaged object given by all its attributes
#' given as a 0-1 vector. It is also possible to compute depth values for
#' more objects. In this case the objects should be given as a matrix where
#' each row corresponds to one
#' object.
#'
#' @param context is a formal context whose objects represent the data cloud
#' w.r.t. which Tukeys depth is computed
#'
#' @param row_weights it is possible to give every object a weight (with the
#' interpretation that object with e.g. weight $2$ appear twice as often in the
#'  data set as objects with weight 1)
#'
#' @param col_weights it is possible to give every attribute a weight (with the
#'  interpretation that a weighted maximum (minimum) is calculated for
#'   Tukeys outlyingness (depth) such that attributes with higher weights get
#'    more important)
#'
#' @return returns the depth value(s) of the object(s) w.r.t. the data cloud.
#'
#' @examples
#'
#' context <- fcaR::planets
#' depth_values <- compute_tukeys_depth(context, context)
#' table(depth_values)
#' which(depth_values == max(depth_values))
#' which(depth_values == min(depth_values))
#'
#' @export
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

#' Tukeys true median order
#'
#' @description 'compute_tukeys_median_order' computes that partial order
#' in the space of ALL partial orders (that are supersets of startorder)
#' that has the maximal Tukeys depth w.r.t. the given
#' data cloud represented by the given context (given in the form of a list of
#' posets, where every entry of the list is an incidence relation apposited
#' with its negation. (In terms of conceptual scaling we use here the
#' complemented scaling)
#'
#' @param corders data set of partial orders (given in the form of a list of
#' posets, where every entry of the list is an incidence relation apposited
#' with its negation. (In terms of conceptual scaling we use here the
#' complemented scaling)
#'
#' @param startorder is a binary relation that can be used to restrict the space
#' of partial orders in which one searches the partial order(s) with the highest
#' Tukey depth. Concretely the search space is the space of all partial orders
#' in complemented conceptual scaling that are supersets of the relation
#' startorder. (startorder needs not to be a partial order))
#' @examples
#' all_4_orders <- compute_all_partial_orders(q = 4, complemented = TRUE,
#'  list = TRUE)
#' sampled_corders <- all_4_orders[c(rep(10, 20), (1:8), (21:30))]
#' tukeys_median <- compute_tukeys_median_order(sampled_corders)$median
#' plot_order(tukeys_median)
#' plot_order(sampled_corders[[1]])
#'
#' @export
compute_tukeys_median_order <- function(corders,
                                        startorder = corders[[1]] * 0) {
  # name eigtl. compute_tukeys_true_median_order
  #
  # input checks
  if (!is_extendable_to_porder(startorder)) {
    print("warning: invalid relation startorder (startorder is not extendable
           to a partial order, therefore the searchspce is empty.")
  }
  q <- nrow(corders[[1]])
  # columnsums represent the corresponding outlyingness values w.r.t. Tukeys
  # depth
  sum_corder <- Reduce("+", corders)
  # In the end ans_old will be the deepest partial order that will be return,
  # whereas
  # ans_new is the first relation that has larger depth but is not extendable
  # to a
  # partial order anymore
  ans_old <- ans_new <- startorder
  # set recursively attributes in ans_new to 1 to decrease Tukeys depth
  # start attributes that correspond to the largest Tukeys outlyingness values
  while (TRUE) {
    max_corder <- max(sum_corder[which(ans_old == 0)])
    i <- which(ans_old == 0 & sum_corder == max_corder)
    i <- sample(rep(i, 2), size = 1)
    ans_new <- ans_old
    ans_new[i] <- 1
    if (!is_extendable_to_porder(ans_new)) {
      median <- cbind(ans_old[, (1:q)], 1 - ans_old[, (1:q)])
      context <- list_to_context(corders, complemented = TRUE)
      depth <- compute_tukeys_depth(as.vector(median), context)

      return(list(median = median, depth = depth))
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


  m_leq <- corder[, (1:q)]
  diag(m_leq) <- 1
  m_leq <- compute_transitive_hull(m_leq)
  m_nleq <- corder[, -(1:q)]
  if (any(m_leq == 1 & m_nleq == 1)) {
    return(FALSE)
  }
  if (!relations::relation_is_acyclic(relations::as.relation(m_leq))) {
    return(FALSE)
  }
  return(TRUE)
}

#' Location-Separation type test statistic based on Tukeys depth
#' @description 'compute_loc_sep_statistic' computes a test statistic for
#' differences in location or separation of two distributions of order data
#' based on Tukeys depth: One maximizes the depth of a partial order w.r.t.
#' distribution 1 under the constraint that the depth w.r.t. distribution 2
#' is not below a certain threshold c defined by the smallest depth of the
#' lambda * 100 percent of the observed depth values. Then, by changing the
#' roles of distribution 1 and distribution 2 and taking a minimum, the
#' statistic is symmetrized For a small value of
#' lambda the test statistic is more or less similar to a generalization of
#' the T-based test. (This value lambda is that value for which both
#' optimizations lead to the same maximal value. A value of lambda smaller
#' than this value seems to be not a reasonable choice). For lambda=1 one gets
#' essentially a test that is similar to a generalization of the M-based test
#' defined in Li et al. (2004)
#'
#' @references Jun Li and Regina Y. Liu: New Nonparametric Tests of Multivariate Locations
#' and Scales Using Data Depth.
#' Statistical Science , Nov., 2004, Vol. 19, No. 4 (Nov., 2004), pp. 686-696
#'
#' @param corders1 complemented orders of distribution 1
#' @param corders2 complemented orders of distribution 2
#' @param lambda parameter for setting the threhold c, see above
#'
#'
#' @return  the value of the test statistic
#'
#' @examples
#' all_4_orders <- compute_all_partial_orders(4,complemented=TRUE,list=TRUE)
#' i <- sample((1:219),size=110)
#' orders1 <- all_4_orders[i]
#' orders2 <- all_4_orders[-i]
#' compute_loc_sep_statistic(orders1,orders2,0.8)
#'
#'
#'
#' @export
compute_loc_sep_statistic <- function(corders1, corders2, lambda) {
  n1 <- length(corders1)
  n2 <- length(corders2)
  context1 <- list_to_context(corders1, complemented = TRUE)
  context2 <- list_to_context(corders2, complemented = TRUE)
  depth1 <- compute_tukeys_depth(context1, context1)
  depth2 <- compute_tukeys_depth(context2, context2)


  startorder1 <- Reduce("pmin", corders1[which(depth1 >= stats::quantile(
    depth1, lambda))])

  startorder2 <- Reduce("pmin", corders2[which(depth2 >= stats::quantile(
    depth2, lambda))])
  depth1 <- compute_tukeys_median_order(corders1, startorder2)$depth
  depth2 <- compute_tukeys_median_order(corders2, startorder1)$depth
  return(min(depth1, depth2))
}

#' Location-Separation type test based on Tukeys depth
#' @description 'compute_loc_sep_statistic' computes a test that is based on
#' the function 'compute_loc_sep_statistic' that is senstivive for
#' differences in location or separation of two distributions of order data
#' based on Tukeys depth: One maximizes the depth of a partial order w.r.t.
#' distribution 1 under the constraint that the depth w.r.t. distribution 2
#' is not below a certain threshold c defined by the smallest depth of the
#' lambda * 100 percent of the observed depth values. For a very small value of
#' lambda, the test statistic is more or less similar to a generalization of
#' the M-based#' test, wheres for very high values of lambda one gets
#' essentially a test that is similar to a generalization of the T-based test
#'  defined in
#'
#' Jun Li and Regina Y. Liu: New Nonparametric Tests of Multivariate Locations
#' and Scales Using Data Depth.
#' Statistical Science , Nov., 2004, Vol. 19, No. 4 (Nov., 2004), pp. 686-696
#'
#'  The test is performed as a simple observation-randomization test
#'
#' @param corders1 complemented orders of distribution 1
#' @param corders2 complemented orders of distribution 2
#' @param lambda parameter for setting the threhold c, see above
#' @param nrep Number of repetions in the permutation sceme
#'
#' @return a list with the value of the test statistic, the value of the test
#'  under a permutation sceme that corresponds to the null hypothesis of
#'  identical distributions
#'
#' @export
compute_loc_sep_test <- function(corders1, corders2, lambda, nrep) {
  observed_statistic <- compute_loc_sep_statistic(corders1, corders2, lambda)
  h0_statistics <- rep(0, nrep)
  corders <- c(corders1, corders2)
  n <- length(corders)
  n1 <- length(corders1)
  for (k in (1:nrep)) {
    index <- sample((1:n), size = n1)
    h0_statistics[k] <- compute_loc_sep_statistic(
      corders[index], corders[-index], lambda
    )
  }

  return(list(
    observed_statistic = observed_statistic,
    h0_statistics = h0_statistics,
    p_value = mean(h0_statistics <= observed_statistic)
  ))
}





compute_tukeys_separation <- function(orders1, orders2,
                                      startorder = orders1[[1]] * 0) {
  # name urspr. tukeys_true:median_difference
  # coputes that partial order in the space of ALL partial orders
  # that has the maximal tukeys depth w.r.t. the given data cloud representet
  # by th given contetxt (given in the form of a list of posets, where every
  # etry of the list is an incidence relation apposited with its negation
  # (In terms of conceptual scaling we use here the complemented scaling

  q <- nrow(orders1[[1]])
  sum_1 <- Reduce("+", orders1)
  sum_2 <- Reduce("+", orders2)
  sum_1_2 <- pmax(sum_1, sum_2)




  ans_old <- ans_new <- startorder

  max_sum <- max(sum_1_2[which(ans_old == 0)])
  i <- which(ans_old == 0 & sum_1_2 == max_sum)
  i <- sample(rep(i, 2), size = 1)
  while (TRUE) {
    max_sum_old <- max_sum
    max_sum <- max(sum_1_2[which(ans_old == 0)])
    i <- which(ans_old == 0 & sum_1_2 == max_sum)
    i <- sample(rep(i, 2), size = 1)
    ans_new <- ans_old
    ans_new[i] <- 1
    if (!is_extendable_to_porder(ans_new)) {
      return(max_sum_old + 0.0001 * max_sum)
    }
    m1 <- ans_new[, (1:q)]
    diag(m1) <- 1
    m1 <- relations::relation_incidence(relations::transitive_closure(
      relations::as.relation(m1)
    ))
    m2 <- ans_new[, -(1:q)]
    ans_old <- cbind(m1, m2)
  }
}

compute_geodetic_median <- function(corders,
                                    proportion,
                                    auto = FALSE, fraction) {
  context <- list_to_context(corders, complemented = FALSE)
  td <- compute_tukeys_depth(context, context)
  if (auto) {
    tukeys_median <- as.vector(compute_tukeys_median_order(corders))$median
    ordered_depths <- sort(td, decreasing = TRUE)
    for (k in seq_along(corders)) {
      extent <- rep(0, ncol(context))
      extent[which(td >= ordered_depths[k])] <- 1
      intent <- calculate_psi(extent, context)
      if (all(intent <= tukeys_median)) {
        proportion <- k / length(corders) * fraction
        break
      }
    }
  }

  i <- which(td >= stats::quantile(td, 1 - proportion))
  extent <- rep(0, length(corders))
  extent[i] <- 1
  intent <- calculate_psi(extent, context)
  dim(intent) <- dim(corders[[1]])
  colnames(intent) <- colnames(corders[[1]])
  rownames(intent) <- rownames(corders[[1]])
  return(compute_tukeys_median_order(corders = corders, startorder = intent))
}


#' Converts list to context
#'
#' @description 'list_to_context' converts a list to a context
#'
#' @param list input list
#'
#' @param complemented should the context be returned with complemented
#'  attributes
#'
#' @export
list_to_context <- function(list, complemented) {
  # converts a list of orders given by incidence
  # relations as 0-1 matrices into a context of crosses
  m <- length(list)
  mat <- array(0, c(m, length(list[[1]])))

  for (k in (1:m)) {
    mat[k, ] <- as.vector(list[[k]])
  }
  if (complemented) {
    return(cbind(mat, 1 - mat))
  } else {
    return(mat)
  }
}


# properties of depth functions
is_quasiconcave <- function(depths, context) {
  m <- nrow(context)
  for (k in (1:m)) {
    i <- which(depths > depths[k])
    extent <- rep(0, m)
    extent[i] <- 1
    if ((length(i) > 0) &&
      (operator_closure_obj_input(extent, context)[k] == 1)) {
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

#' All partial orders on a set of q elements
#'
#' @description 'compute_all_partial_orders' returns the set of all partial
#'  orders on a set of $q$ elements
#'
#'
#' @param q is the number of elements of the basic space
#' @param names are the names of the q elements
#' @param complemented if TRUE, the orders are return in a complemented
#'  conceptual scaling
#' @param list if TRUE the orders are returned as a list. Otherwise a
#'  formal context is returned
#'
#' @return returns the set of all partial orders on a space of q elements
#'
#' @export
compute_all_partial_orders <- function(q, names = (1:q), complemented, list) {
  perms <- gtools::permutations(q, q)
  colnames(perms) <- names
  context <- ranking_scaling(perms,
    remove_full_columns = FALSE,
    complemented = FALSE
  )
  ans <- calculate_concept_lattice(context = context, compute_extents = FALSE)
  ans <- ans$intents[-nrow(ans$intents), ]
  colnames(ans) <- colnames(context)
  ans_list <- context_to_list(ans, complemented = FALSE)
  if (list) {
    if (complemented) {
      for (k in (1:nrow(ans))) {
        ans_list[[k]] <- cbind(ans_list[[k]], 1 - ans_list[[k]])
      }
    }
    return(ans_list)
  }
  if (!list) {
    if (complemented) {
      return(cbind(ans, 1 - ans))
    } else {
      return(ans)
    }
  }
}


# other depth functions

#' Simple localized depth function based on betweenness w.r.t.a prespecified
#'  mode
#'
#' @description 'compute_betweenness_depth' computes a simple depth function
#' by counting how many points are between the envisaged point and a
#' center (index_modus). This depth function is not quasiconcave but
#' it is star-shaped.
#' @param intent intent of the envisage object
#'
#' @param context the underlying context
#'
#' @param index_modus the index of the object within the context that represents
#' the center
#'
#'
#' @export
compute_betweenness_depth <- function(intent, context, index_modus) {
  if (is.vector(intent)) {
    m <- nrow(context)
    extent <- calculate_phi(pmin(context[index_modus, ], intent), context)
    ans <- sum(extent)
    return(m - ans)
  }
  if (is.matrix(intent)) {
    ans <- rep(0, nrow(intent))
    for (k in (1:nrow(intent))) {
      ans[k] <- compute_betweenness_depth(intent[k, ], context, index_modus)
    }
    return(ans)
  }
}


# other depth functions

#' Simple simplicial-type localized depth function based on betweenness w.r.t.
#' a prespecified mode
#'
#' @description 'compute_one_simplicial_depth' # computes another simple depth
#' function that adds for every point of the data clous to allenvisaged points
#' between this point and the center a one. This depth function is
# not quasiconcave but star-shaped
#'
#' @param intent intent of the envisage object
#'
#' @param context the underlying context
#'
#' @param index_modus the index of the object within the context that represents
#' the center
#'
#'
#' @export
compute_one_simplicial_depth <- function(intent, context, index_modus) {
  #
  if (is.matrix(intent)) {
    m <- nrow(context)
    ans <- 0
    for (k in (1:m)) {
      extent <- rep(0, m)
      extent[index_modus] <- 1
      extent[k] <- 1
      if (all(calculate_psi(extent, context) <= intent)) {
        ans <- ans + 1
      }
    }
    return(ans)
  }
  if (is.matrix(intent)) {
    ans <- rep(0, nrow(intent))
    for (k in (1:nrow(intent))) {
      ans[k] <- compute_one_simplicial_depth(intent[k, ], context, index_modus)
    }
    return(ans)
  }
}


## zu ueberarbeiten:
## exportieren?
ranking_scaling <- function(x,
                            remove_full_columns = FALSE,
                            complemented = FALSE) {
  m <- dim(x)[1]
  n <- dim(x)[2]
  names <- rep("", n^2)
  neg_names <- rep("", n^2)
  ans <- array(0, c(m, n^2))
  for (k in (1:m)) {
    temp <- array(0, c(n, n))
    for (l1 in (1:n)) {
      for (l2 in (1:n)) {
        temp[l1, l2] <- ((x[k, l1] <= x[k, l2])) * 1
      }
    }
    ans[k, ] <- as.vector(temp)
  }
  t <- 1
  for (l1 in (1:n)) {
    for (l2 in (1:n)) {
      names[t] <- paste(c(
        colnames(x)[l1], " <= ",
        colnames(x)[l2]
      ), collapse = "")

      neg_names[t] <- paste(c(" NOT(", colnames(ans)[t], ") "),
        collapse = ""
      )

      t <- t + 1
    }
  }
  colnames(ans) <- names

  if (complemented) {
    ans <- cbind(ans, 1 - ans)
    colnames(ans)[-(1:n^2)] <- neg_names
  }

  if (remove_full_columns) {
    i <- which(colSums(ans) == m)
    ans <- ans[, -i]
  }
  return(ans)
}

## evtl noch mit package fcaR zu harmnisieren...:

operator_closure_obj_input <- function(subset_object, context) {
  # Defines the closure operator for computing all extends (objects)

  # Input: subset_object (array): set of objects
  #         context (matrix): formal context which is used to calculate
  # the extent

  # Output: subset (array): to smallest closure in the FCA based on
  #                         subset_object and context
  calculate_phi(calculate_psi(subset_object, context), context)
}



operator_closure_attr_input <- function(subset_attribute, context) {
  # Defines the closure operator for computing all intents (attribute)

  # Input: subset_attribute (array): set of attributes
  #         context (matrix): formal context which is used to
  # calculate the intent

  # Output: subset (array): to smallest closure in the FCA based on
  #                         subset_attribute and context

  calculate_psi(calculate_phi(subset_attribute, context), context)
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



random_context <- function(nrow = 20,
                           ncol = 10,
                           prob = 0.5) {
  matrix(stats::runif(nrow * ncol) <= prob, nrow = nrow, ncol = ncol) * 1
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



calculate_concept_lattice <- function(context, compute_extents = TRUE) {
  # Calculates the formal concept lattice.
  # Therefore, all formal concept which are defined by the formal context are
  # calculated.

  # Input: context (matrix): represents the formal context (rows: objects,
  # columns: attributes)
  #         compute.extends (logical): If it is sufficient to only calculate
  # the intent

  # Output: (list) intents(array (0,1 values)): each row represents one intent,
  # (1 = attribute is contained)
  #                 extent(array with 0,1 values): each row represents one
  # extent, (1 = attribute is contained)
  #                 concepts(list): corresponding intent and extend are saved
  # together
  #                        saving not by index, but directly by their names


  result <- list()

  # Calculating all intents using the closure operator property
  result$intents <- compute_all_closure(operator_closure_attr_input, context)

  number_closure <- dim(result$intents)[1]
  number_objects <- dim(context)[1]
  result$concepts <- rep("", number_closure)

  if (compute_extents) {
    result$extents <- matrix(FALSE,
      ncol = number_objects,
      nrow = number_closure
    )
    for (k in (1:number_closure)) {
      # Calculate the extends based on the intents
      result$extents[k, ] <- calculate_phi(result$intents[k, ], context)
      result$concepts[k] <- paste("{",
        paste((rownames(context))[which(result$extents[k, ] == 1)],
          collapse = ","
        ),
        "}   {",
        paste((colnames(context))[which(result$intents[k, ] == 1)],
          collapse = ","
        ),
        "}",
        collapse = ""
      )
    }
  } else {
    for (k in (1:number_closure)) {
      result$concepts[k] <- paste("{",
        paste((colnames(context))[which(result$intents[k, ] == 1)],
          collapse = ","
        ),
        "}",
        collapse = ""
      )
    }
  }

  return(result)
}



compute_all_closure <- function(closure_operator, context,
                                number_attributes = NA,
                                already_computed_closures = 1000) {
  # Calculation of all sets of the complete lattice.
  # based on: Granter (2013), Diskrete Mathematik: Geordnete Mengen,
  # Springer Spekturm, p.68

  # Input: closure_operator (func): set-operator which calculates the
  # smallest closure
  #               based on a subset
  #         context (matrix): formal context which precises the closure_operator
  #         number_attributes (NA or integer): determines the number of
  # attributes
  #         already_computed_closures (int): states the frequency how often the
  #               information 'how many closures are already computed'#
  # is printed

  # Output: (array, elements in 0,1): each row states one computed closure
  #                                   (1 = element in closure)



  if (is.na(number_attributes)) {
    number_attributes <- dim(context)[2]
  }

  # Calculating the first lattice set based on the empty set and the used
  # context
  # Ganter, p68 Algorithm: First Closure
  old_closure <- closure_operator(rep(0, number_attributes), context)
  all_closure <- list()
  all_closure[[1]] <- old_closure

  # In this part all further lattice sets are computed
  t <- 2
  not_all_closures_computed <- TRUE
  while (not_all_closures_computed) {
    attributs_selected <- which(old_closure == 1)

    # Determining all the attributes which could be added, hence which are not
    # selected yet
    if (length(attributs_selected) == 0) {
      index <- (1:number_attributes)
    } else {
      index <- (1:number_attributes)[-attributs_selected]
    }

    # Ganter, p.86 Algorithm: Next Closure
    # Going from the larges to the lowest not yet added attribute until the new
    # calculated closure is larger (in sense of the 'lektisch' order, see Ganter
    # p. 26)
    for (element in sort(index, decreasing = TRUE)) {
      # Adding the new element with 'adds_element()' and computing the closure
      new_closure <- closure_operator(
        adds_element(old_closure, element),
        context
      )
      # Test if the new closer is larger then the older closure. If yes, go on.
      if (compare_closures_lower_i(old_closure, new_closure, element)) {
        break # break of the for-loop (not while)
      }
    }

    # Saving the new closure and now it takes the place of the old closure.
    old_closure <- all_closure[[t]] <- new_closure

    # Testing if all closures are computed, the last one has all attributes
    # selected
    if (all(new_closure == 1)) {
      not_all_closures_computed <- FALSE
    }
    # Test if print-information on how many closures are already computed
    if (t %% already_computed_closures == 0) {
      cat(t, "many closures were computed.\n")
    }
    # assignment to the new saving space
    t <- t + 1
  }
  # Convert from list to array and return the object
  return(t(array(unlist(all_closure), dim = c(number_attributes, t - 1))))
}


adds_element <- function(old_subset, element) {
  # Adds a further element to old_subset and deletes all larger elements
  # based on: Granter (2013), Diskrete Mathematik: Geordnete Mengen,
  # Springer Spektur, p.85

  # input: old_subset (array with 0,1 elements): subset to which the element
  # should be added
  #                                             (1 represents element in subset)
  #         element (integer): element (position) which is added

  # output: subset (array with 0,1 elements): subset with added element
  #                                          (1 represents element in subset)

  # if the element is the first, the subset only consists of this element
  if (element == 1) {
    subset <- rep(0, length(old_subset))
    subset[element] <- 1
  } else {
    index_lower_element_index <- rep(0, length(old_subset))
    index_lower_element_index[(1:(element - 1))] <- 1
    # pmin: A and temp are compared by element by element and
    # the minimum is selected
    subset <- pmin(old_subset, index_lower_element_index)
    subset[element] <- 1
  }
  return(subset)
}



compare_closures_lower_i <- function(old_closure, new_closure, element) {
  # Tests if the old_closure is smaller than  the new_closure within
  # the meaning of
  # 'lektisch' order
  # based on: Granter (2013), Diskrete Mathematik: Geordnete Mengen,
  # Springer Spekturm, p.26 + 84

  # Input: old_closure (array with 0,1 elements): closure
  # (subset, 1= element within closure)
  #         new_closure (array with 0,1 elements): closure
  # (subset, 1= element within closure)
  #         element (integer): element which is used for comparing

  # Output (logical): returns true if old_closure < new_closure
  if (element == 1) {
    return(new_closure[element] == 1 & old_closure[element] == 0)
  } else {
    temp <- rep(0, length(old_closure))
    temp[(1:(element - 1))] <- 1
    return(new_closure[element] == 1 & old_closure[element] == 0 &
      all(pmin(old_closure, temp) == pmin(new_closure, temp)))
  }
}

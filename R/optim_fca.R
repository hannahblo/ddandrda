#' Context with all partial orders as intents
#'
#' @description 'compute_context_all_porders' computes a formal context whose
#' intents are all partial orders on a set of q elements
#' (PLUS the ALL-relation!): Since every partial order is an intersection
#' of a set of linear orders (more concretely the set of all linear
#' extensions), one can can compute the set of all partial orders as the
#' intents of a formal context where every object is a linear order L and
#'  every attribute is a pair (a,b) and L I (a,b) iff (a,b) in L.
#' Note that the empty intersection of objects gives the all relation,
#' and the all relation is not a partial order
#'
#' #'
#' @param q is the number of elements of the basic space
#' @param names are the names of the q elements
#'
#' @examples
#' q <- 5
#' steps <- 10000
#' context_for_q_porders <- compute_context_all_porders(q=q)
#' corders <- compute_all_partial_orders(q=q,complemented=TRUE,list=TRUE)
#' context <- list_to_context(corders,complemented=TRUE)
#' index <- sample((1:nrow(context)),size=3)
#' sampled_context <- context[index,]
#' g <- function(intent,context){0.00001+compute_tukeys_depth(c(intent,1-intent)
#' , sampled_context)}
#' tukeys_true_median <- compute_tukeys_median_order(corders[index])
#' tukeys_median_based_on_mc_heuristic <- sample_concept(
#' context_for_q_porders,steps=steps,g)
#' par(mfrow=c(2,1))
#' plot_order(tukeys_true_median$median)
#' dim(tukeys_median_based_on_mc_heuristic) <- c(q,q)
#' plot_order(tukeys_median_based_on_mc_heuristic)
#'
#' @export
compute_context_all_porders <- function(q, names = (1:q)) {
  perms <- gtools::permutations(q, q)
  colnames(perms) <- names
  context <- ranking_scaling(perms,
    remove_full_columns = FALSE,
    complemented = FALSE
  )

  return(context)
}

#' Return 1
#'
#' @description A function that always returns 1
#'
#' @export
return_eins <- function() {
  1
}



#'  Sampling a formal concept
#'
#' 'sample_concept' samples a formal concept of a given formal context
#' according to a probability function f that assigns to every intent a
#' corrseponding sampling probability. The algorithm implemented is the one
#' given based in:
#'
#' Mario Boley, Thomas Gärtner and Henrik Grosskreutz:
#' Formal Concept Sampling for Counting and Threshold-Free Local Pattern Mining.
#' Proceedings of the 2010 SIAM International Conference on Data Mining (SDM)
#' pp.177 - 188
#'
#' @param context is the underlying context
#' @param steps is the number of steps within the Markov chain Monte Carlo
#'  algorithm
#' @param f is the depth function which assigns to every intent a sampling
#'  probability. This functions has to have two arguments: the intent and the
#'  underlying context
#'
#' @examples q <- 5
#' steps <- 10000
#' context_for_q_porders <- compute_context_all_porders(q = q)
#' corders <- compute_all_partial_orders(q = q, complemented = TRUE, list = TRUE)
#' context <- list_to_context(corders, complemented = TRUE)
#' index <- sample((1:nrow(context)), size = 3)
#' sampled_context <- context[index, ]
#' g <- function(intent, context) {
#'   0.00001 + compute_tukeys_depth(c(intent, 1 - intent), sampled_context)
#' }
#' tukeys_true_median <- compute_tukeys_median_order(corders[index])
#' tukeys_median_based_on_mc_heuristic <- sample_concept(
#'   context_for_q_porders,
#'   steps = steps, g
#' )
#' par(mfrow = c(2, 1))
#' plot_order(tukeys_true_median$median)
#' dim(tukeys_median_based_on_mc_heuristic) <- c(q, q)
#' plot_order(tukeys_median_based_on_mc_heuristic)
#'
#' @export
sample_concept <- function(context, steps = 10000, f) {
  maximum <- -Inf
  m <- dim(context)[1]
  n <- dim(context)[2]
  top_attr <- operator_closure_attr_input(rep(0, n), context)
  bottom_obj <- operator_closure_obj_input(rep(0, m), context)

  if (stats::runif(1) > 0.5) {
    a <- rep(1, m)
    b <- top_attr
  } else {
    b <- rep(1, n)
    a <- bottom_obj
  }

  for (i in (1:steps)) {
    if (stats::runif(1) > 0.5) {
      l <- sample((1:n), size = 1)
      bb <- b
      bb[l] <- 1
      aa <- calculate_phi(bb, context)
      bb <- calculate_psi(aa, context)
      alpha <- g_ext(aa, a, context) * n / g_int(b, bb, context) / m
      if (is.na(alpha)) {
        alpha <- 0
      }
    } else {
      o <- sample((1:m), size = 1)
      aa <- a
      aa[o] <- 1
      bb <- calculate_psi(aa, context)
      aa <- calculate_phi(bb, context)
      alpha <- g_int(bb, b, context) * m / g_ext(a, aa, context) / n
      if (is.na(alpha)) {
        alpha <- 0
      }
    }
    #
    if (f(b, context) > maximum) {
      b_maximum <- b
    }
    maximum <- max(maximum, f(b, context))



    if (stats::runif(1) < alpha * f(bb, context) / f(b, context)) {
      a <- aa
      b <- bb
    }
    # Diesen Teil brauchen wir nichtif (runif(1) >= Q) {
    # b <- b_maximum
    # a <- calculate_phi(b, context)
    # }
  }



  return(b_maximum)
}
# T=concept.gen(bg,15000,f)

g_int <- function(b, bb, context) {
  if (all(b == bb)) {
    return(0)
  }
  ans <- 0
  i <- which(bb == 1 & b == 0)
  for (k in i) {
    temp <- b
    temp[k] <- 1
    if (all(operator_closure_attr_input(temp, context) == bb)) {
      ans <- ans + 1
    }
  }
  return(ans)
}



g_ext <- function(a, aa, context) {
  g_int(a, aa, t(context))
}


# T=concept.gen(bg,10000,f)



#  computes a formal context that rpresents a data matrix as a matrix of total orders
#'
#' given a matrix x with n columns where every row is one data point,
#' computes for every data point x_i the incidence_matrix
#' x_i^k <= x_i^l , k,l in {1, .. n}
#' @param x A matrix. Every row represents one total order
#' @param remove_full_columns (logical): If True, then resuting columns that consist only
#' of ones, are removed
#' @param complemented (logical)  If True, then also the attributes 
#' NOT (x_i^k <= x_i^l , k,l in {1, .. n}) are included
#' @return matrix where row i represents the incidence matrixthat corresponds to the i-th 
#' row of the data matrix X
compute_poset_scaling <- function(x,
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




#' Convert a context into a list
#'
#' @description 'convert_fc_to_list_poset' converts a context that represents
#' a partial order (or an arbitrary homogeneous relation, possibly complemented)
#' into a list
#'
#' @param context The formal context that should be converted
#' @param complemented should be true if a complemented context is given (then,
#' a list of complemented incidence matrices is returned)
#' @param col_names Names of the columns of the context
#' @param row_names Names of the rows of the context
#'
#' @return list of posets corresponding to the objects of the context
convert_fc_to_list_poset <- function(context, complemented = FALSE,
                                    col_names = NULL, row_names = NULL) {
  n_rows <- nrow(context)
  if (complemented) {
    n_items <- sqrt(ncol(context) / 2)
  } else {
    n_items <- sqrt(ncol(context))
  }
  n_cols <- n_items
  if (complemented) {
    n_cols <- 2 * n_items
  }
  list <- list()
  for (k in (1:n_rows)) {
    temp <- context[k, ]
    dim(temp) <- c(n_items, n_cols)
    colnames(temp) <- col_names
    rownames(temp) <- row_names
    list[[k]] <- temp
  }

  return(list)
}


#' Context with all partial orders as objsexts
#'
#' @description 'compute_context_all_poset' computes a formal context whose
#' objects are all partial orders on a set of n_items elements
#' (PLUS the ALL-relation!): Since every partial order is an intersection
#' of a set of linear orders (more concretely the set of all linear
#' extensions), one can can compute the set of all partial orders as the
#' intents of a formal context where every object is a linear order L and
#' every attribute is a pair (a,b) and L I (a,b) iff (a,b) in L.
#' Note that the empty intersection of objects gives the all relation,
#' and the all relation is not a partial order
#'
#'
#' @param n_items is the number of elements of the basic space
#' @param names are the names of the n_items elements
#'
#' @examples
#' n_items <- 5
#' steps <- 10000
#' context_for_n_items_p_orders <- compute_context_all_poset(
#'   n_items =
#'     n_items
#' )
#' c_orders <- compute_all_poset(
#'   n_items = n_items, complemented = TRUE,
#'   list = TRUE
#' )
#'
#' @export
compute_context_all_poset <- function(n_items, names = (1:n_items)) {
  perms <- gtools::permutations(n_items, n_items)
  colnames(perms) <- names
  context <- compute_poset_scaling(perms,
                                     remove_full_columns = FALSE,
                                     complemented = FALSE
  )

  return(context)
}




#' Test if new observation lies in conclusion based on nominal scaling
#'
#' @description Based on nominal scaling this function tests if a further
#' object lies in the conclusion of a premise
#'
#' @param subset (vector of (0,1)): 1 represents that the point is within the
#' subset
#' @param obj_porder_obs (nominal): observation to test if lies in conclusion
#' @param info_list (containing data_values): nominal attribute of each
#' observation  (same length as premise)
#'
#' @return logical value. TRUE if obj_nominal_obs lies in the conclusion, else
#' FALSE is returened
#'
#' @export
test_poset_in_concl <- function(subset, obj_porder_obs,
                                 info_list = NULL) {
  number_item <- dim(subset[[1]])[[1]]
  subset_intersect <- 1 * Reduce("&", subset,
                                 init = matrix(1,
                                               nrow = number_item,
                                               ncol = number_item
                                 )
  )
  subset_union <- 1 * Reduce("|", subset,
                             init = matrix(0,
                                           nrow = number_item,
                                           ncol = number_item
                             )
  )

  number_obj_porder <- length(obj_porder_obs)

  in_conclusion <- rep(FALSE, length(obj_porder_obs))

  for (index_obj_porder in seq_along(1:number_obj_porder)) {
    if (all(subset_intersect <= obj_porder_obs[[index_obj_porder]]) &&
        all(obj_porder_obs[[index_obj_porder]] <= subset_union)) {
      in_conclusion[index_obj_porder] <- TRUE
    }
  }
  return(in_conclusion)
}



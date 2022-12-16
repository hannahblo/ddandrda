#' Compute the relation product
#'
#' @description Computing the reltion product of two squared, same sized
#' matrices.
#'
#' @param x Represents a relation matrix. Note that has to be a squared matrix.
#' @param y Represents a relation matrix. Note that has to be a squared matrix.
#'
#' @return 0-1-Matrix: Represents a graph after two steps which are defined by
#'         x and y
compute_relation_product <- function(x, y) {

  # Input check
  if (!is.matrix(x) || !is.matrix(y)) {
    stop("Input must be matrix")
  }
  if (dim(y)[1] != dim(x)[1]) {
    stop("Dimension missmatch!")
  }

  number_row_x <- dim(x)[1]
  number_col_x <- dim(x)[2]
  number_col_y <- dim(y)[2]

  product_result <- array(0, c(number_row_x, number_col_y))

  for (k in (1:number_col_x)) {
    # X[.,k] edge between . to k
    # Y[k,.] edge from k to .

    # computes if there is a path which uses knot k as intermediate step
    product_result[which(x[, k] == 1), which(y[k, ] == 1)] <- 1
  }

  return(product_result)
}



#' Compute the transitive hull of a partial order
#'
#' @description
#' 'compute_transitive_hull' returns a 0-1-matrix which represents the
#' order-pairs given by the transitivity property of a partial order
#'
#' @param relation_mat Rrepresents a relation matrix. Note that
#' has to be a squared matrix.
#'
#' @return The transitive hull of the relation matrix relation_mat
#'
#' @examples
#' relation_mat_input <- matrix(0, nrow = 5, ncol = 5)
#' relation_mat_input[1, 3] <- 1
#' relation_mat_input[2, 1] <- 1
#' relation_mat_input[4, 3] <- 1
#' compute_transitive_hull(relation_mat_input)
#'
#' @export
compute_transitive_hull <- function(relation_mat) {

  # Input check
  if (!is.matrix(relation_mat)) {
    stop("relation_mat must be matrix.")
  }
  if (!all(relation_mat %in% c(0, 1))) {
    stop("relation_mat must be matrix containing only 0's or 1's.")
  }

  if (nrow(relation_mat) != ncol(relation_mat)) {
    stop("relation_mat must be squared matrix.")
  }

  # @relation_mat (sqared matrix): represents a relation matrix
  # Return (squared matrix): the transitive hull of the relation matrix
  # relation_mat

  number_obj <- dim(relation_mat)[1]

  old_matrix <- array(0, c(number_obj, number_obj))
  next_matrix <- relation_mat
  transitive_hull <- relation_mat

  # each while-loop computes the next step of the path given by relation_mat
  while (any(old_matrix != next_matrix)) {
    old_matrix <- next_matrix
    # this computes the next step. In other words in the first loop it computes
    # all edges which can be obtained by the combination of twp edges in
    # relation_mat
    next_matrix <- compute_relation_product(old_matrix, relation_mat)

    # contains all paths which can be done in maximal number of loop iteration
    # of steps
    transitive_hull <- transitive_hull + next_matrix
  }

  # more than one possible path --> than the number of paths was computed by the
  # while-loop
  # --> set to 1
  index_non_zero <- which(transitive_hull > 0)
  transitive_hull[index_non_zero] <- 1

  return(transitive_hull)
}





#' Test if matrix represents a partial order
#'
#' @description Checks if the matrix represents a partial order (thus it
#' is reflexiv, transitive and anti-symmetric)
#'
#' @param po_candidate 0-1- Matrix. If entry (i,j)is zero, then i is  smaller or
#' equal to j. If entry is zero, then this does not hold.
#' @param omit_reflexivity (logical) if reflexivity should be omited, so
#' diagonal entry do not matter
#'
#' @return logical TRUE if po_candidate represents a partial order
#'
#' @examples
#' mat_1 <- matrix(0, nrow = 5, ncol = 5)
#' mat_1[1, 3] <- 1
#' mat_1[2, 1] <- 1
#' mat_1[4, 3] <- 1
#' mat_1[2, 3] <- 1
#' test_if_porder(mat_1)
#' test_if_porder(mat_1, omit_reflexivity = TRUE)
#'
#' @export
test_if_porder <- function(po_candidate, omit_reflexivity = FALSE) {

  # Input check
  check_input_tipo(po_candidate, omit_reflexivity)

  # Step 0: Check the simple cases, depending on omit_relfexivity
  if (all(po_candidate == diag(nrow(po_candidate)))) {
    return(TRUE)
  }

  if (omit_reflexivity && all(po_candidate ==
      matrix(0, nrow = nrow(po_candidate), ncol = ncol(po_candidate)))) {
    return(TRUE)
  }
  if (!omit_reflexivity && any(diag(po_candidate) != 1)) {
    return(FALSE)
  }

  # Step 1: check if cycle exists
  diag(po_candidate) <- 0
  if (any(is.na(Rfast::topological_sort(po_candidate)))) {
    return(FALSE)
  }

  # Step 2 Check if the transitivity holds (i.e. a<b and b<c but also a and c
  # are not comparable is not allowed)
  # Here, we use that each pair is
  # thus if the transitive hull is unequal to the relation itself, then it is
  # not a transitive
  # We use the function in fca_ufg_partial_order.r

  return(all(compute_transitive_hull(po_candidate) == po_candidate))
  # this tolerance is sufficient since only 0 or 1 exists
}

compute_relation_product <- function(x, y) {

  # @X (matrix): Represents a graph with edges (weight one) and knots
  # @Y (matrix): Represents a graph with edges (weight one)and knots
  # Return (matrix): Represents a graph after two steps which are defined by X and Y

  # Input check
  if (dim(y)[1] != dim(x)[1]) {
    print("dimension missmatch!")
    stop
  }

  number_row_X <- dim(x)[1]
  number_col_X <- dim(x)[2]
  number_col_Y <- dim(y)[2]

  product_result <- array(0, c(number_row_X, number_col_Y))

  for (k in (1:number_col_X)) {
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
#' 'compute_transitive_hull' returns a 0-1-matrix which represents the order-pairs
#' given by the transitivity property of a partial order
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

  # @relation_mat (sqared matrix): represents a relation matrix
  # Return (squared matrix): the transitive hull of the relation matrix relation_mat

  number_obj <- dim(relation_mat)[1]

  old_matrix <- array(0, c(number_obj, number_obj))
  next_matrix <- relation_mat
  transitive_hull <- relation_mat

  # each while-loop computes the next step of the path given by relation_mat
  while (any(old_matrix != next_matrix)) {
    old_matrix <- next_matrix
    # this computes the next step. In other words in the first loop it computes
    # all edges which can be obtained by the combination of twp edges in relation_mat
    next_matrix <- compute_relation_product(old_matrix, relation_mat)

    # contains all paths which can be done in maximal number of loop iteration of steps
    transitive_hull <- transitive_hull + next_matrix
  }

  # more than one possible path --> than the number of paths was computed by the while-loop
  # --> set to 1
  index_non_zero <- which(transitive_hull > 0)
  transitive_hull[index_non_zero] <- 1

  return(transitive_hull)
}

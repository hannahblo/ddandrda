# Input check to functions of file base_partial_order.R-------------------------


#' Input check function to the function test_if_porder
#' This is just a help function
#'
#' @param po_candidate 0-1- Matrix. If entry (i,j)is zero, then i is  smaller
#' or equal to j. If entry is zero, then this does not hold.
#' @param omit_reflexivity (logical) if reflexivity should be omited, so
#' diagonal entry do not matter
check_input_tipo <- function(po_candidate, omit_reflexivity) {
  if (!is.logical(omit_reflexivity)) {
    stop("omit_reflexivity must be logical.")
  }

  if (!is.matrix(po_candidate) || !all(po_candidate %in% c(0, 1))) {
    stop("po_candidate must be matrix containing only 0's or 1's.")
  }

  if (nrow(po_candidate) != ncol(po_candidate)) {
    stop("po_candidate must be squared matrix.")
  }
}



# Input check to functions of file definition_formal_context.R------------------




#' Help Function of compute_conceptual_scaling()
#' Test if all input variables are valid
#'
#' @param input_factor (NULL or vector of factor values)
#' @param input_ordinal_numeric (NULL or vector of ordinal/numeric values)
#' @param input_spatial (NULL or list of two dimensional vectors)
#' @param input_porder (NULL or list of square same sized matrices)
#' @param scaling_methods (NULL or vector containing "porder_edge" or/and
#' "ordinal")
#'
#' @return logiacl value if input check is passed
check_input_ccs_1 <- function(input_factor = NULL,
                              input_ordinal_numeric = NULL,
                              input_spatial = NULL,
                              input_porder = NULL,
                              scaling_methods = NULL) {
  if (all(
    is.null(input_factor),
    is.null(input_ordinal_numeric),
    is.null(input_spatial),
    is.null(input_porder)
  )) {
    stop("Data input is empty.")
  }

  # bool input_vaild = FALSE;
  # input_valid &= is.null(input_factor);

  # f(input_valid){
  #
  # }

  length_values <- unique(c(
    length(input_factor),
    length(input_ordinal_numeric),
    dim(input_spatial)[1],
    length(input_porder)
  ))
  number_obj <- setdiff(length_values, c(0))
  if (length(number_obj) > 1) {
    stop("Data inputs must have same length or be NULL.")
  }

}


#' Help Function of compute_conceptual_scaling()
#' Test if all input variables are valid
#'
#' @param input_factor (NULL or vector of factor values)
#' @param input_ordinal_numeric (NULL or vector of ordinal/numeric values)
#' @param input_spatial (NULL or list of two dimensional vectors)
#' @param input_porder (NULL or list of square same sized matrices)
#' @param scaling_methods (NULL or vector containing "porder_edge" or/and
#' "ordinal")
#'
#' @return logiacl value if input check is passed
check_input_ccs_2 <- function(input_factor = NULL,
                              input_ordinal_numeric = NULL,
                              input_spatial = NULL,
                              input_porder = NULL,
                              scaling_methods = NULL) {
  if (!is.null(input_porder) &&
    (!is.list(input_porder) && !is.matrix(input_porder[[1]]))) {
    stop("input_porder must either be NULL or a list of matrices.")
  }

  if (!is.null(input_factor) &&
    (!(class(input_factor)[1] == "factor"))) {
    stop("input_factor must either be NULL or a vector of factors.")
  }

  # TODO
  # check scaling methods used

  # TODO
  # when input rows have names --> check if the order is everywhere the same
}




#' Help Function of compute_conceptual_scaling()
#' Test if all input variables are valid
#'
#' @param input_factor (NULL or vector of factor values)
#' @param input_ordinal_numeric (NULL or vector of ordinal/numeric values)
#' @param input_spatial (NULL or list of two dimensional vectors)
#' @param input_porder (NULL or list of square same sized matrices)
#' @param scaling_methods (NULL or vector containing "porder_edge" or/and
#' "ordinal")
#'
#' @return logiacl value if input check is passed
check_input_ccs_3 <- function(input_factor = NULL,
                              input_ordinal_numeric = NULL,
                              input_spatial = NULL,
                              input_porder = NULL,
                              scaling_methods = NULL) {
  if (!is.null(input_ordinal_numeric) &&
    (!(class(input_ordinal_numeric)[1] == "ordered" ||
      class(input_ordinal_numeric)[1] == "numeric" ||
      class(input_ordinal_numeric)[1] == "integer"))) {
    stop("input_ordinal_numeric must either be null or of class ordered,
           numeric or integer.")
  }

  if (!is.null(input_spatial)) {
    if (any(duplicated.matrix(input_spatial))) {
      stop("At least two points are duplicates.")
    }

    # Testing if points in point_matrix are collinear
    slope <- (input_spatial[1,2] - input_spatial[2,2]) / (input_spatial[1,1] - input_spatial[2,1])
    further_slopes_compare <- apply(input_spatial, 1, FUN = function(x) {
      identical(unname((input_spatial[1,2] - x[2]) / (input_spatial[1,1] - x[1])), slope) })
    if (all(further_slopes_compare[-1])) {
      stop("Points are collinear.")
    }
  }
}


# Input check to functions of file ufg_depth.R----------------------------------

#' Help Function of compute_conceptual_scaling()
#' Test if all input variables are valid
#'
#' @param list_porder (list of square matrix) list of matrices representing
#' partial orders of the same item dimension,
#'
#' @return logiacl value if input check is passed
check_input_porder_list <- function(list_porder) {
  if (!is.list(list_porder) && !is.matrix(list_porder)) {
    stop("list_porder (or ufg_candidate called) should be a list")
  }

  if (!is.matrix(list_porder[[1]])) {
    stop("list_porder (or ufg_candidate called) should be list of matrices.")
  }

  if (!(nrow(list_porder[[1]]) == ncol(list_porder[[1]]))) {
    stop("list_porder (or ufg_candidate called) should consist of
         square matrices")
  }

  # TODO
  # Test ob alles auch porders sind
  # TODO
  # Input check ausladen
}

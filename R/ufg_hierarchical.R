#' This is a helpfunction for ufg_1_depth_hierarchical and
#' ufg_2_depth_hierarchical
#'
#' @param objset object set
#' @param context (matrix of 0 and 1) representing the hierarchically ordered
#' data
#'
#' @return logical
check_mingenerator_hierarchical <- function(objset, context){
  if (all(objset == 0)) {return(FALSE)}
  extent <- operator_closure_obj_input(objset,context)
  indexs <- which(objset == 1)
  for (k in indexs) {
    reduced_objset <- objset
    reduced_objset[k] <- 0
    if (all(operator_closure_obj_input(
      reduced_objset,context) == extent)) {
      return(FALSE)
    }
  }
  return(TRUE)
}



#' Computes the ufg premises of cardinality one for hierarchically ordered data
#'
#' This is based on ufg depth introduced in:
#' Hannah Blocher, Georg Schollmeyer  (2024+): The union-free generic depth
#' using formal concept analysis.
#'
#' @description This function computes the contributing values for the ufg
#' depth given by one element ufg premises
#'
#' @param context (matrix of 1 and 0) representing a formal context
#' @param weights (vector) weights of objects
#'
#' @return list of depth value and number of ufg-premises with cardinality 1
#'
#' @export
ufg_1_depth_hierarchical <- function(context, weights = rep(1,nrow(context))) {
  # computes all ufg premises of cardinality 1 and the corresponding depth
  # values
  n_row <- nrow(context)
  result <- rep(0,n_row)
  S <- 0
  for (k in seq_len(n_row)) {
    extent <- rep(0,n_row)
    extent[k] <- 1
    check_result <- check_mingenerator_hierarchical(extent,context)
    if (!check_result) {
      print("warning: singleton premise that is not an ufg-premise")
    }
    if (check_result) {
      extent <- operator_closure_obj_input(extent,context)
      result[which(extent == 1)] = result[which(extent == 1)] + weights[k]
      S <- S + weights[k]
    }
  }

  return(list(depths = result/S, number_of_ufgs = S))
}



#' Computes the ufg premises of cardinality two for hierarchically ordered data
#'
#' This is based on ufg depth introduced in:
#' Hannah Blocher, Georg Schollmeyer  (2024+): The union-free generic depth
#' using formal concept analysis.
#'
#' @description This function computes the contributing values for the ufg
#' depth given by two element ufg premises
#'
#' @param context (matrix of 1 and 0) representing a formal context
#' @param weights (vector) weights of objects
#'
#' @return list of depth value and number of ufg-premises with cardinality 2
#'
#' @export
ufg_2_depth_hierarchical <- function(context, weights = rep(1,nrow(context))) {
  # computes all ufg premises of cardinality 2 and the corresponding depth
  # values, the corresponding intents are also computed
  n_row <- nrow(context)
  result <- rep(0,n_row)
  t <- 1
  S <- 0
  for (k in seq_len(n_row - 1)) {
    for (l in(seq(from = k + 1, to = n_row))) {
      extent <- rep(0,n_row)
      extent[c(k,l)] <- 1
      check_result <- check_mingenerator_hierarchical(extent,context)
      if (!check_result) {
        print("Warning: two-element premise that is not an ufg-premise")
      }
      if (check_result) {
        extent <- operator_closure_obj_input(extent,context)
        result[which(extent == 1)] = result[which(extent == 1)]
        + 2 * weights[k] * weights[l]
        S <- S + 2 * weights[k] * weights[l]
        t <- t + 1
      }
    }
  }
  return(list(depths = result/S, number_of_ufgs = S) )
}







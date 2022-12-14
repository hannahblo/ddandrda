#' Help Function of compute_conceptual_scaling()
#' Test if all input variables are valid
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


#' Test if the subcontext fc given by partial orders and the scaling method
#' "edge_non_edge_porder" is a generic premise
#'
#' @description This function test if a premise of partial orders is generic
#' based on containing
#'
#' @param fc_sub (matrix) Formal context describing a subset of partial orders
#' using the "edge_non_edge_porder" scaling method
#' @param cardinality_ufg (integer) number of partial orders, must be same
#' length as dim(fc_sub)[1]
#'
#' @return logical value stating wether fc_sub is a generic premise
test_generic_porder <- function(fc_sub, cardinality_ufg) {

  attr_distinguish <- which(colSums(fc_sub) == (cardinality_ufg - 1))

  # If cardinality of attr_distinguish is 1, what follows produces an error thus
  # we have to catch it bevor. As cardianlity_ufg>1 this cannot be an generator
  if (length(attr_distinguish) <= 1) {
    return(FALSE)
  }

  fc_sub_attr_dist <- fc_sub[, attr_distinguish]
  has_obj_distinguish_attr <- which(rowSums(fc_sub_attr_dist) <
                                      length(attr_distinguish))

  if (!(length(has_obj_distinguish_attr) == cardinality_ufg)) {
    return(FALSE)
  }

}


#' Test if the subcontext fc given by partial orders and the scaling method
#' "edge_non_edge_porder" is a union-free premise
#'
#' @description This function test if a premise of partial orders is  union-free
#' based on containing
#'
#' @param fc_sub (matrix) Formal context describing a subset of partial orders
#' using the "edge_non_edge_porder" scaling method
#' @param ufg_candidate (list of matrices) a list of partial orders given by
#' indices matrices (based on the same items) which should be checked weatther
#' they define a union-free premise. Note that length of ufg_candidate must
#' equl dim(fc_sub)[1]
#'
#' @return logical value stating wether fc_sub is a  union-free premise
test_ufree_porder <- function(fc_sub, ufg_candidate) {
  # Use that an object which does not ly in any proper subset of ufg_candidate
  # must fulfill that the corresponding intent holds and for each partial order
  # in ufg_candidate at least one distinguishable element must hold.

  intent <- which(colSums(fc_sub) == dim(fc_sub)[1])

  # this is done vio the formal subcontext
  number_item <- dim(ufg_candidate[[1]])[1]
  distingish_obj <- as.list(rep(NA, dim(fc_sub)[1]))
  index_disting_general <- which(colSums(fc_sub) == dim(fc_sub)[1] - 1)

  for (index_fc in seq(1, dim(fc_sub)[1])) {
    distingish_obj_inner <-
      index_disting_general[which(fc_sub[index_fc, index_disting_general] == 0)]
    edge_exists_part <- which(distingish_obj_inner <= number_item * number_item)

    if (any(edge_exists_part)) {
      distingish_obj[[index_fc]] <- index_disting_general[edge_exists_part]
    }
  }

  # Now use that we are only checking if a partial order exists in particular we
  # check if the "smallest" holding all these conditions exists.
  # Thus we use that a partial order must either have an pair or not, but both
  # together is not possible

  test_candidate <- matrix(rep(0, number_item * number_item),
                           ncol = number_item
  )
  diag(test_candidate) <- rep(1, number_item)

  for (index_intent in intent) {
    if (index_intent <= number_item * number_item) {
      test_candidate[index_intent] <- 1
    }
  }

  possibl_constr_disting <- expand.grid(distingish_obj)

  for (index_disting in seq(1, dim(possibl_constr_disting)[1])) {
    test_candidate_inner <- test_candidate

    for (index_intent in possibl_constr_disting[index_disting, ]) {
      if (!is.na(index_intent)) {
        if (index_intent <= number_item * number_item) {
          test_candidate_inner[index_intent] <- 1
        }
      }
    }

    test_candidate_inner <- compute_transitive_hull(test_candidate_inner)
    if (test_if_porder(test_candidate_inner)) {
      return(TRUE)
    }
  }

  # we didn't found a possible b which is only contained in the entire set but
  # in non subset --> Thus, return false
  return(FALSE)
}


#' Test if the subcontext fc given by partial orders and the scaling method
#' "edge_non_edge_porder" is a generic union-free premise
#'
#' @description This function test if a premise of partial orders is generic
#' nion-free based on containing
#'
#' @param fc_sub (matrix) Formal context describing a subset of partial orders
#' using the "edge_non_edge_porder" scaling method
#' @param input_check (logical, default TRUE) Wether the input should be checked
#'
#'
#' @return logical value stating wether fc_sub is a generic union-free premise
#'
#' @examples
#'
#' @export
test_ufg_porder <- function(ufg_candidate, input_check = TRUE) {

  # Input check
  if (!is.logical(input_check)) {
    stop("input_check must be logical.")
  }

  if (input_check) {
    check_input_porder_list(ufg_candidate)
  }

  # the number of elements must be strictly larger than 1 (else trivial)
  if (length(ufg_candidate) <= 1) {
    return(FALSE)
  }

  # check for duplications
  if (length(ufg_candidate) != length(unique(ufg_candidate))) {
    return(FALSE)
  }

  # set up / basis
  cardinality_ufg <- length(ufg_candidate)
  fc_sub <- compute_conceptual_scaling(input_porder = ufg_candidate)

  # check if generator (means minimal premise)
  if (!test_generic_porder(fc_sub = fc_sub,
                           cardinality_ufg = cardinality_ufg)) {
    return(FALSE)
  }

  # check if union-free
  return(test_ufree_porder(fc_sub = fc_sub, ufg_candidate = ufg_candidate))
}



#' Computes the ufg premises of a list of partial orders
#'
#' @description This function computes all union-free generic premises of the
#' a subset of partial orders. Hereby the partial orders are given as a list of
#' indice matrices. Further one can add a upper cardinality boundary for the
#' cardinality of the ufg premsies
#'
#' @param porder_list
#' @param scaling
#' @param max_card_ufg
#' @param input_check
#'
#' @return TODO
#'
#' @examples
#'
#' @export
compute_ufg_porder <- function(porder_list,
                               scaling = "porder_edge_non_edge",
                               max_card_ufg = NULL,
                               # method,
                               # param_method,
                               input_check = TRUE) {
  # Input check
  if (input_check) {
    # TODO
  }

  item_numbers <- dim(data_[[1]])[1]
  obs_numbers <- length(data_list)

  ufg_list <- list()

  if (scaling == "porder_edge_non_edge") {
    max_card <- (item_numbers * item_numbers) / 2
    if (is.null(scaling_param)) {
      max_card <- scaling_param[["max_card"]]
    }

    for (card_sub in 2:max_card) {
      all_subsets <- combn(1:obs_numbers, card_sub, simplify = FALSE)
      for (sub in all_subsets) {
        if (test_ufg_porder(data_list[sub])) {
          append(ufg_list) <- sub
        }
      }
    }
  }
  if (scaling == "porder_edge") {
    # TODO
  }
}



# compute_ufg_depth <- function(data = ,
#                               scaling = ,
#                               method = standard,
#                               param = NA){
#
#
#
#   if (scaling = "porder_edge_non_edge" & method = "naive" & is.na(param)) {
#     if (is.list(data) & is.matrix(data[[1]])) {
#       return(compute_ufg_porder_matrix(data_list = data,
#                                        scaling = "edge_non_edge"))
#     }
#     if (is.data.frame(data) || is.matrix(data)) {
#       return(compute_ufg_porder_fc())
#     }
#   }
#
#   stop("Input doesn't match to any possible ufg computation combination.
#        Please refer to ?compute_ufg_depth.")
#
# }

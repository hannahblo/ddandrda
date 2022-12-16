
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

  return(TRUE)
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

  # this is done via the formal subcontext
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
                           ncol = number_item)
  diag(test_candidate) <- rep(1, number_item)
  intent_edge <- intent[which(intent <= number_item * number_item)]

  test_candidate[intent_edge] <- 1

  possibl_constr_disting <- expand.grid(distingish_obj)

  for (index_disting in seq(1, dim(possibl_constr_disting)[1])) {
    test_candidate_inner <- test_candidate

    for (index_intent in possibl_constr_disting[index_disting, ]) {
      if (!is.na(index_intent)) {
          test_candidate_inner[index_intent] <- 1
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
#' @param ufg_candidate (matrix) Formal context describing a subset of partial
#' orders using the "edge_non_edge_porder" scaling method
#' @param input_check (logical, default TRUE) Wether the input should be checked
#'
#'
#' @return logical value stating wether fc_sub is a generic union-free premise
#'
#' @examples
#' relation_1 <- matrix(0, ncol = 4, nrow = 4)
#' diag(relation_1) <- 1
#'
#' relation_2 <- relation_1
#' relation_2[1, c(2,3,4)] <- 1
#' relation_2[2, c(3,4)] <- 1
#' relation_2[3, 4] <- 1
#'
#' list_porder_2 <- list(relation_1, relation_2)
#'
#' test_ufg_porder(list_porder_2)
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
#' @param porder_list (list of square matrix) list of matrices representing
#' partial orders of the same item dimension
#' @param max_card_ufg (integer or NULL) maximal cardinalty of the ufg premise
#' cardinality
#' @param input_check (logical) TRUE when input check sould be done, else set to
#' FALSE
#'
#' @return returns all ufg premises by indicating the one in porder_list
#'
#' @export
compute_ufg_porder_index <- function(porder_list,
                                     max_card_ufg = NULL,
                                     input_check = TRUE) {
  # Input check
  if (input_check) {
    check_input_porder_list(porder_list)
    if (!is.null(max_card_ufg) && !is.integer(max_card_ufg)) {
      stop("max_card_ufg needs to be either NULL or an integer.")
    }
  }

  if (length(porder_list) <= 1) {
    return(list())
  }

  item_numbers <- dim(porder_list[[1]])[1]
  obs_numbers <- length(porder_list)

  ufg_index_list <- list()

  max_card <- min((item_numbers * item_numbers) / 2,
                  max_card_ufg, obs_numbers)

  index_list <- 1
  for (card_sub in 2:max_card) {
    all_subsets <- utils::combn(1:obs_numbers, card_sub, simplify = FALSE)
    for (sub in all_subsets) {
      if (test_ufg_porder(porder_list[sub], input_check = FALSE)) {
        ufg_index_list[[index_list]] <- sub
        index_list <- index_list + 1
      }
    }
  }

  return(ufg_index_list)
}


#' Computes the ufg premises of a list of partial orders
#'
#' @description This function computes all ufg depth of the partial orders given
#' by porder_values based on porder_list. Hereby the partial orders are given as
#' a list ofindice matrices. Further one can add a upper cardinality boundary
#' for the cardinality of the ufg premsies
#'
#' @param porder_value (list of square matrix) list of matrices representing
#' partial orders of the same item dimension, based on these relations the ufg
#' is computed
#' @param porder_list (list of square matrixes) list of matrices representing
#' partial orders of the same item dimension, computation of the ufg value of
#' these partial orders
#' @param max_card_ufg (integer or NULL) maximal cardinalty of the ufg premise
#' cardinality
#' @param input_check (logical) TRUE when input check sould be done, else set to
#' FALSE
#'
#' @return returns all ufg premises by indicating the one in porder_list
#'
#' @export
compute_ufg_depth <- function(porder_value,
                              porder_list,
                              max_card_ufg = NULL,
                              input_check = TRUE) {
  # Input check
  if (input_check) {
    check_input_porder_list(unlist(list(porder_list, porder_value),
                                   recursive = FALSE))
    if (!is.null(max_card_ufg) && !is.integer(max_card_ufg)) {
      stop("max_card_ufg needs to be either NULL or an integer.")
    }
  }

  item_numbers <- dim(porder_list[[1]])[1]
  obs_numbers <- length(porder_list)

  ufg_index_list <- list()

  max_card <- min((item_numbers * item_numbers) / 2,
                  max_card_ufg, obs_numbers)

  proportion <- rep(0, length(porder_value))
  total_number <- 0
  for (card_sub in 2:max_card) {
    all_subsets <- utils::combn(1:obs_numbers, card_sub, simplify = FALSE)
    for (sub in all_subsets) {
      if (test_ufg_porder(porder_list[sub], input_check = FALSE)) {
        total_number <- total_number + 1
        proportion <- proportion +
          1 * test_porder_in_conclusion_newobjs(porder_list[sub], porder_value)

      }
    }
  }
  return(proportion / total_number)
}

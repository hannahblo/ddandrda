
# Preparation of computing the ufg premises
prepare_ufgpremises_poset <- function(list_mat_poset_ml,
                                      number_items) {

  fc_ml_porder <-
    compute_conceptual_scaling(input_porder = list_mat_poset_ml)
  porder_all <- compute_all_poset(number_items, list = FALSE, complemented = TRUE)

  data_context <- get_weighted_representation(fc_ml_porder) # duplication
  n_row_context <- nrow(data_context$x_weighted)
  count_dup <- data_context$counts
  number_obs <- sum(data_context$counts)
  list_poset_premises <- convert_fc_to_list_poset(data_context$x_weighted[ ,(1:(number_obs * number_obs))],  complemented = FALSE)

  whole_context <- rbind(data_context$x_weighted, porder_all) # context of all posets
  index <- which(!duplicated(whole_context))
  whole_context <- whole_context[index,]
  return(list(count_dup = count_dup,
              number_obs = number_obs,
              whole_context = whole_context,
              list_poset_premises = list_poset_premises,
              n_row_context = n_row_context))
}



# Computing the ufg depth based on already computed premises
compute_ufg_existprem_poset <- function(poset_interest, ufg_premises,
                                        prep_ufg_premises) {

  emp_prob <- prep_ufg_premises$count_dup / prep_ufg_premises$number_obs
  depth_ufg <- rep(0, length(poset_interest))
  constant_c <- 0

  for (i in 1:length(ufg_premises)) {
    # print(paste0("Iteration ", i,  " of ", dim(ufg_premises)[1]))
    index_premise <- ufg_premises[[i]]
    prod_emp_ufg <- prod(emp_prob[index_premise])
    concl_ufg <- test_poset_in_concl(prep_ufg_premises$list_poset_premises[index_premise], poset_interest) * 1

    depth_ufg <- depth_ufg + concl_ufg * prod_emp_ufg
    constant_c <- constant_c + prod_emp_ufg
  }

  depth_value <- depth_ufg / constant_c

  return(depth_value)

}



#' Computes the ufg depth of posets
#'
#' This is based on ufg depth introduced in:
#' Hannah Blocher, Georg Schollmeyer, Christoph Jansen and Malte Nalenz (2023):
#' Depth Functions for Partial Orders with a Descriptive Analysis of Machine
#' Learning Algorithms. In: Proceedings of the Thirteenth International Symposium
#' on Imprecise Probabilities: Theories and Applications (ISIPTA '23).
#' Proceedings of Machine Learning Research, vol. 215. PMLR.
#'
#' @description This function computes all ufg depth of the partial orders given
#' by poset_values based on poset_list. Hereby the partial orders are given as
#' a list ofindice matrices. Further one can add a upper cardinality boundary
#' for the cardinality of the ufg premsies
#'
#' @param poset_observed (list of square matrix) list of matrices representing
#' partial orders of the same item dimension, based on these relations the ufg
#' is computed
#' @param poset_depth (list of square matrixes) list of matrices representing
#' partial orders of the same item dimension, computation of the ufg value of
#' these partial orders
#' @param max_card_ufg (integer or NULL) maximal cardinality of the ufg premise
#' cardinality
#' @param min_card_ufg (integer or NULL) minimal cardinality of the ufg premise
#' @param input_check (logical) TRUE when input check sould be done, else set to
#' FALSE
#' @param print_progress_text (logical) TRUE when information about computation
#' progress should be added
#' @param save_ufg_premises (logical) TRUE when ufg premises stored and returned
#'
#' @return returns the ufg depth
#'
#' @export
compute_ufg_depth_poset <- function(poset_observed,
                                     poset_depth = NULL,
                                     min_card_ufg = NULL,
                                     max_card_ufg = NULL,
                                     input_check = TRUE,
                                     print_progress_text = TRUE,
                                     save_ufg_premises = FALSE) {
  # Input check
  if (input_check) {
    check_input_porder_list(poset_observed)
    if (!is.null(poset_depth)) {
      check_input_porder_list(poset_depth)
    }
    else {
      poset_depth <- poset_observed
    }
    if (!is.null(max_card_ufg) && !is.integer(max_card_ufg)) {
      stop("max_card_ufg needs to be either NULL or an integer.")
    }
    if (!is.logical(save_ufg_premises)) {
      stop("save_ufg_premise must be logical")
    }
  }



  # Deleting the duplicates and saving how often every element is observed
  item_numbers <- dim(poset_observed[[1]])[1]
  observed_as_df <- as.data.frame(t(matrix(unlist(poset_observed),
                                           ncol = length(poset_observed))))
  observed_dup <- dplyr::count(dplyr::group_by_all(observed_as_df))
  # dplyr::count() changes the order, thus we cannot use unique(poset_observed)
  observed_list_unique <-
    unlist(apply(observed_dup[, seq(1, item_numbers * item_numbers)], 1,
                 function(x) {list(matrix(x, ncol = item_numbers))}),
           recursive = FALSE)
  number_dupl <- observed_dup$n
  number_unique_obs <- length(observed_list_unique)

  # Compute the empirical distribution
  emp_prob <- number_dupl / sum(number_dupl)

  # Computing min_card and max_card
  max_card <- min((item_numbers * item_numbers)/2, max_card_ufg,
                  number_unique_obs)
  min_card <- max(min_card_ufg, 2)
  if (min_card > max_card) {
    stop("max_card_ufg and min_card_ufg do not match to the item values or to\n
         each other.")
  }

  # Going through all possible subset of cardinality between min_card and
  # max_card. Check if this subset is a ufg premise. If yes, add (include also
  # the duplications) those ufg premise to total_number and check which
  # poset_depth value lies in the conclusion.
  # For those which lie in the conclusion, add (include also the duplications)
  # the count_poset_depth value.
  ufg_premises <- list()

  # count_poset_depth <- rep(0, length(poset_depth)) -> kommt weg !!!!!!!!!!!!
  total_number_premises <- 0

  depth_ufg <- rep(0, length(poset_depth))
  constant_cn <- 0
  for (card_sub in min_card:max_card) {
    # we are going through all subsets of size card_sub.
    # We use  Gosper's Hack therefore
    # see: http://programmingforinsomniacs.blogspot.com/2018/03/gospers-hack-explained.html
    number_subsets <- choose(number_unique_obs, card_sub)
    number_iteration <- 1
    subset_binary <- c(rep(0, (number_unique_obs - card_sub)), rep(1, card_sub))
    while (TRUE) {
      if (print_progress_text) {
        print(paste0("Testing subset number ", number_iteration,
                     " of total ", number_subsets, " with cardinality ",
                     card_sub))
        number_iteration <- number_iteration + 1
      }
      if (test_ufg_poset(observed_list_unique[as.logical(subset_binary)], input_check = FALSE)) {
        total_number_premises <- total_number_premises +
          1 * prod(number_dupl[as.logical(subset_binary)])

        lies_in_concl <- test_poset_in_concl(
          observed_list_unique[as.logical(subset_binary)], poset_depth) * 1
        # count_poset_depth <- count_poset_depth + lies_in_concl *
        #   prod(number_dupl[as.logical(subset_binary)]) -> kommt weg!!!!!!!!!!!!!

        prob_obs_emp <-  prod(emp_prob[as.logical(subset_binary)])
        depth_ufg <- depth_ufg + lies_in_concl * prob_obs_emp
        constant_cn <- constant_cn + prob_obs_emp

        if (save_ufg_premises) {
          ufg_premises <- append(ufg_premises, list(observed_list_unique[as.logical(subset_binary)]))
        }

      }

      # switch to next subset or break while loop
      if (all(subset_binary[seq(1, card_sub)] == rep(1, card_sub))) {
        break
      }
      index_one <- which(subset_binary == 1)
      max_one <- max(index_one)
      max_zero_s_max_one <- max(
        which(subset_binary == 0)[which(subset_binary == 0) < max_one])
      subset_binary[c(max_zero_s_max_one, max_zero_s_max_one + 1)] <- c(1,0)
      ones_larger <- index_one[index_one > max_zero_s_max_one + 1]
      if (length(ones_larger) != 0) {
        subset_binary[seq(min(ones_larger), number_unique_obs)] <- 0
        subset_binary[seq(number_unique_obs - length(ones_larger) + 1,
                          number_unique_obs)] <- rep(1, length(ones_larger))
      }
    }
  }
  return(list(# ufg_depth = count_poset_depth/total_number, -> kommt weg!!!!!!!!!
    depth_ufg = depth_ufg / constant_cn,
    constant_cn = constant_cn,
    total_number_premises = total_number_premises,
    ufg_premises = ufg_premises))
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
#' length as dim(fc_sub)
#'
#' @return logical value stating wether fc_sub is a generic premise
test_generic_poset <- function(fc_sub, cardinality_ufg) {
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
#' based on containing. This function should not be executed without executing
#' test_generic_poset bevor and getting a TRUE value
#'
#' @param fc_sub (matrix) Formal context describing a subset of partial orders
#' using the "edge_non_edge_porder" scaling method
#' @param ufg_candidate (list of matrices) a list of partial orders given by
#' indices matrices (based on the same items) which should be checked weatther
#' they define a union-free premise. Note that length of ufg_candidate must
#' equl dim(fc_sub)
#'
#' @return logical value stating wether fc_sub is a  union-free premise
test_ufree_poset <- function(fc_sub, ufg_candidate) {
  # Use that an object which does not ly in any proper subset of ufg_candidate
  # must fulfill that the corresponding intent holds and for each partial order
  # in ufg_candidate at least one distinguishable element must hold.

  # intent <- which(colSums(fc_sub) == dim(fc_sub)[1])
  number_item <- dim(ufg_candidate[[1]])[1]


  # Now use that we are only checking if a partial order exists in particular we
  # check if the "smallest" holding all these conditions exists.
  # Thus we use that a partial order must either have an pair or not, but both
  # together is not possible
  # We do not need to compute the transitive hull as the intersection of partial
  # orders always is a partial order

  test_candidate <- Reduce("&", ufg_candidate,
                           matrix(rep(1, number_item * number_item),
                                  ncol = number_item)) * 1


  # check if this is already a valid candidate, in the sense that if every
  # partial order of ufg_candidate is needed to obtain this intersection.
  needed_for_intersection <- rep(TRUE, dim(fc_sub)[1])
  for (not_index in seq(1, dim(fc_sub)[1])) {
    test_candidate_inner <- Reduce("&", ufg_candidate[-not_index],
                                   matrix(rep(1, number_item * number_item), ncol = number_item)) * 1
    if (all(test_candidate == test_candidate_inner)) {
      needed_for_intersection[not_index] <- FALSE
    }
  }

  # if every partial order is needed, then we already found our candidate
  if (all(needed_for_intersection)) {
    return(TRUE)
  }

  # now we compute every intersection variants for those elements which have an
  # impact in the intersection
  # note that at least one needed_for_intersection must be FALSE, else we would
  # have duplicates and then the ufg_candidate wouldn't be generic.
  # see comment in the description
  if (any(needed_for_intersection)) {
    index_needed_intersect <- which(needed_for_intersection)

    candidate_wo_intersect <- list()
    for (i in seq_len(length(index_needed_intersect))) {
      candidate_wo_intersect[[i]] <- Reduce("&", ufg_candidate[-index_needed_intersect[[i]]],
                                            matrix(rep(1, number_item * number_item),
                                                   ncol = number_item)) * 1
    }
  } else {
    index_needed_intersect <- NULL
    candidate_wo_intersect <- NULL
  }


  # We compute the edges which are only given by one ufg_candidate.
  # This is done via the formal subcontext
  edge_unique <- as.list(rep(NA, dim(fc_sub)[1]))
  index_edge_unique_all <- intersect(
    which(colSums(fc_sub) == 1),
    seq(1, number_item * number_item)
  )

  # adding NA for those which are needed for the intersection. These candidates
  # can either have a uique edge which leads to the candidate or they delete an
  # edge which is needed for the ufg candidate --> therefore NA
  for (index_fc in seq(1, dim(fc_sub)[1])) {
    if (needed_for_intersection[index_fc]) {
      edge_unique_index_fc <- intersect(which(fc_sub[index_fc, ] == 1),
                                        index_edge_unique_all)
      if (length(edge_unique_index_fc) == 0) {
        edge_unique[[index_fc]] <- NA
      } else {
        edge_unique[[index_fc]] <- c(edge_unique_index_fc, NA)
      }
    } else {
      edge_unique[[index_fc]] <- intersect(which(fc_sub[index_fc, ] == 1),
                                           index_edge_unique_all)
    }
  }


  # If there exists a partial order which is not needed to obtain the inter-
  # section, then this partial order must have an edge which only is obtained by
  # including this partial order.
  # Thus for these partial orders the edge_unique part is not allowed to be
  # empty. If yes, then this is not a ufg as one can delete this one
  if (any(is.na(edge_unique[!needed_for_intersection]))) {
    return(FALSE)
  }

  # Now, we go through every possible candidate combinations and check weather
  # it is a candidate or not
  possible_add_edge <- expand.grid(edge_unique)
  union_ufg_candidate <- Reduce("|", ufg_candidate,
                                init =  matrix(0, nrow = number_item,
                                               ncol = number_item)) * 1

  for (index_add in seq(1:dim(possible_add_edge)[1])) {
    intersect_part <- which(is.na(possible_add_edge[index_add, ]))
    edge_part <- which(!is.na(possible_add_edge[index_add, ]))

    # edge_part must be at least length 2. In all other cases we would have already
    # delt with it
    test_candidate_inner <- test_candidate
    test_candidate_inner[as.numeric(possible_add_edge[index_add, edge_part])] <- 1
    test_candidate_inner <- compute_transitive_hull_poset(test_candidate_inner)



    # check if test_candidate is a subset of the union. If not, this is not
    # a candidate
    if (all(test_candidate_inner <= union_ufg_candidate)) {
      # check if test_candidate is a valid partial order (e.g. circles could
      # exists)
      if (test_if_poset(test_candidate_inner)) {
        # when the intersection_candidate exists, then check if it is needed to
        # obtain the test_candidate. If not, then one can delete this candidate

        if (length(intersect_part) == 0) {
          return(TRUE)
        } else {

          # this can only be one index
          index_wo_intersect <- which(index_needed_intersect %in% intersect_part)

          is_needed_inner <- rep(FALSE, length(index_wo_intersect))
          for (i in 1:length(index_wo_intersect)) {
            candidate_wo_intersect_inner <- candidate_wo_intersect[[index_wo_intersect[i]]]
            candidate_wo_intersect_inner[as.numeric(possible_add_edge[index_add, edge_part])] <- 1
            candidate_wo_intersect_inner <- compute_transitive_hull_poset(candidate_wo_intersect_inner)
            if (!all(candidate_wo_intersect_inner == test_candidate_inner)) {
              is_needed_inner[[i]] <- TRUE
            } else {
              break
            }
          }
          if (all(is_needed_inner)) {
            return(TRUE)
          }
        }
      }
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
#' relation_2[1, c(2, 3, 4)] <- 1
#' relation_2[2, c(3, 4)] <- 1
#' relation_2[3, 4] <- 1
#'
#' list_porder_2 <- list(relation_1, relation_2)
#'
#' test_ufg_poset(list_porder_2)
#'
#' @export
test_ufg_poset <- function(ufg_candidate, input_check = TRUE) {
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
  if (!test_generic_poset(
    fc_sub = fc_sub,
    cardinality_ufg = cardinality_ufg
  )) {
    return(FALSE)
  }

  # check if union-free
  return(test_ufree_poset(fc_sub = fc_sub, ufg_candidate = ufg_candidate))
}




#' Enumerate all ufg premises of a complemented poset context
#'
#' @description 'enumerate_ufg_premises' enumerates all ufg premises for a set
#' of partial orders given as a complemented context
#'
#'
#' @param whole_context is the whole context that describes the whole space of
#' complemented partial orders.
#'
#' For the implementation with 'test_ufg_porder' as a test function for checking
#' ufg-candidates it is not needed to supply the whole context of all partial
#' orders, instead it is enough to supply only the orders obtained in the data
#' sample. Currently the implementation uses 'test_ufg_porder'
#'
#' For the implementation with 'test_explicitly_ufg_p_order' as a test function
#' for checking ufg-candidates it is needed to supply the context of all
#' partial orders. The current implementation does not use the function
#' 'test_explicitly_ufg_p_order'
#'
#'
#'
#' @param n_row_context is the number of objects in the observed data sample.
#' It is assumed that the first 'n_row_context' rows of the context
#' 'whole_context' represent the observed partial orders.
#'
#'
#' @param print_progress If TRUE, the progress of the enumeration will printed.
#'
#'
#' @return A list of all ufg premises given as a vector of indices (w.r.t. the
#' first 'n_row_context' rows of the context 'whole_context').
#'
#' @export
enumerate_ufg_premises_poset <- function(whole_context, n_row_context,
                                         print_progress = TRUE) {
  # compute list of porders (this is only needed for the function
  # test_ufg_porder)
  data_list <- convert_fc_to_list_poset(
    whole_context[
      seq_len(n_row_context),
      seq_len(ncol(whole_context) / 2)
    ],
    complemented = FALSE
  )

  # fast version for checking if ufg set is in the list of already enumerated
  # ufg sets
  "%fin%" <- fastmatch::"%fin%"

  n_items <- sqrt(ncol(whole_context) / 2)
  ## currently nowhere used
  upper_bound_ufg_dimension <- n_items * (n_items - 1) / 2

  # list of of sets is stored in the list 'result'
  result <- list()

  # counter for countinf currently enumerated ufg premise, important for
  # the look-up list 'sets'
  counter <- 1

  # 0-1 vector specifying ufg premise (entries with higher index than
  # n_row_context will always be 0)
  subset <- rep(0, n_row_context)

  # The list 'sets' serves as a look-up list. and the following function
  # 'enum_ufg_premises_recursive' stops if an ufg-premise was already visited.
  sets <- list()


  # For ease of enumeration, premises of size 1 are also enumerated but in the
  # end they are excluded
  simple_ufg_index <- NULL





  # This function recursively enumerates all ufg-premises and stores them in the
  # list 'result'. The list sets serves as a look up list and the function stops
  # if an ufg-premise was already visited.
  enum_ufg_premises_recursive <- function(subset, whole_context,
                                          n_row_context) {
    if (paste(which(subset == 1), collapse = ";") %fin% sets[seq_len(counter)]) {
      stop
    }

    # The objects in the hull of a current ufg-premise can not be used
    # to enlarge the ufg. Therefore they are later excluded beforehand
    # (see line ''index <- which(extent==0 & mask==1)''
    extent <- operator_closure_obj_input(
      subset,
      whole_context[seq_len(n_row_context), ]
    )

    if (all(extent == 1)) {
      stop
    }

    # every order p of the current ufg set genuinely removes some attributes
    # from the intent. If another order p2 (outside the current ufg set)
    # does also remove these attributes (and possibly more), then p would
    # become redundant. Therefore, such orders p2 are excluded by mask ,
    # cf. line ''index <- which(extent==0 & mask==1)''

    mask <- rep(1, n_row_context)
    for (k in which(subset == 1)) {
      subset_new <- subset
      subset_new[k] <- 0

      intent <- calculate_psi(subset_new, whole_context[seq_len(n_row_context), ])
      idx <- which(whole_context[k, ] == 0 & intent == 1)
      if (length(idx) >= 2) {
        idx2 <- which(rowSums(whole_context[seq_len(n_row_context), idx]) == 0)
        mask[idx2] <- 0
      }
      if (length(idx) == 1) {
        mask[which(whole_context[seq_len(n_row_context), idx] == 0)] <- 0
      }
    }

    # index of orders that are worth inspecting if they can be added to enlarge
    # the ufg set
    index <- which(extent == 0 & mask == 1)
    ## Versuch
    if (sum(subset) >= 2) {
      i1 <- which(subset[seq_len(n_row_context)] == 1)
      #print(which(subset==1))
      #print("i1:")
      #print(i1)
      index_dist_attr <- which(colSums(whole_context[i1,]) == length(i1) - 1)
      #print(index_dist_attr)
      i2 <- which(rowSums(whole_context[seq_len(n_row_context),index_dist_attr]) >= length(i1))
      # print(which( ! (index%in%i2)))
      #index <- setdiff(index,i2)
    }

    ## Versuch

    # stop if there cannot be added any order
    if (length(index) == 0) {
      stop
    }
    for (k in index) {
      subset_new <- subset
      subset_new[k] <- 1
      subset_new_whole_context <- c(subset_new, rep(0, nrow(whole_context) -
                                                      n_row_context))

      # proceed if |ufg|=1 or if subset_new was not already visited
      subset_new <<- subset_new
      if (sum(subset_new) == 1 |
          (!(paste(which(subset_new == 1), collapse = ";") %fin%
             sets[seq_len(counter)]))) {
        # This would be the line if one would use the explicit function for
        # testing
        # the ufg-property.

        # if(sum(subset_new)==1 |   test_explicitly_ufg_p_order(
        #  subset_new_whole_context,whole_context)){
        #
        # proceed if |ufg|=1 or uf subset_new is an ufg set


        # This would be the line if one would use the direct function
        # test_ufg_porder for testing the ufg-property.
        if (sum(subset_new) == 1 |
            test_ufg_poset(data_list[which(subset_new == 1)]) == TRUE) {
          subset_new_index <- which(subset_new == 1)
          # store ufg set in list result
          result[[counter]] <<- subset_new_index
          # store ufg set also in the look up list (as character)
          sets[[counter]] <<- paste(subset_new_index, collapse = ";")
          # if the ufg set is a singleton store this in the index-vector
          # simple_ufg_index
          if (length(subset_new_index) == 1) {
            simple_ufg_index <<- c(
              simple_ufg_index, counter
            )
          }

          # set counter that counts the number of ufg sets to counter + 1
          counter <<- counter + 1
          if (print_progress & counter %% 1000 == 0) {
            print(paste("enumerated ",
                        counter,
                        " ufg-premises",
                        collapse = ""
            ))
          }
          # proceed with the new ufgset subset_new
          enum_ufg_premises_recursive(subset_new, whole_context, n_row_context)
        }
      }
    }
    stop
  }



  # Now call enum_ufg_premises_recursive and return the results
  #
  enum_ufg_premises_recursive(subset, whole_context, n_row_context)
  result <- result[-counter]
  result <- result[-simple_ufg_index]
  return(result)
}

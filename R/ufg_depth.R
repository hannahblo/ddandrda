

### formal context based on a subset of partial order (definition)
compute_fc_subset_porders <- function(partial_orders) {
  # @partial_orders(list of matrices): each matrix represents a partial order
  # IMPORTANT: As computation is much easier to understand if reflexiv part is added,
  # we leave this redundant row!!!!!!!!

  stop("noch durchgehen obdas passt, insb mit packet, bisher nur kopiert")
  fc <- matrix(, ncol = length(partial_orders[[1]]) * 2, nrow = length(partial_orders))

  for (i in 1:length(partial_orders)) {
    fc[i, ] <- c(partial_orders[[i]], !partial_orders[[i]])
  }

  return(fc)
}

# compute_disting_attr <- function(partial_orders) {
#
#   stop("noch durchgehen obdas passt, insb mit packet, bisher nur kopiert")
#
#   # this is done vio the formal subcontext
#   fc <- compute_fc_subset_porders(partial_orders)
#   distingishable_set <- as.list(rep(NA, length(partial_orders)))
#   index_disting_general <- which(colSums(fc) == dim(fc)[1] - 1)
#
#   for (index_po in seq(1, length(partial_orders))) {
#     distingishable_set[[index_po]] <- index_disting_general[which(fc[index_po, index_disting_general] == 0)]
#   }
#
#   return(distingishable_set)
# }
#
# test_if_ufg_partial <- function(ufg_candidate) {
#
#   stop("noch durchgehen obdas passt, insb mit packet, bisher nur kopiert")
#
#   # the number of elements must be strictly larger than 1 (else trivial)
#   if (length(ufg_candidate) <= 1) {
#     return(FALSE)
#   }
#
#   # check if generator
#   cardinality_ufg <- length(ufg_candidate)
#   fc_sub <- compute_fc_subset_porders(ufg_candidate)
#   attr_distinguish <- which(colSums(fc_sub) == (cardinality_ufg - 1))
#
#   # If cardinality of attr_distinguish is 1, what follows produces an error thus,
#   # we have to catch it bevor. As cardianlity_ufg>1 this cannot be an generator
#   if (length(attr_distinguish) == 1) {
#     return(FALSE)
#   }
#
#   fc_sub_attr_dist <- fc_sub[, attr_distinguish]
#   has_obj_distinguish_attr <- which(rowSums(fc_sub_attr_dist) < length(attr_distinguish))
#
#   if (!(length(has_obj_distinguish_attr) == cardinality_ufg)) {
#     return(FALSE)
#   }
#
#   # check if ufg
#   # Use that an object which does not ly in any proper subset of ufg_candidate
#   # must fulfill that the corresponding intent holds and for each partial order
#   # in ufg_candidate at least one distinguishable element must hold.
#
#   intent <- compute_intent(ufg_candidate)
#   disting_set <- compute_disting_attr(ufg_candidate)
#
#   # Now use that we are only checking if a partial order exists in particular we
#   # check if the "smallest" holding all these conditions exists.
#   # Thus we use that a partial order must either have an pair or not, but both
#   # together is not possible
#   number_item <- dim(ufg_candidate[[1]])[1]
#   test_candidate <- matrix(rep(0, number_item  * number_item) , ncol = number_item)
#   diag(test_candidate) <- rep(1, number_item) # ??wird nicht benötigt da im intent auch die redundanten reflexiven drin sind
#
#   for (index_intent in intent) {
#     if (index_intent <= number_item * number_item) {
#       test_candidate[index_intent] <- 1
#     }
#   }
#
#   possible_constraints_by_disting <- expand.grid(disting_set)
#
#   for (index_disting in seq(1, dim(possible_constraints_by_disting)[1])) {
#     test_candidate_inner <- test_candidate
#
#     for (index_intent in possible_constraints_by_disting[index_disting, ]) {
#       if (index_intent <= number_item * number_item) {
#         test_candidate_inner[index_intent] <- 1
#       }
#     }
#
#     test_candidate_inner <- compute_transitive_hull(test_candidate_inner)
#     if (test_if_porder(test_candidate_inner)) {
#       return(TRUE)
#     }
#   }
#
#   # we didn't found a possible b which is only contained in the entire set but in
#   # non subset --> Thsus, return false
#   return(FALSE)
# }
#
#
#
#
# compute_ufg_porder_matrix <- function(data_list, scaling){
#
#   item_numbers <- dim(data_list[[1]])[1]
#   obs_numbers <- length(data_list)
#
#   ufg_list <- list()
#
#   if (scaling == "edge_non_edge") {
#     max_card <- (item_numbers * item_numbers) / 2
#     for (card_sub in 2:max_card) {
#       all_subsets <- combn(1:obs_numbers, card_sub, simplify = FALSE)
#       for (sub in all_subsets) {
#         if (test_ufg_porder(data_list[sub])) {
#           append(ufg_list) <- sub
#         }
#       }
#     }
#   }
#   if (scaling == "edge"){
#
#   }
# }
#
# compute_ufg_porder_fc <- function(){
#
# }
#
#
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
#   stop("Input doesn't match to any possible ufg computation combination! Please
#        refer to ?compute_ufg_depth.")
#
# }

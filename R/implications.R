# Computation of conclusion ---------------------------------------------------

#' Computes the conclusion based on interordinal scaled attribute
#'
#' @description Based on interordnial scaling this functions computes every
#' object which lies in the extent set given by the insert objects
#'
#' @param premise (vector of (0,1)): 1 represents that the point is within the
#' subset
#' @param list_info (containing data_values): numeric attribute of each
#' observation (same length as premise)
#'
#' @return (list): the premise input and the conclusion, two vectors with
#' elements  in 0 and 1, with 1 representing that the observation is in the set
compute_interordinal_concl <- function(premise, list_info) {
  data_values <- list_info$data_values
  index_premise <- which(premise == 1)

  inner_conclusion <- rep(0, length(data_values))

  # every point with attribute being between maximal and minimal attribute value
  # of the subset is within the premises
  if (sum(index_premise) > 0) {
    min_premise_value <- min(data_values[index_premise])
    max_premise_value <- max(data_values[index_premise])

    index_conclusion <- which((data_values >= min_premise_value) &
      (data_values <= max_premise_value))

    inner_conclusion[index_conclusion] <- 1
    inner_conclusion[index_premise] <- 1
  }


  return(list(premise = premise, conclusion = inner_conclusion))
}


#' Computes the conclusion based on nominal scaled attribute
#'
#' @description Based on interordnial scaling this functions computes every
#' object which lies in the extent set given by the insert objects
#'
#' @param premise (vector of (0,1)): 1 represents that the point is within the
#' subset
#' @param list_info (containing data_values): nominal attribute of each
#' observation (same length as premise)
#'
#' @return (list): the premise input and the conclusion, two vectors with
#' elements  in 0 and 1, with 1 representing that the observation is in the set
compute_nominal_conclusion <- function(premise, list_info) {
  data_values <- list_info$data_values
  index_premise <- which(premise == 1)
  inner_conclusion <- rep(0, length(data_values))

  # if only one attribute value is in the premises, then the conclusion contains
  # each observation with the same attribute value
  # If two or more attributes occur in the premise, then no attribute holds for
  # every element in the premise and thus, the entire set is the conclusion
  if (sum(index_premise) > 0) {
    premise_value <- unique(data_values[index_premise])
    if (length(premise_value) > 1) {
      inner_conclusion <- rep(1, length(data_values))
    }
    if (length(premise_value) == 1) {
      index_conclusion <- which(data_values %in% premise_value)
      inner_conclusion[index_conclusion] <- 1
    }
  }
  return(list(premise = premise, conclusion = inner_conclusion))
}




# Computation generator --------------------------------------------------------

#' Calculates the generator based on interordinal scaled attributes
#'
#' @description Based on interordinal scaling this function computes the
#' generators the conclusion given by subset. The definition of generator is
#' given by TODO
#'
#' @param subset (vector of (0,1)): 1 represents that the point is within the
#' subset
#' @param list_info (containing data_values): numeric attribute of each
#' observation  (same length as premise)
#'
#' @return the generator of the closure of subset, vectors with elements in 0
#' and 1, with 1 representing that the observation is in  the set
compute_generator_interordinal <- function(subset, list_info) {
  data_values <- list_info$data_values
  index_subset <- which(subset == 1)

  # the closure of the empty set is the empty set and for one point it is this
  # point, since we assume no duplication exists.
  if (length(index_subset) <= 1) {
    return(list(subset))
  }

  # The generator is smallest and the largest attribute value
  min_subset_value <- min(data_values[index_subset])
  max_subset_value <- max(data_values[index_subset])

  index_min <- which(data_values == min_subset_value)
  index_max <- which(data_values == max_subset_value)

  # Obtain all values which are within the subset and also have min/max value
  index_min_subset <- intersect(index_subset, index_min)
  index_max_subset <- intersect(index_subset, index_max)

  generator <- list()
  for (count_lower in index_min_subset) {
    for (count_upper in index_max_subset) {
      inner_generator <- rep(0, length(data_values))
      inner_generator[c(count_lower, count_upper)] <- 1

      generator <- append(generator, list(inner_generator))
    }
  }
  return(generator)
}


#' Calculates the generator based on nominal scaled attributes
#'
#' @description Based on interordinal scaling this function computes the
#' generators the conclusion given by subset. The definition of generator is
#' given by TODO
#'
#' @param subset (vector of (0,1)): 1 represents that the point is within the
#' subset
#' @param list_info (containing data_values): factor attribute of each
#' observation  (same length as premise)
#'
#' @return the generator of the closure of subset, vectors with elements in 0
#' and 1, with 1 representing that the observation is in  the set
compute_generator_nominal <- function(subset, list_info) {
  data_values <- list_info$data_values
  subset <- compute_nominal_conclusion(subset,
    list_info = list_info
  )$conclusion
  index_subset <- which(subset == 1)

  # the generator of the empty set is the empty set
  if (length(index_subset) < 1) {
    return(list(subset))
  }

  subset_values <- data_values[index_subset]
  generator <- list()

  # If every element of the closure has the same attribute value, each element
  # of which has this attribute is a generator
  if (length(unique(subset_values)) == 1) {
    for (counter in index_subset) {
      inner_generator <- rep(0, length(data_values))
      inner_generator[counter] <- 1
      generator <- append(generator, list(inner_generator))
    }
  } else {
    # Each subset which contains two different factor values is a generator
    for (count_1 in 1:(sum(subset) - 1)) {
      for (count_2 in (count_1 + 1):sum(subset)) {
        index_1 <- index_subset[count_1]
        index_2 <- index_subset[count_2]
        if (data_values[count_1] != data_values[count_2]) {
          inner_generator <- rep(0, length(data_values))
          inner_generator[c(index_1, index_2)] <- 1
          generator <- append(generator, list(inner_generator))
        }
      }
    }
  }

  return(generator)
}





# Test if new observation lies in conclusion -----------------------------------

#' Test if new observation lies in conclusion based on interordinal scaling
#'
#' @description Based on interordinal scaling this function tests if a further
#' object lies in the conclusion of a premise
#'
#' @param subset (vector of (0,1)): 1 represents that the point is within the
#' subset
#' @param obj_numeric_obs (numeric): observation to test if lies in conclusion
#' @param info_list (containing data_values): numeric attribute of each
#' observation  (same length as premise)
#'
#' @return logical value. TRUE if obj_numeric_obs lies in the conclusion, else
#' FALSE is returened
test_interordinal_in_concl <- function(subset, obj_numeric_obs,
                                                   info_list) {
  index_subset <- which(subset == 1)

  if (sum(index_subset) == 0) {
    return(FALSE)
  }

  subset_attr <- info_list[["data_values"]][index_subset]
  if ((min(subset_attr) <= obj_numeric_obs) &&
    (obj_numeric_obs <= max(subset_attr))) {
    return(TRUE)
  }

  return(FALSE)
}



#' Test if new observation lies in conclusion based on nominal scaling
#'
#' @description Based on nominal scaling this function tests if a further
#' object lies in the conclusion of a premise
#'
#' @param subset (vector of (0,1)): 1 represents that the point is within the
#' subset
#' @param obj_ordinal_obs (nominal): observation to test if lies in conclusion
#' @param info_list (containing data_values): nominal attribute of each
#' observation  (same length as premise)
#'
#' @return logical value. TRUE if obj_nominal_obs lies in the conclusion, else
#' FALSE is returened
test_ordinal_in_concl <- function(subset, obj_ordinal_obs,
                                              info_list) {
  index_subset <- which(subset == 1)

  if (sum(index_subset) == 0) {
    return(FALSE)
  }

  subset_attr <- info_list[["data_values"]][index_subset]
  return(obj_ordinal_obs %in% subset_attr)
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
test_porder_in_concl <- function(subset, obj_porder_obs,
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

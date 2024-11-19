
#' compute weighted representation of a data matrix
#'
#' @description computes weighthed representation of a data matrix x with
#' duplicated rows, returns unique(x) together with counts: how often appears
#' the column, mean_y: mean of y in the set of the duplicated columns
#' WARNING: the order of the rows generally differs from the order one would get
#' by applying the function 'unique()'.
#' @param x data matrix
#' @param y a further variable. The mean of y in every set of duplicated
#' x-rows is computed
#'
#' @return list with mean_y: mean of y in the set of the duplicated columns
#' counts: how often appears the row in the original data matrix X
  
#' @export
get_weighted_representation <- function(x, y = rep(1, dim(x)[1])) {
  ## computes weighted representation of a data matrix x with duplicated rows,
  ##  returns unique(x) together with counts: how often appears the column,
  # mean_y: mean of y in the set of the duplicated columns
  xd <- data.frame(cbind(x, y))
  names(xd)[1] <- "v1"
  v1 <- "v1"
  p <- dim(x)[2]
  result <- as.matrix(plyr::ddply(xd, names(xd[(1:p)]),
                                  dplyr::summarise,
                                  count = length(v1),
                                  mean.y = mean(y), sum.y = sum(y)))
  x_weighted <- result[, (1:p)]
  colnames(x_weighted) <- colnames(x)
  return(list(x_weighted = x_weighted,
              y_weighted = result[, p + 3],
              mean_y = result[, p + 2],
              counts = result[, p + 1]))
}




#' Defines the closure operator for computing all extents (objects)
#'
#' @description computes the closure operator on the object set based on a
#' formal context
#'
#' @param subset_object (array): set of objects
#' @param context (matrix): formal context which is used to calculate the
#' extent
#'
#' @return subset (array): to smallest closure in the FCA based on
#' subset_object and context
operator_closure_obj_input <- function(subset_object, context) {
  calculate_phi(calculate_psi(subset_object, context), context)
}


#' Defines the closure operator for computing all extents (objects)
#'
#' @description computes the closure operator on the intent set based on a
#' formal context
#'
#' @param subset_attribute (array): set of attributes
#' @param context (matrix): formal context which is used to calculate the
#' intent
#'
#' @return subset (array): to smallest closure in the FCA based on
#' subset_object and context
operator_closure_attr_input <- function(subset_attribute, context) {
  calculate_psi(calculate_phi(subset_attribute, context), context)
}


#' Calculates for a subset of attributes the minimal extent based on the
#' given context
#'
#' @param subset_attributes (array): set of attributes
#' @param context (matrix): formal context which is used to calculate the extent
#'
#' @return subset (array): the smallest extent (set of objects) in the FCA
#                         based on subset_attributes and the formal context
calculate_phi <- function(subset_attributes, context) {
  index_attribute <- which(subset_attributes == 1)
  selected_attributes <- as.matrix(context[, index_attribute])
  dim(selected_attributes) <- c(dim(context)[1], length(index_attribute))

  # Counting for each object how many selected attributes hold and choosing the
  # one where all attributes are true
  count_objects_attribute_hold <- rowSums(selected_attributes)
  index_obejct <- which(count_objects_attribute_hold == length(index_attribute))

  # returning a list which represents which objects correspond to the considered
  # attribute set
  extend <- rep(0, dim(context)[1])
  extend[index_obejct] <- 1

  return(extend)
}



#' Calculates for a subset of objects the minimal intent based on the given
#'context
#'
#' @param subset_objects (array): set of attributes
#' @param context (matrix): formal context which is used to calculate the intent
#'
#' @return subset (array): the smallest intent (set of attributes) in the FCA
#                         based on subset_objects and the formal context
calculate_psi <- function(subset_objects, context) {
  # Determines and sub-setting the objects which are selected
  index_object <- which(subset_objects == 1)
  selected_objects <- as.matrix(context[index_object, ])
  dim(selected_objects) <- c(length(index_object), dim(context)[2])

  # Counting for each attribute how many selected objects are related and chose
  # the ones where all objects are related
  count_atts_object_related <- colSums(selected_objects)
  index_attribute <- which(count_atts_object_related ==
                             length(index_object))

  # returning an array which represents the attributes which correspond to the
  # considered object set
  intent <- rep(0, dim(context)[2])
  intent[index_attribute] <- 1
  return(intent)
}

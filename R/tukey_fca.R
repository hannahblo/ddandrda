# computes Tukeys outlyingness for a given context. This function is only
# internaly used for computing Tukeys depth as 1- Tukeys outlyingness.
# Thereofre, for details look at the documentation of the exportet function
# compute_tukeys_depth directly below
compute_tukeys_outlyingness <- function(intent,
                                        context,
                                        row_weights = rep(1, nrow(context)),
                                        col_weights = rep(1, ncol(context))) {
  if (all(intent == 1)) {
    return(0)
  }
  # compute weighted column-max
  normed_row_weights <- row_weights / sum(row_weights)
  weighted_context <- ((normed_row_weights) %*% t(col_weights)) * context
  weighted_column_means <- t(weighted_context) %*% (rep(1, nrow(context)))
  if (is.vector(intent)) {
    return(max(weighted_column_means[which(intent == 0)]))
  }
  if (is.matrix(intent)) {
    return(sapply(as.list(as.data.frame(t(intent))),
                  compute_tukeys_outlyingness,
                  context = context, row_weights = row_weights,
                  col_weights = col_weights, simplify = TRUE
    ))
  }
}

#' Computes Tukeys depth of an intent w.r.t. a given formal context
#'
#' @description 'compute_tukeys_depth' returns the depth value of an object
#' represented by its intent (given as a 0-1 vector) w.r.t. a data cloud
#' represented by a formal context.
#'
#' @param intent represents the envisaged object given by all its attributes
#' given as a 0-1 vector. It is also possible to compute depth values for
#' more objects. In this case the objects should be given as a matrix where
#' each row corresponds to one
#' object.
#'
#' @param context is a formal context whose objects represent the data cloud
#' w.r.t. which Tukeys depth is computed.
#'
#' @param row_weights it is possible to give every object a weight (with the
#' interpretation that objects with e.g. weight $2$ appear twice as often in the
#'  data set as objects with weight 1).
#'
#' @param col_weights it is possible to give every attribute a weight (with the
#' interpretation that a weighted maximum (minimum) is calculated for
#' Tukeys outlyingness (depth) such that attributes with higher weights get
#' more important).
#'
#' @return returns the depth value(s) of the object(s) w.r.t. the data cloud.
#'
#' @examples
#'
#' context <- fcaR::planets
#' depth_values <- compute_tukeys_depth(context, context)
#' table(depth_values)
#' which(depth_values == max(depth_values))
#' which(depth_values == min(depth_values))
#'
#' @export
compute_tukeys_depth <- function(intent,
                                 context,
                                 row_weights = rep(1, nrow(context)),
                                 col_weights = rep(1, ncol(context))) {
  # Tukeys depth is simply computed as 1- Tukeys outlyingness
  return(1 - compute_tukeys_outlyingness(
    intent,
    context,
    row_weights,
    col_weights
  ))
}

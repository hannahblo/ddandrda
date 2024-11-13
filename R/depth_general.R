#' Computes the quasiconcave hull of a depth function
#'
#' This is based on ufg depth introduced in:
#' Hannah Blocher, Georg Schollmeyer  (2024+): The union-free generic depth
#' using formal concept analysis.
#'
#' @param depth_values (vector) depth value of the objects (same order as the
#' one of the context)
#' @param context (matrix with 0 and 1) representing the context corresponding
#' to the depth
#'
#' @returns vector
#'
#' @export
compute_quasiconcave_hull <- function(depth_values, context) {
  # computes the quasiconcave hull of a depth function
  ans <- rep(0, length(depth_values))
  for (k in sort(unique(depth_values))) {
    i <- which(depth_values >= k)
    temp <- rep(0, nrow(context))
    temp[i] <- 1
    temp <- operator_closure_obj_input(temp, context)
    ans[which(temp == 1)] <- k
  }
  return(ans)
}

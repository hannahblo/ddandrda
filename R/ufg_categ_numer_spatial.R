
#' needed in test_ufg_main_cns
#' This function tests if the subset is a ufg-premise WITHOUT utilizing that the
#' data are mixed spatial, numeric, nominal
#'
#' @param observed (nx2) the observed values
#' @param observed_in_grid (vector) assignment to the grid
#' @param sf_spatial (sf-object) converted spatial_grid object to sf-object
#' @param grid_spatial (Lx2 matrix) the spatial component of the entire grid
#' (L is the number of grids)
#' @param grid_numeric (vector) the numeric component of the entire grid
#' @param grid_nominal (vector) the nominal component of the entire grid
test_ufg_classic_cns <- function(observed,
                             observed_in_grid,
                             sf_spatial,
                             grid_spatial,
                             grid_numeric,
                             grid_nominal) {

  # computing the closure
  hull_set <- compute_hull_inner_cns(observed[, c(1,2)],
                                 observed_in_grid,
                                 sf_spatial,
                                 grid_spatial,
                                 grid_numeric,
                                 grid_nominal)

  if (sum(hull_set) == length(observed_in_grid)) { # trivial
    result <- list(is_ufg = FALSE,
                   hull_set = hull_set)
  }
  if (sum(hull_set) != length(observed_in_grid)) {
    union_hull_subsets <- rep(FALSE, dim(grid_spatial)[1])
    # check if the union of proper subsets are the same
    for (i in 1:length(observed_in_grid)) {
      inner_observed <- observed[-i, ]
      inner_observed_in_grid <- observed_in_grid[-i]
      inner_hull <- compute_hull_inner_cns(inner_observed[, c(1,2)],
                                       inner_observed_in_grid,
                                       sf_spatial,
                                       grid_spatial,
                                       grid_numeric,
                                       grid_nominal)
      union_hull_subsets <- union_hull_subsets | inner_hull
    }
    result <- list(is_ufg = !all(union_hull_subsets == hull_set),
                   hull_set = hull_set)
  }
  return(result)
}


#' needed in test_ufg_main_cns
#' This function checks which point lies in the spatial boundary of the convex
#' set
#'
#' @param observed (nx2) the observed values
#' @param observed_in_grid (vector) assignment to the grid
check_boundary_input_cns <- function(observed,
                                 observed_in_grid) {

  sf_set <- sf::st_multipoint(observed[, c(1,2)])
  hull_obs <- sf::st_convex_hull(sf_set)

  if (c("LINESTRING") %in% class(hull_obs)) {
    match_x <- !is.na(match(observed[, 1], hull_obs[, 1]))
    match_y <- !is.na(match(observed[, 2], hull_obs[, 2]))
  } else {
    match_x <- !is.na(match(observed[, 1], hull_obs[[1]][, 1]))
    match_y <- !is.na(match(observed[, 2], hull_obs[[1]][, 2]))
  }


  lies_in_bgrid <- observed_in_grid[!(match_x & match_y)]

  lies_boundary_bgrid <-  observed_in_grid[(match_x & match_y)]

  is_boundary <- (length(lies_in_bgrid) == 0)

  return(list(is_boundary = is_boundary,
              lies_in = lies_in_bgrid,
              lies_boundary = lies_boundary_bgrid))
}


#' needed in test_ufg_main_cns
#' This function checks if it could be a ufg-premise based on the boundary
#' values; e.g. if there exists an element that is obviously already given by
#' the others
#'
#' @param is_boundary (list) produced by check_boundary_input_cns
#' @param grid_numeric (vector) the numeric component of the entire grid
#' @param grid_nominal (vector) the nominal component of the entire grid
check_ufg_lies_in_cns <- function(is_boundary,
                              grid_numeric,
                              grid_nominal) {
  maybe_ufg <- list()

  for (i in is_boundary$lies_in) {
    maybe_ufg_inner <- (min(grid_numeric[i]) <
                          min(grid_numeric[is_boundary$lies_boundary]))
    maybe_ufg_inner <- (max(grid_numeric[i]) >
                          max(grid_numeric[is_boundary$lies_boundary]))  ||
      maybe_ufg_inner

    if (length(unique(grid_nominal[is_boundary$lies_boundary])) == 1) {
      if (!(unique(grid_nominal[i]) %in% unique(grid_nominal[is_boundary$lies_boundary])))
        maybe_ufg_inner <- maybe_ufg_inner || TRUE
    }

    maybe_ufg <- append(maybe_ufg, maybe_ufg_inner)
  }


  return(any(!unlist(maybe_ufg)))
}




#' Testing if and ufg-premise for mixed spatial-numeric-nominal data
#'
#' This is based on ufg depth introduced in:
#' Hannah Blocher, Georg Schollmeyer  (2024+): The union-free generic depth
#' using formal concept analysis.
#'
#' @description This function tests if the subset is a ufg-premise for mixed
#' spatial-numeric-nominal data
#'
#' @param observed (nx2) the observed values
#' @param observed_in_grid (vector) assignment to the grid
#' @param sf_spatial (sf-object) converted spatial_grid object to sf-object
#' @param grid_spatial (Lx2 matrix) the spatial component of the entire grid
#' (L is the number of grids)
#' @param grid_numeric (vector) the numeric component of the entire grid
#' @param grid_nominal (vector) the nominal component of the entire grid
test_ufg_main_cns <- function(observed,
                          observed_in_grid,
                          sf_spatial,
                          grid_spatial,
                          grid_numeric,
                          grid_nominal) {

  result <- list()

  # in this function we go through all sizes of the input set and for each size
  # we check if it is a ufg-premise

  # no set with size 5 can be an ufg premise
  if (length(observed_in_grid) > 4) {
    result <- list(is_ufg = FALSE,
                   hull_set = list(NA))
  }


  # for size 2 simple check if it is trivial
  if (length(observed_in_grid) == 2) {
    hull_set <- compute_hull_inner_cns(observed[, c(1,2)],
                                   observed_in_grid,
                                   sf_spatial,
                                   grid_spatial,
                                   grid_numeric,
                                   grid_nominal)
    if (sum(hull_set) == 2) {
      result <- list(is_ufg = FALSE,
                     hull_set = list(NA))
    } else {
      result <- list(is_ufg = TRUE,
                     hull_set = hull_set)
    }
  }

  # for size 3, we have to test directly if the union of the proper
  # subset-conlcusions equal the conclusion of the entire set at once.
  if (length(observed_in_grid) == 3) {
    result <- test_ufg_classic_cns(observed,
                               observed_in_grid,
                               sf_spatial,
                               grid_spatial,
                               grid_numeric,
                               grid_nominal)
  }

  # size 4: first check some basics (e.g. that the one point gives no further
  # variation on spatial and numeric and nominal componnent) and if this is not
  # fulfilled that check analogously to size 3.
  if (length(observed_in_grid) == 4) {
    is_boundary <- check_boundary_input_cns(observed,
                                        observed_in_grid)
    # grid_spatial)

    # case one: one point lies within the boundary. When this point does not
    # give an further information --> then this is not an ufg
    if (!is_boundary$is_boundary) {

      is_not_ufg <- check_ufg_lies_in_cns(is_boundary,
                                      grid_numeric,
                                      grid_nominal)
      if (is_not_ufg) {
        result <- list(is_ufg = FALSE,
                       hull_set = list(NA))
      }
      if (!is_not_ufg) {
        result <- test_ufg_classic_cns(observed,
                                   observed_in_grid,
                                   sf_spatial,
                                   grid_spatial,
                                   grid_numeric,
                                   grid_nominal)
      }
    }
    # case two: all points lie on boundary --> need to test old style
    if (is_boundary$is_boundary) {
      result <- test_ufg_classic_cns(observed,
                                 observed_in_grid,
                                 sf_spatial,
                                 grid_spatial,
                                 grid_numeric,
                                 grid_nominal)
    }

  }

  return(result)
}


#' Computes the ufg-depth of mixed cateorical_numeric_spatial data based on an
#' grid
#'
#' This is based on ufg depth introduced in:
#' Hannah Blocher, Georg Schollmeyer  (2024+): The union-free generic depth
#' using formal concept analysis.
#'
#' @description Function computes the ufg depht for mixed spatial,nominal and
#' numeric data based on an grid and an point pattern. For each grid element and
#' point pattern element the numeric and categorical value must be able to be
#' evaluated
#'
#' @param observed (nx2) the observed values
#' @param empirical_prob (vector) the counting number of duplications for each
#' observation
#' @param observed_in_grid (vector) assignment to the grid
#' @param grid_spatial (Lx2 matrix) the spatial component of the entire grid
#' (L is the number of grids)
#' @param grid_numeric (vector) the numeric component of the entire grid
#' @param grid_nominal (vector) the nominal component of the entire grid
#' @param lower_bound_ufg (integer) value strictly larger than 1
#' (lower ufg size bound)
#' @param upper_bound_ufg (integer) value strictly larger than lower_bound_ufg
#' (upper ufg size bound)
#'
#' @return returns a list containing the ufg depth, the number of ufg-premises
#' and a list of possible errors thrown.
#'
#' @export
compute_ufg_grid_cns <- function(observed,
                                 empirical_prob,
                                 observed_in_grid,
                                 grid_spatial,
                                 grid_numeric,
                                 grid_nominal,
                                 lower_bound_ufg  = 2,
                                 upper_bound_ufg = 4) {

  # data preparation
  sf_spatial <- sf::st_as_sf(data.frame(x = grid_spatial[, 1],
                                        y = grid_spatial[, 2]),
                             coords = c("x", "y"))
  # index_set <-  which(binary_grid_observed)

  count_ufg <- list()
  total_ufg <- list()
  error_list <- 0
  error_count <- 1

  for (i in seq(lower_bound_ufg, min(upper_bound_ufg, dim(observed)[1]))) {
    gosper_subset <- c(rep(0, dim(observed)[1] - i), rep(1, i))
    print(paste0("Now check subset size ", i, " at time stamp ", Sys.time()))

    count_ufg_inner <- rep(0, dim(sf_spatial)[1])
    total_ufg_inner <- 0

    j <- 1

    while (TRUE) {
      print(paste0("Now check subset size ", i, " and test ", j))
      index_test <- rep(FALSE, dim(observed)[1])
      index_test[which(gosper_subset != 0)] <- TRUE#
      observed_test <- observed[index_test, ]
      observed_test_in_grid <- observed_in_grid[index_test]
      ufg_test <- test_ufg_main_cns(observed_test,
                                observed_test_in_grid,
                                sf_spatial,
                                grid_spatial,
                                grid_numeric,
                                grid_nominal)

      if (ufg_test$is_ufg %in% c(TRUE, FALSE)) {
        if (ufg_test$is_ufg) {
          count_ufg_inner <- count_ufg_inner +
            prod(empirical_prob[index_test]) * ufg_test$hull_set
          total_ufg_inner <- total_ufg_inner + prod(empirical_prob[index_test])
        }
      }
      if (!(ufg_test$is_ufg %in% c(TRUE, FALSE))) {
        saveRDS(index_test, paste0("error_ufg_test_", error_count, ".RDS"))
        print("error")
        error_list <- error_list + 1
        error_count <- error_count + 1
      }




      # we are going through all subsets of size card_sub.
      # We use  Gosper's Hack therefore
      # see: http://programmingforinsomniacs.blogspot.com/2018/03/gospers-hack-explained.html
      if (all(gosper_subset[seq(1, i)] == 1)) {
        break # stop while loop
      }
      gosper_one <- which(gosper_subset == 1)
      gosper_zero <- which(gosper_subset != 1)
      max_zero <- max(gosper_zero[gosper_zero < max(gosper_one)])
      gosper_subset[c(max_zero, max_zero + 1)] <- c(1,0)

      gosper_one <- which(gosper_subset == 1)
      ones_above <- length(which(gosper_one > max_zero))
      h_seq <- seq(max_zero + 1, length(gosper_subset))
      gosper_subset[h_seq] <- c(rep(0, length(h_seq) - ones_above), rep(1, ones_above))

      j <- j + 1

    }
    count_ufg <- append(count_ufg, list(count_ufg_inner))
    total_ufg <- append(total_ufg, total_ufg_inner)
  }

  # computation of ufg depth

  result <- rep(0, dim(grid_spatial)[1])
  for (i in 1:length(total_ufg)) {
    if (total_ufg[[i]] != 0) {
      result <- result + count_ufg[[i]] / total_ufg[[i]]
    }
  }

  return(list(depth = result,
              count_ufg_cardinality = count_ufg,
              total_ufg_cardinality = total_ufg,
              error_list = error_list))
}

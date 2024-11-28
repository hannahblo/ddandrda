#' Computation convex operator for mixed spatial, numeric, categorical data
#'
#' This is based on ufg depth introduced in:
#' Hannah Blocher, Georg Schollmeyer  (2024+): The union-free generic depth
#' using formal concept analysis.
#'
#' @description This function computes the closure operator for mixed spatial,
#'  numeric, nominal observations
#'
#' @param observed (nx2) the observed values
#' @param observed_in_grid (vector) assignment to the grid
#' @param sf_spatial (sf-object) converted spatial_grid object to sf-object
#' @param grid_spatial (Lx2 matrix) the spatial component of the entire grid
#' (L is the number of grids)
#' @param grid_numeric (vector) the numeric component of the entire grid
#' @param grid_nominal (vector) the nominal component of the entire grid
#'
#' @return logical value
compute_hull_inner_cns <- function(observed,
                               observed_in_grid,
                               sf_spatial,
                               grid_spatial,
                               grid_numeric,
                               grid_nominal) {


  # spatial hull
  # https://gis.stackexchange.com/questions/360699/most-efficient-way-to-find-points-in-a-polygon-polygon-always-rectangular
  sf_set <- sf::st_multipoint(observed)
  hull_obs <- sf::st_convex_hull(sf_set)

  # simple bounding box --> less to test here
  bbox <- sf::st_bbox(sf_set)
  inbbox <- (grid_spatial[,1] >= bbox[1]) & (grid_spatial[,1] <= bbox[3]) &
    (grid_spatial[,2] >= bbox[2]) & (grid_spatial[,2] <= bbox[4])

  # computing hull
  index_ch_bbox <- sf::st_intersects(hull_obs, sf_spatial[inbbox, ])
  in_ch <- rep(FALSE, dim(grid_spatial)[1])

  in_ch[which(inbbox)[index_ch_bbox[[1]]]] <- TRUE
  # in_ch[bbox[index_ch_bbox]] <- TRUE
  # inbbox[inbbox][ index_ch[[1]]] <- TRUE

  in_ch[observed_in_grid] <- TRUE
  # note, when one point in grid observed, the entire grid is observed,
  # even when the center of the grid does not lie in the hull
  # in_ch[inbbox] <- TRUE
  in_ch[is.na(in_ch)] <- FALSE


  # numeric hull
  min_numeric_set <- min(grid_numeric[observed_in_grid])
  max_numeric_set <- max(grid_numeric[observed_in_grid])
  in_numeric <- (grid_numeric <= max_numeric_set) &
    (min_numeric_set <= grid_numeric)
  in_numeric[is.na(in_numeric)] <- FALSE

  # nominal hull
  in_nominal <- rep(TRUE, dim(grid_spatial)[1])
  if (length(unique(na.omit(grid_nominal[observed_in_grid]))) == 1) {
    in_nominal <- grid_nominal %in% unique(grid_nominal[observed_in_grid])
  }
  in_nominal[is.na(in_numeric)] <- FALSE

  # hull of merged context equals intersection
  return(in_ch & in_numeric & in_nominal)
}

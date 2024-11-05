#' This function computes the closure operator for mixed spatial, numeric, nominal
#' observations
compute_hull_inner <- function(observed,
                               observed_in_grid,
                               sf_spatial,
                               grid_spatial,
                               grid_numeric,
                               grid_nominal) {


  # spatial hull
  # https://gis.stackexchange.com/questions/360699/most-efficient-way-to-find-points-in-a-polygon-polygon-always-rectangular
  sf_set <- sf::st_multipoint(observed) # matrix(unlist(grid_spatial[set, ]), ncol = 2)
  hull_obs <- st_convex_hull(sf_set)

  # simple bounding box --> less to test here
  bbox <- st_bbox(sf_set)
  inbbox <- (grid_spatial[,1] >= bbox[1]) & (grid_spatial[,1] <= bbox[3]) &
    (grid_spatial[,2] >= bbox[2]) & (grid_spatial[,2] <= bbox[4])

  # computing hull
  index_ch_bbox <- st_intersects(hull_obs, sf_spatial[inbbox, ])
  in_ch <- rep(FALSE, dim(grid_spatial)[1])

  in_ch[which(inbbox)[index_ch_bbox[[1]]]] <- TRUE
  # in_ch[bbox[index_ch_bbox]] <- TRUE
  # inbbox[inbbox][ index_ch[[1]]] <- TRUE

  in_ch[observed_in_grid] <- TRUE # note, when one point in grid observed, the entire grid is observed, even when the center of the grid does not lie in the hull
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

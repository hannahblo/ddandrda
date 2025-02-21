grid_spatial_1 <- matrix(c(0.5, 0.5, 0.5, 1.5, 1.5, 1.5, 2.5, 2.5, 2.5,
                           0.5, 1.5, 2.5, 0.5, 1.5, 2.5, 0.5, 1.5, 2.5),
                         ncol = 2)
grid_numeric_1 <- c(0.4, 0.3, 0.5, 0.1, 0.1, 0.4, 0.22, 0.1, 0.5)
grid_nominal_1 <- c("a", "a", "b", "b", "b", "a", "a", "c", "b")
observed_1 <- matrix(c(0.7, 2.3, 1.99, 0.2,
                       0.7, 1.1, 1.5, 0.2),
                     ncol = 2)
empirical_prob_1 <- c(1, 2, 1, 1)
observed_in_grid_1 <- c(1, 8, 5, 1)

sf_spatial_1 <- sf::st_as_sf(data.frame(x = grid_spatial_1[, 1],
                                        y = grid_spatial_1[, 2]),
                             coords = c("x", "y"))
index_test_1 <- c(FALSE, TRUE, FALSE, TRUE)
index_test_2 <- c(FALSE, FALSE, TRUE, TRUE)


test_that("compute_hull_inner_cns works", {
  expect_equal(compute_hull_inner_cns(observed = observed_1[index_test_1, ],
                                      observed_in_grid =
                                        observed_in_grid_1[index_test_1],
                                      sf_spatial = sf_spatial_1,
                                      grid_spatial = grid_spatial_1,
                                      grid_numeric = grid_numeric_1,
                                      grid_nominal = grid_nominal_1),
               c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_equal(compute_hull_inner_cns(observed = observed_1[index_test_2, ],
                                      observed_in_grid =
                                        observed_in_grid_1[index_test_2],
                                      sf_spatial = sf_spatial_1,
                                      grid_spatial = grid_spatial_1,
                                      grid_numeric = grid_numeric_1,
                                      grid_nominal = grid_nominal_1),
               c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))
})

test_that("compute_transitive_hull works", {
  relation_mat_input <- matrix(0, nrow = 5, ncol = 5)
  relation_mat_input[1, 3] <- 1
  relation_mat_input[2, 1] <- 1
  relation_mat_input[4, 3] <- 1
  relation_mat_output <- relation_mat_input
  relation_mat_output[2, 3] <- 1
  expect_equal(compute_transitive_hull(relation_mat_input), relation_mat_output)
})

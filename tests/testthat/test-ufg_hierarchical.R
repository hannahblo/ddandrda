fc_1 <- t(matrix(c(1, 0, 1, 0, 0, 0,
                 1, 0, 1, 0, 0, 0,
                 1, 0, 0, 1, 0, 0,
                 1, 0, 0, 1, 0, 0,
                 0, 1, 0, 0, 1, 0,
                 0, 1, 0, 0, 1, 0,
                 0, 1, 0, 0, 0, 1,
                 0, 1, 0, 0, 0, 1),
               nrow = 6))


testthat::test_that("ufg_1_depth_hierarchical works", {
  expect_equal(ufg_1_depth_hierarchical(fc_1),
               list(depths = rep(0.25, 8), number_of_ufgs = 8))
})


testthat::test_that("ufg_2_depth_hierarchical works", {
  expect_equal(ufg_2_depth_hierarchical(fc_1),
               list(depths = rep(0, 8),
                    number_of_ufgs = 48))
})

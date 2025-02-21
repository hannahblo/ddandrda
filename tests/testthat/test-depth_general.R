fc_1 <- t(matrix(c(1, 0, 1, 0, 0, 0,
                   1, 0, 1, 0, 0, 0,
                   1, 0, 0, 1, 0, 0,
                   1, 0, 0, 1, 0, 0,
                   0, 1, 0, 0, 1, 0,
                   0, 1, 0, 0, 1, 0,
                   0, 1, 0, 0, 0, 1,
                   0, 1, 0, 0, 0, 1),
                 nrow = 6))

testthat::test_that("compute_quasiconcave_hull works", {
  expect_equal(compute_quasiconcave_hull(c(0, 0.3, 0.1, 0.2, 0.4, 0.5, 1, 1),
                                         fc_1),
               c(0.3, 0.3, 0.3, 0.3, 0.5, 0.5, 1.0, 1.0))
})

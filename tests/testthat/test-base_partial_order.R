# this is not a partial order (reflexivity, transitivity doesn't hold)
mat_1 <- matrix(0, nrow = 5, ncol = 5)
mat_1[1, 3] <- 1
mat_1[2, 1] <- 1
mat_1[4, 3] <- 1
# this is not a partial order (only if reflexiv ommitted)
mat_2 <- mat_1
mat_2[2, 3] <- 1
# this is a partial order
mat_3 <- matrix(0, nrow = 5, ncol = 5)
diag(mat_3) <- 1
# this is not a partial order (only if reflexiv ommitted)
mat_4 <- matrix(0, nrow = 4, ncol = 2)
# this is not a partial order (only if reflexiv ommitted)
mat_5 <- mat_3
mat_5[1, 2] <- -1
# this is not a partial order
mat_6 <- matrix(rep(1, 9), nrow = 3)
# this is not a partial order
mat_7 <- matrix(0, nrow = 4, ncol = 4)
diag(mat_7) <- 1
mat_7[1, 3] <- 1
mat_7[3, 1] <- 1
# this is a partial order
mat_8 <- mat_1
diag(mat_8) <- 1
# this is a partial order
mat_9 <- mat_2
diag(mat_9) <- 1
# this is not a partial order (only if reflexiv ommitted)
mat_10 <- matrix(0, nrow = 6, ncol = 6)


test_that("compute_transitive_hull works", {
  expect_error(test_if_porder(c(1, 0, 0)))
  expect_error(test_if_porder(mat_4))
  expect_error(test_if_porder(mat_5))
  expect_equal(compute_transitive_hull(mat_1), mat_2)
  expect_equal(compute_transitive_hull(mat_2), mat_2)
  expect_equal(compute_transitive_hull(mat_3), mat_3)
})

test_that("compute_relation_product works", {
  expect_equal(compute_relation_product(mat_1, mat_3), mat_1)
  expect_equal(compute_relation_product(mat_3, mat_3), mat_3)
})

test_that("test_if_porder works", {
  expect_error(test_if_porder(c(1, 0, 0)))
  expect_error(test_if_porder(mat_4))
  expect_error(test_if_porder(mat_5))
  expect_equal(test_if_porder(mat_1), FALSE)
  expect_equal(test_if_porder(mat_2), FALSE)
  expect_equal(test_if_porder(mat_6), FALSE)
  expect_equal(test_if_porder(mat_7), FALSE)
  expect_equal(test_if_porder(mat_3), TRUE)
  expect_equal(test_if_porder(mat_8), FALSE)
  expect_equal(test_if_porder(mat_9), TRUE)
  expect_equal(test_if_porder(mat_1, omit_reflexivity = TRUE), FALSE)
  expect_equal(test_if_porder(mat_2, omit_reflexivity = TRUE), TRUE)
  expect_equal(test_if_porder(mat_3, omit_reflexivity = TRUE), TRUE)
  expect_equal(test_if_porder(mat_6, omit_reflexivity = TRUE), FALSE)
  expect_equal(test_if_porder(mat_7, omit_reflexivity = TRUE), FALSE)
  expect_equal(test_if_porder(mat_8, omit_reflexivity = TRUE), FALSE)
  expect_equal(test_if_porder(mat_9, omit_reflexivity = TRUE), TRUE)
  expect_equal(test_if_porder(matrix(0, nrow = 6, ncol = 6),
    omit_reflexivity = TRUE), TRUE)
})
